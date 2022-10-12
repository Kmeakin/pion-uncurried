use super::semantics::binders::IsClosed;
use super::*;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SynthExpr(pub Expr, pub Arc<Value>);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CheckExpr(pub Expr);

/// Expressions
impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn synth_lit(&mut self, lit: &surface::Lit<Span>) -> (Lit, Arc<Value>) {
        match lit {
            surface::Lit::Bool(_, b) => (Lit::Bool(*b), Arc::new(Value::prim(Prim::BoolType))),
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    #[debug_ensures(ret.0.is_closed(self.local_env.len(), self.meta_env.len()))]
    pub fn synth_error_expr(&mut self) -> SynthExpr {
        SynthExpr(Expr::ERROR, Arc::new(Value::ERROR))
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    #[debug_ensures(ret.0.is_closed(self.local_env.len(), self.meta_env.len()))]
    pub fn synth_expr(&mut self, expr: &surface::Expr<Span>) -> SynthExpr {
        let file = self.file;
        match expr {
            surface::Expr::Error(_) => self.synth_error_expr(),
            surface::Expr::Lit(_, lit) => {
                let (lit, type_value) = self.synth_lit(lit);
                SynthExpr(Expr::Lit(lit), type_value)
            }
            surface::Expr::Name(span, name) => {
                let symbol = Symbol::intern(self.db, name);

                if let Some((index, type_value)) = self.local_env.lookup(symbol) {
                    return SynthExpr(Expr::Local(index), type_value);
                }

                if let Some(item) = crate::ir::lookup_item(self.db, file, symbol) {
                    match item {
                        ir::Item::Let(ir) => {
                            let type_value = synth_let_def_expr(self.db, ir);
                            return SynthExpr(Expr::Global(GlobalVar::Let(ir)), type_value);
                        }
                        ir::Item::Enum(ir) => {
                            let type_value = synth_enum_def_expr(self.db, ir);
                            return SynthExpr(Expr::Global(GlobalVar::Enum(ir)), type_value);
                        }
                        ir::Item::Variant(ir) => {
                            let type_value = synth_enum_variant_expr(self.db, ir);
                            return SynthExpr(Expr::Global(GlobalVar::Variant(ir)), type_value);
                        }
                    }
                }

                match name.as_str() {
                    "Type" => return SynthExpr(Expr::Prim(Prim::Type), Arc::new(Value::TYPE)),
                    "Bool" => return SynthExpr(Expr::Prim(Prim::BoolType), Arc::new(Value::TYPE)),
                    _ => {}
                }

                crate::error!(span.into_file_span(file), "Unbound variable: `{name}`")
                    .emit(self.db);
                self.synth_error_expr()
            }
            surface::Expr::Hole(span, hole) => {
                let expr_name = match hole {
                    surface::Hole::Underscore => self.name_source.fresh(),
                    surface::Hole::Name(name) => VarName::User(Symbol::new(self.db, name.clone())),
                };
                let type_name = self.name_source.fresh();
                let type_source = MetaSource::HoleType(*span);
                let expr_source = MetaSource::HoleExpr(*span);
                let type_value =
                    self.push_meta_value(expr_name, type_source, Arc::new(Value::TYPE));
                let expr = self.push_meta_expr(type_name, expr_source, type_value.clone());
                SynthExpr(expr, type_value)
            }
            surface::Expr::FunType(_, pats, ret) => {
                let initial_len = self.local_env.len();
                let fun_args = self.synth_telescope(pats);
                let CheckExpr(ret) = self.check_expr_is_type(ret);
                self.local_env.truncate(initial_len);

                SynthExpr(
                    Expr::FunType(fun_args, Arc::new(ret)),
                    Arc::new(Value::TYPE),
                )
            }
            surface::Expr::FunExpr(_, pats, body) => {
                let initial_len = self.local_env.len();
                let fun_args = self.synth_telescope(pats);
                let SynthExpr(body_core, body_type) = self.synth_expr(body);
                let ret_type = self.quote_ctx().quote_value(&body_type);
                self.local_env.truncate(initial_len);

                let fun_core = Expr::FunExpr(fun_args.clone(), Arc::new(body_core));
                let closure =
                    FunClosure::new(self.local_env.values.clone(), fun_args, Arc::new(ret_type));
                let fun_type = Value::FunType(closure);
                SynthExpr(fun_core, Arc::new(fun_type))
            }
            surface::Expr::FunCall(call_span, fun, args) => {
                let SynthExpr(fun_core, fun_type) = self.synth_expr(fun);
                let fun_type = self.elim_ctx().force_value(&fun_type);
                let closure = match fun_type.as_ref() {
                    Value::FunType(closure) => closure,
                    Value::Stuck(Head::Prim(Prim::Error), _) => return self.synth_error_expr(),
                    _ => {
                        self.report_non_fun_call(*call_span, fun.span(), &fun_type);
                        for arg in args {
                            let SynthExpr(..) = self.synth_expr(arg);
                        }
                        return self.synth_error_expr();
                    }
                };

                let expected_arity = closure.arity();
                let actual_arity = args.len();
                if actual_arity != expected_arity {
                    self.report_arity_mismatch(
                        *call_span,
                        fun.span(),
                        actual_arity,
                        expected_arity,
                        &fun_type,
                    );
                    return self.synth_error_expr();
                }

                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut arg_cores = Vec::with_capacity(args.len());
                let mut arg_values = Vec::with_capacity(args.len());
                let mut args = args.iter();

                while let Some((arg, (FunArg { r#type, .. }, cont))) =
                    Option::zip(args.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let CheckExpr(arg_core) = self.check_expr(arg, &r#type);
                    let arg_value = self.eval_ctx().eval_expr(&arg_core);
                    closure = cont(arg_value.clone());
                    arg_cores.push(arg_core);
                    arg_values.push(arg_value);
                }

                // Synth the rest of the arguments, in case too many arguments were passed to
                // the function. The result is ignored, but we still need to check them for any
                // errors
                for arg in args {
                    let SynthExpr(..) = self.synth_expr(arg);
                }

                let ret_type = self
                    .elim_ctx()
                    .apply_fun_closure(&initial_closure, arg_values);
                SynthExpr(
                    Expr::FunCall(Arc::new(fun_core), Arc::from(arg_cores)),
                    ret_type,
                )
            }
            surface::Expr::Let(_, pat, init, body) => {
                let initial_len = self.local_env.len();
                let SynthPat(pat_core, type_value) = self.synth_ann_pat(pat);
                let type_core = self.quote_ctx().quote_value(&type_value);

                let CheckExpr(init_core) = self.check_expr(init, &type_value);
                let init_value = self.eval_ctx().eval_expr(&init_core);

                self.subst_pat(&pat_core, type_value, Some(init_value));
                let SynthExpr(body_core, body_type) = self.synth_expr(body);
                self.local_env.truncate(initial_len);

                SynthExpr(
                    Expr::Let(
                        Arc::new(pat_core),
                        Arc::new(type_core),
                        Arc::new(init_core),
                        Arc::new(body_core),
                    ),
                    body_type,
                )
            }
            surface::Expr::Match(span, scrut, branches) => {
                let name = self.name_source.fresh();
                let source = MetaSource::MatchType(*span);
                let match_type = self.push_meta_value(name, source, Arc::new(Value::TYPE));

                let CheckExpr(match_expr) = self.check_match_expr(scrut, branches, &match_type);
                SynthExpr(match_expr, match_type)
            }
        }
    }

    fn synth_telescope(&mut self, pats: &[surface::AnnPat<Span>]) -> Telescope<Expr> {
        pats.iter().map(|arg| self.synth_fun_arg(arg)).collect()
    }

    fn synth_fun_arg(&mut self, arg: &surface::AnnPat<Span>) -> FunArg<Expr> {
        let SynthPat(pat_core, pat_type) = self.synth_ann_pat(arg);
        let type_core = self.quote_ctx().quote_value(&pat_type);
        self.subst_pat(&pat_core, pat_type, None);
        FunArg {
            pat: pat_core,
            r#type: type_core,
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn check_expr_is_type(&mut self, expr: &surface::Expr<Span>) -> CheckExpr {
        self.check_expr(expr, &Arc::new(Value::TYPE))
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    #[debug_ensures(ret.0.is_closed(self.local_env.len(), self.meta_env.len()))]
    pub fn check_expr(&mut self, expr: &surface::Expr<Span>, expected: &Arc<Value>) -> CheckExpr {
        match (expr, expected.as_ref()) {
            (surface::Expr::Error(_), _) => CheckExpr(Expr::ERROR),
            (surface::Expr::FunExpr(_, pats, body), Value::FunType(closure))
                if pats.len() == closure.arity() =>
            {
                let initial_len = self.local_env.len();
                let initial_closure = closure.clone();
                let mut closure = closure.clone();

                let mut args_values = Vec::with_capacity(pats.len());
                let mut fun_args = Vec::with_capacity(pats.len());

                let mut pats = pats.iter();
                while let Some((
                    pat,
                    (
                        FunArg {
                            r#type: expected, ..
                        },
                        cont,
                    ),
                )) = Option::zip(pats.next(), self.elim_ctx().split_fun_closure(closure))
                {
                    let type_core = self.quote_ctx().quote_value(&expected);

                    let arg_value = Arc::new(Value::local(self.local_env.len().to_level()));
                    let CheckPat(pat_core) = self.check_ann_pat(pat, &expected);
                    self.subst_pat(&pat_core, expected, None);

                    closure = cont(arg_value.clone());
                    args_values.push(arg_value);
                    fun_args.push(FunArg {
                        pat: pat_core,
                        r#type: type_core,
                    });
                }

                let expected_ret = self
                    .elim_ctx()
                    .apply_fun_closure(&initial_closure, args_values);
                let CheckExpr(ret_core) = self.check_expr(body, &expected_ret);
                self.local_env.truncate(initial_len);

                CheckExpr(Expr::FunExpr(
                    Telescope(Arc::from(fun_args)),
                    Arc::new(ret_core),
                ))
            }
            (surface::Expr::Let(_, pat, init, body), _) => {
                let initial_len = self.local_env.len();
                let SynthPat(pat_core, type_value) = self.synth_ann_pat(pat);
                let type_core = self.quote_ctx().quote_value(&type_value);

                let CheckExpr(init_core) = self.check_expr(init, &type_value);
                let init_value = self.eval_ctx().eval_expr(&init_core);

                self.subst_pat(&pat_core, type_value, Some(init_value));
                let CheckExpr(body_core) = self.check_expr(body, expected);
                self.local_env.truncate(initial_len);

                CheckExpr(Expr::Let(
                    Arc::new(pat_core),
                    Arc::new(type_core),
                    Arc::new(init_core),
                    Arc::new(body_core),
                ))
            }
            (surface::Expr::Match(_, scrut, branches), _) => {
                self.check_match_expr(scrut, branches, expected)
            }
            _ => {
                let SynthExpr(core, got) = self.synth_expr(expr);
                match self.unify_ctx().unify_values(&got, expected) {
                    Ok(()) => CheckExpr(core),
                    Err(error) => {
                        let span = expr.span();
                        self.report_type_mismatch(span, expected, &got, error);
                        CheckExpr(Expr::ERROR)
                    }
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    #[debug_ensures(ret.0.is_closed(self.local_env.len(), self.meta_env.len()))]
    fn check_match_expr(
        &mut self,
        scrut: &surface::Expr<Span>,
        branches: &[(surface::Pat<Span>, surface::Expr<Span>)],
        expected: &Arc<Value>,
    ) -> CheckExpr {
        // TODO: check for exhaustivity and report unreachable patterns

        // FIXME: update `expected` with defintions introduced by `check_pat`
        // without having to quote `expected` back to `Expr`

        let SynthExpr(scrut_core, scrut_type) = self.synth_expr(scrut);
        let scrut_value = self.eval_ctx().eval_expr(&scrut_core);

        let expected_core = self.quote_ctx().quote_value(expected);

        let branches = branches
            .iter()
            .map(|(pat, expr)| {
                let initial_len = self.local_env.len();
                let CheckPat(pat_core) = self.check_pat(pat, &scrut_type);
                self.subst_pat(&pat_core, scrut_type.clone(), Some(scrut_value.clone()));
                let expected = &self.eval_ctx().eval_expr(&expected_core);
                let CheckExpr(expr_core) = self.check_expr(expr, expected);
                self.local_env.truncate(initial_len);

                (pat_core, expr_core)
            })
            .collect();

        CheckExpr(Expr::Match(Arc::new(scrut_core), branches))
    }
}
