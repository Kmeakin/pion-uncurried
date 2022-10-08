use super::*;

/// Items
impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn elab_module(&mut self, module: ir::Module) -> Module {
        let items = module.items(self.db);
        let items = items
            .iter()
            .filter_map(|item| match item {
                ir::Item::Let(let_def) => {
                    let let_def = elab_let_def(self.db, *let_def);
                    Some(Item::Let(let_def))
                }
                ir::Item::Enum(enum_def) => {
                    let enum_def = elab_enum_def(self.db, *enum_def);
                    Some(Item::Enum(enum_def))
                }
                ir::Item::Variant(_) => None,
            })
            .collect();
        Module { items }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn elab_let_def(&mut self, let_def: ir::LetDef) -> LetDef {
        let name = let_def.name(self.db);
        let surface::LetDef { ty, body, .. } = let_def.surface(self.db);

        let (body_expr, type_value) = match ty {
            Some(ty) => {
                let CheckExpr(type_expr) = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_expr);
                let CheckExpr(body_expr) = self.check_expr(body, &type_value);
                (body_expr, type_value)
            }
            None => {
                let SynthExpr(body_expr, type_value) = self.synth_expr(body);
                (body_expr, type_value)
            }
        };
        let type_expr = self.quote_ctx().quote_value(&type_value);

        let expr_opts = EvalOpts {
            beta_reduce: true, // TODO: disable beta reduction
            ..EvalOpts::ZONK
        };
        let value_opts = EvalOpts {
            error_on_unsolved_meta: true,
            ..EvalOpts::EVAL_CBV
        };

        let body_value = self.eval_ctx().with_opts(value_opts).eval_expr(&body_expr);
        let (body_expr, _) = self
            .eval_ctx()
            .with_opts(expr_opts)
            .normalize_expr(&body_expr);

        let type_value = self.eval_ctx().with_opts(value_opts).eval_expr(&type_expr);
        let (type_expr, _) = self
            .eval_ctx()
            .with_opts(expr_opts)
            .normalize_expr(&type_expr);

        debug_assert!(
            body_expr.is_closed(EnvLen(0), EnvLen(0)),
            "free variables in `body_expr`: {body_expr:#?}"
        );
        debug_assert!(
            body_expr.is_closed(EnvLen(0), EnvLen(0)),
            "free variables in `type_expr`: {type_expr:#?}"
        );

        LetDef {
            name,
            body: (body_expr, body_value),
            ty: (type_expr, type_value),
        }
    }

    // NOTE: does not truncate env
    pub fn synth_enum_def(&mut self, enum_def: ir::EnumDef) -> EnumDefSig {
        let surface::EnumDef { args, ret_type, .. } = enum_def.surface(self.db);

        let mut self_args = Vec::with_capacity(args.len());
        let args: Arc<[_]> = args
            .iter()
            .map(|pat| {
                let SynthPat(pat_core, type_value) = self.synth_ann_pat(pat);
                let type_core = self.quote_ctx().quote_value(&type_value);
                self_args.push(Arc::new(Value::local(self.local_env.len().to_level())));
                self.subst_pat(&pat_core, type_value, None);
                FunArg {
                    pat: pat_core,
                    ty: (type_core),
                }
            })
            .collect();

        let self_type = Value::Stuck(
            Head::EnumDef(enum_def),
            if args.len() == 0 {
                vec![]
            } else {
                vec![Elim::FunCall(self_args)]
            },
        );
        let self_type = Arc::new(self_type);

        let ret_type = match ret_type {
            Some(ret_type) => {
                let SynthExpr(ret_core, _) = self.synth_expr(&ret_type);
                let ret_value = self.eval_ctx().eval_expr(&ret_core);
                let expected = Arc::new(Value::Type);
                match self.unify_ctx().unify_values(&ret_value, &expected) {
                    Ok(_) => {
                        let type_core = self.quote_ctx().quote_value(&ret_value);
                        type_core
                    }
                    Err(error) => {
                        self.report_type_mismatch(ret_type.span(), &expected, &ret_value, error);
                        Expr::Error
                    }
                }
            }
            None => Expr::Type,
        };

        EnumDefSig {
            args,
            ret_type,
            self_type,
        }
    }

    pub fn elab_enum_def(&mut self, enum_def: ir::EnumDef) -> EnumDef {
        let initial_len = self.local_env.len();
        let name = enum_def.name(self.db);
        let surface::EnumDef { variants, .. } = enum_def.surface(self.db);

        let sig = &self.synth_enum_def(enum_def);

        let variants = variants
            .iter()
            .map(|variant| self.elab_enum_variant(variant, sig))
            .collect();
        self.local_env.truncate(initial_len);

        EnumDef {
            name,
            sig: sig.clone(),
            variants,
        }
    }

    pub fn elab_enum_variant(
        &mut self,
        enum_variant: &surface::EnumVariant<TextRange>,
        sig: &EnumDefSig,
    ) -> EnumVariant {
        let surface::EnumVariant {
            name,
            args,
            ret_type: ty,
        } = enum_variant;

        let initial_len = self.local_env.len();
        let name = Symbol::new(self.db, name.to_owned());
        let args: Arc<[_]> = args
            .iter()
            .map(|pat| {
                let SynthPat(pat_core, type_value) = self.synth_ann_pat(pat);
                let type_core = self.quote_ctx().quote_value(&type_value);
                self.subst_pat(&pat_core, type_value.clone(), None);
                FunArg {
                    pat: pat_core,
                    ty: (type_core, type_value),
                }
            })
            .collect();

        let ret_type = match ty {
            Some(ty) => {
                let CheckExpr(ret_core) = self.check_expr_is_type(ty);
                let ret_value = self.eval_ctx().eval_expr(&ret_core);
                (self.quote_ctx().quote_value(&ret_value), ret_value)
            }
            None => (
                self.quote_ctx().quote_value(&sig.self_type),
                sig.self_type.clone(),
            ),
        };
        self.local_env.truncate(initial_len);

        EnumVariant {
            name,
            args,
            ret_type,
        }
    }
}
