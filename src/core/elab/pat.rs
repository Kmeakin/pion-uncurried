use super::*;

pub struct SynthPat(pub Pat, pub Arc<Value>);

pub struct CheckPat(pub Pat);

impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn synth_error_pat(&mut self) -> SynthPat {
        // let name = self.name_source.fresh();
        // let source = MetaSource::Error;
        // let ty = self.push_meta_value(name, source, Arc::new(Value::Type));
        SynthPat(Pat::Error, Arc::new(Value::Error))
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn synth_pat(&mut self, pat: &surface::Pat<Span>) -> SynthPat {
        let file = self.file;
        let db = self.db;
        let mut meta = || {
            let name = self.name_source.fresh();
            let span = pat.span();
            let source = MetaSource::PatType(span);
            self.push_meta_value(name, source, Arc::new(Value::Type))
        };

        match pat {
            surface::Pat::Error(_) => SynthPat(Pat::Name(VarName::Underscore), meta()),
            surface::Pat::Wildcard(_) => SynthPat(Pat::Name(VarName::Underscore), meta()),
            surface::Pat::Name(_, name) => {
                let name = VarName::User(Symbol::intern(db, name));
                SynthPat(Pat::Name(name), meta())
            }
            surface::Pat::Lit(_, lit) => {
                let (lit, ty) = self.synth_lit(lit);
                SynthPat(Pat::Lit(lit), ty)
            }
            surface::Pat::Variant(span, name, pats) => {
                let symbol = Symbol::intern(self.db, name);
                match crate::ir::lookup_item(self.db, self.file, symbol) {
                    Some(ir::Item::Variant(variant)) => {
                        let EnumVariant { args, ret_type, .. } =
                            elab_enum_variant(self.db, variant);
                        let pats = pats
                            .iter()
                            .zip(args.iter().map(|FunArg { pat, ty }| FunArg {
                                pat: pat.clone(),
                                ty: ty.1.clone(),
                            }))
                            .map(|(pat, arg)| {
                                let CheckPat(pat) = self.check_pat(pat, &arg.ty);
                                pat
                            })
                            .collect();
                        SynthPat(Pat::Variant(variant, pats), ret_type.1)
                    }
                    _ => {
                        crate::error!(span.into_file_span(file), "Unbound variable: `{name}`")
                            .emit(db);
                        self.synth_error_pat()
                    }
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn check_pat(&mut self, pat: &surface::Pat<Span>, expected: &Arc<Value>) -> CheckPat {
        let expected = self.elim_ctx().force_value(expected);
        match pat {
            surface::Pat::Error(_) => CheckPat(Pat::Name(VarName::Underscore)),
            surface::Pat::Wildcard(_) => CheckPat(Pat::Name(VarName::Underscore)),
            surface::Pat::Name(_, name) => {
                let name = VarName::User(Symbol::intern(self.db, name));
                CheckPat(Pat::Name(name))
            }
            _ => {
                let SynthPat(core, got) = self.synth_pat(pat);
                match self.unify_ctx().unify_values(&got, &expected) {
                    Ok(_) => CheckPat(core),
                    Err(error) => {
                        let span = pat.span();
                        self.report_type_mismatch(span, &expected, &got, error);
                        CheckPat(Pat::Error)
                    }
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn synth_ann_pat(&mut self, pat: &surface::AnnPat<Span>) -> SynthPat {
        let surface::AnnPat { pat, ty } = pat;
        match ty {
            None => self.synth_pat(pat),
            Some(ty) => {
                let CheckExpr(type_core) = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);
                let CheckPat(pat_core) = self.check_pat(pat, &type_value);
                SynthPat(pat_core, type_value)
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn check_ann_pat(
        &mut self,
        pat: &surface::AnnPat<Span>,
        expected: &Arc<Value>,
    ) -> CheckPat {
        let surface::AnnPat { pat, ty } = pat;
        match ty {
            None => self.check_pat(pat, expected),
            Some(ty) => {
                let CheckExpr(type_core) = self.check_expr_is_type(ty);
                let type_value = self.eval_ctx().eval_expr(&type_core);

                if let Err(error) = self.unify_ctx().unify_values(&type_value, expected) {
                    let span = pat.span();
                    self.report_type_mismatch(span, expected, &type_value, error);
                }

                self.check_pat(pat, &type_value)
            }
        }
    }

    pub fn subst_pat(&mut self, pat: &Pat, ty: Arc<Value>, value: Option<Arc<Value>>) {
        match (pat, ty.as_ref(), value.as_ref()) {
            (Pat::Error, ..) => {
                self.local_env.push(VarName::Underscore, ty, value);
            }
            (Pat::Name(name), ..) => {
                self.local_env.push(*name, ty, value);
            }
            (Pat::Lit(lit), ..) => {
                let pat_value = Arc::new(Value::Lit(lit.clone()));
                let name = self.name_source.fresh();
                self.local_env.push(name, ty, Some(pat_value));
            }
            _ => unreachable!("Cannot subst {value:#?} with type {ty:#?} into {pat:#?}"),
        }
    }
}
