use super::*;
use crate::ir::lookup_item;

pub struct SynthPat(pub Pat, pub Arc<Value>);

pub struct CheckPat(pub Pat);

impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn synth_error_pat(&mut self) -> SynthPat { SynthPat(Pat::Error, Arc::new(Value::ERROR)) }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn synth_pat(&mut self, pat: &surface::Pat<Span>) -> SynthPat {
        let db = self.db;
        let mut meta = || {
            let name = self.name_source.fresh();
            let span = pat.span();
            let source = MetaSource::PatType(span);
            self.push_meta_value(name, source, Arc::new(Value::TYPE))
        };

        match pat {
            surface::Pat::Error(_) => SynthPat(Pat::Name(VarName::Underscore), meta()),
            surface::Pat::Wildcard(_) => SynthPat(Pat::Name(VarName::Underscore), meta()),
            surface::Pat::Name(_, name) => {
                let name = VarName::User(Symbol::intern(db, name));
                SynthPat(Pat::Name(name), meta())
            }
            surface::Pat::Lit(_, lit) => {
                let (lit, r#type) = self.synth_lit(lit);
                SynthPat(Pat::Lit(lit), r#type)
            }
            surface::Pat::Variant(_, name, pats) => {
                let name = Symbol::new(self.db, name.to_owned());
                let variant = match lookup_item(self.db, self.file, name) {
                    Some(ir::Item::Variant(ir)) => ir,
                    _ => todo!("No such variant"),
                };

                let EnumDefSig {
                    args: parent_args, ..
                } = enum_def_sig(self.db, variant.parent(self.db));
                let EnumVariant {
                    args: variant_arg_exprs,
                    arg_values: variant_arg_values,
                    ret_type: variant_ret_type,
                    ..
                } = elab_enum_variant(db, variant);
                if pats.len() != variant_arg_values.len() {
                    todo!("arity mismatch")
                }

                let pats = pats
                    .iter()
                    .zip(variant_arg_values.iter())
                    .map(|(pat, arg)| {
                        let expected = arg.r#type.clone();
                        let CheckPat(pat_core) = self.check_pat(pat, &expected);
                        pat_core
                    })
                    .collect();

                let ret_type = match (parent_args.len(), variant_arg_values.len()) {
                    (0, 0) => variant_ret_type.1,
                    // (0, _) => todo!(),
                    // (_, 0) => todo!(),
                    (..) => {
                        let closure = FunClosure::new(
                            self.local_env.values.clone(),
                            parent_args
                                .iter()
                                .chain(variant_arg_exprs.iter())
                                .cloned()
                                .collect(),
                            Arc::new(variant_ret_type.0),
                        );
                        let args = parent_args
                            .iter()
                            .map(|_| {
                                let name = self.name_source.fresh();
                                let source = MetaSource::Error;
                                let r#type = Arc::new(Value::TYPE);
                                self.push_meta_value(name, source, r#type)
                            })
                            .chain(variant_arg_values.iter().map(|arg| arg.r#type.clone()))
                            .collect();
                        self.elim_ctx().apply_fun_closure(&closure, args)
                    }
                };
                SynthPat(Pat::Variant(variant, pats), ret_type)
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
        let surface::AnnPat { pat, type_: r#type } = pat;
        match r#type {
            None => self.synth_pat(pat),
            Some(r#type) => {
                let CheckExpr(type_core) = self.check_expr_is_type(r#type);
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
        let surface::AnnPat { pat, type_: r#type } = pat;
        match r#type {
            None => self.check_pat(pat, expected),
            Some(r#type) => {
                let CheckExpr(type_core) = self.check_expr_is_type(r#type);
                let type_value = self.eval_ctx().eval_expr(&type_core);

                if let Err(error) = self.unify_ctx().unify_values(&type_value, expected) {
                    let span = pat.span();
                    self.report_type_mismatch(span, expected, &type_value, error);
                }

                self.check_pat(pat, &type_value)
            }
        }
    }
}
