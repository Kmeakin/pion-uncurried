use super::*;
use crate::ir::lookup_item;

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
            surface::Pat::Variant(_, name, pats) => {
                let name = Symbol::new(self.db, name.to_owned());
                let variant = match lookup_item(self.db, self.file, name) {
                    Some(ir::Item::Variant(ir)) => ir,
                    _ => todo!(),
                };

                let EnumDefSig {
                    args: parent_args, ..
                } = enum_def_sig(db, variant.parent(db));

                let EnumVariant { args, .. } = elab_enum_variant(self.db, variant);

                if pats.len() != args.len() {
                    todo!("pattern arity mismatch")
                }

                let initial_len = self.local_env.len();

                let pats = pats
                    .iter()
                    .zip(args.iter())
                    .map(|(pat, FunArg { ty, .. })| {
                        let type_value = Arc::new(Value::Error);
                        // let type_value = self.eval_ctx().eval_expr(ty);
                        let CheckPat(pat_core) = self.check_pat(pat, &type_value);
                        self.subst_pat(&pat_core, type_value, None);
                        pat_core
                    })
                    .collect();

                // TODO: what should it be?
                let type_value = Arc::new(Value::Error);

                self.local_env.truncate(initial_len);
                SynthPat(Pat::Variant(variant, pats), type_value)
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
            (Pat::Variant(variant, pats), ..) => {
                let EnumVariant { args, .. } = elab_enum_variant(self.db, *variant);
                if args.len() != pats.len() {
                    todo!("pattern arity mismatch")
                }

                for (pat, arg) in pats.iter().zip(args.iter()) {
                    // let type_value = self.eval_ctx().eval_expr(&arg.ty);
                    let type_value = Arc::new(Value::Error);
                    self.subst_pat(pat, type_value, None);
                }
            }
            _ => unreachable!("Cannot subst {value:#?} with type {ty:#?} into {pat:#?}"),
        }
    }
}
