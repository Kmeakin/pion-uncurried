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
                let pats = pats.iter().map(|pat| self.synth_pat(pat).0).collect();
                SynthPat(Pat::Variant(variant, pats), Arc::new(Value::ERROR))
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
