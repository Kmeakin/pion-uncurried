use super::*;
use crate::core::elab::ElabCtx;
use crate::core::env::EnvLen;
use crate::core::syntax::Pat;
use crate::core::unelab::UnelabCtx;

impl EnvLen {
    pub fn subst_pat(&mut self, pat: &Pat) { *self += pat.num_binders() }
}

impl QuoteCtx<'_> {
    pub fn subst_pat(&mut self, pat: &Pat) { self.local_len.subst_pat(pat) }
}

impl LocalEnv {
    /// Push an `Value::local` onto env for each binder in `pat`.
    #[debug_ensures(self.len() == old(self.len()) + pat.num_binders())]
    pub fn subst_arg_into_pat(&mut self, pat: &Pat) {
        let var = || Arc::new(Value::local(self.len().to_level()));
        match pat {
            Pat::Error | Pat::Lit(_) | Pat::Name(_) => self.push(var()),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.subst_arg_into_pat(pat)),
        }
    }

    #[debug_ensures(self.len() == old(self.len()) + pat.num_binders())]
    pub fn subst_value_into_pat(&mut self, pat: &Pat, value: Arc<Value>) {
        match pat {
            Pat::Error | Pat::Lit(_) | Pat::Name(_) => self.push(value),
            Pat::Variant(variant, pats) => match value.as_ref() {
                Value::Stuck(Head::Global(GlobalVar::Variant(v)), spine) if variant == v => {
                    if let [Elim::FunCall(args)] = spine.as_slice() {
                        let args = args.iter().cloned();
                        for (pat, arg) in pats.iter().zip(args) {
                            self.subst_value_into_pat(pat, arg);
                        }
                    } else {
                        unreachable!()
                    }
                }
                _ => unreachable!("Cannot subst {value:?} into {pat:?}"),
            },
        }
    }
}

impl EvalCtx<'_> {
    /// Push an `Value::local` onto env for each binder in `pat`.
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn subst_param_pat(&mut self, pat: &Pat) { self.local_env.subst_arg_into_pat(pat) }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn subst_def_pat(&mut self, pat: &Pat, value: Arc<Value>) {
        self.local_env.subst_value_into_pat(pat, value)
    }

    pub fn subst_def_telescope(
        &mut self,
        telescope: &Telescope<Expr>,
        values: impl IntoIterator<Item = Arc<Value>>,
    ) {
        for (arg, value) in telescope.iter().zip(values.into_iter()) {
            self.subst_def_pat(&arg.pat, value);
        }
    }
}

impl UnelabCtx<'_> {
    #[debug_ensures(self.local_names.len() == old(self.local_names.len()) + pat.num_binders())]
    pub fn subst_pat(&mut self, pat: &Pat) {
        match pat {
            Pat::Error => self.local_names.push(VarName::Underscore),
            Pat::Name(name) => self.local_names.push(*name),
            Pat::Lit(_) => self.local_names.push(self.name_source.fresh()),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.subst_pat(pat)),
        }
    }
}

impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn push_param(&mut self, pat: &Pat, r#type: Arc<Value>) {
        match pat {
            Pat::Error => {
                self.local_env.push_param(VarName::Underscore, r#type);
            }
            Pat::Name(name) => {
                self.local_env.push_param(*name, r#type);
            }
            Pat::Lit(_) => {
                let name = self.name_source.fresh();
                self.local_env.push_param(name, r#type);
            }
            Pat::Variant(variant, pats) => {
                let EnumVariant { args, .. } = elab_enum_variant(self.db, *variant);
                if args.len() != pats.len() {
                    unreachable!("pattern arity mismatch")
                }

                for (pat, _) in pats.iter().zip(args.iter()) {
                    // let type_value = self.eval_ctx().eval_expr(&arg.r#type);
                    let type_value = Arc::new(Value::ERROR);
                    self.push_param(pat, type_value);
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn push_def(&mut self, pat: &Pat, r#type: Arc<Value>, value: Arc<Value>) {
        match (pat, r#type.as_ref(), value.as_ref()) {
            (Pat::Error, ..) => {
                self.local_env.push_def(VarName::Underscore, r#type, value);
            }
            (Pat::Name(name), ..) => {
                self.local_env.push_def(*name, r#type, value);
            }
            (Pat::Lit(lit), ..) => {
                let value = Arc::new(Value::Lit(lit.clone()));
                let name = self.name_source.fresh();
                self.local_env.push_def(name, r#type, value);
            }
            (Pat::Variant(variant, pats), ..) => {
                let EnumVariant { args, .. } = elab_enum_variant(self.db, *variant);
                if args.len() != pats.len() {
                    unreachable!("pattern arity mismatch")
                }

                for (pat, _) in pats.iter().zip(args.iter()) {
                    // let type_value = self.eval_ctx().eval_expr(&arg.r#type);
                    let type_value = Arc::new(Value::ERROR);
                    self.push_def(pat, type_value, Arc::new(Value::ERROR));
                }
            }
        }
    }
}
