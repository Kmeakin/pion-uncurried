use super::*;
use crate::core::elab::{synth_enum_variant_expr, ElabCtx};
use crate::core::env::EnvLen;
use crate::core::syntax::Pat;
use crate::core::unelab::UnelabCtx;

impl EnvLen {
    pub fn push_pat_params(&mut self, pat: &Pat) { *self += pat.num_binders() }
}

impl QuoteCtx<'_> {
    pub fn push_pat_params(&mut self, pat: &Pat) { self.local_env.push_pat_params(pat) }
}

impl LocalEnv {
    /// Push an `Value::local` onto env for each binder in `pat`.
    #[debug_ensures(self.len() == old(self.len()) + pat.num_binders())]
    pub fn push_pat_params(&mut self, pat: &Pat) {
        let var = || Arc::new(Value::local(self.len().to_level()));
        match pat {
            Pat::Lit(_) => {}
            Pat::Error | Pat::Name(_) => self.push(var()),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.push_pat_params(pat)),
        }
    }

    #[debug_ensures(self.len() == old(self.len()) + pat.num_binders())]
    pub fn push_pat_defs(&mut self, pat: &Pat, value: Arc<Value>) {
        match pat {
            Pat::Lit(_) => {}
            Pat::Error | Pat::Name(_) => self.push(value),
            Pat::Variant(variant, pats) => {
                let (v, value_args) = value.as_variant().unwrap();
                assert_eq!(v, *variant);
                assert_eq!(value_args.len(), pats.len());
                for (pat, arg) in pats.iter().zip(value_args) {
                    self.push_pat_defs(pat, arg.clone());
                }
            }
        }
    }
}

impl EvalCtx<'_> {
    /// Push an `Value::local` onto env for each binder in `pat`.
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn push_pat_params(&mut self, pat: &Pat) { self.local_env.push_pat_params(pat) }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn push_pat_defs(&mut self, pat: &Pat, value: Arc<Value>) {
        self.local_env.push_pat_defs(pat, value)
    }

    #[track_caller]
    #[debug_requires(telescope.is_closed(self.local_env.len(),self.meta_env.len()))]
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + telescope.num_binders())]
    pub fn push_telescope_defs(
        &mut self,
        telescope: &Telescope<Expr>,
        values: impl IntoIterator<Item = Arc<Value>>,
    ) {
        for (arg, value) in telescope.iter().zip(values.into_iter()) {
            self.push_pat_defs(&arg.pat, value);
        }
    }
}

impl UnelabCtx<'_> {
    #[debug_ensures(self.local_names.len() == old(self.local_names.len()) + pat.num_binders())]
    pub fn push_pat_params(&mut self, pat: &Pat) {
        match pat {
            Pat::Lit(_) => {}
            Pat::Error => self.local_names.push(VarName::UNDERSCORE),
            Pat::Name(name) => self.local_names.push(*name),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.push_pat_params(pat)),
        }
    }
}

impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn push_pat_params(&mut self, pat: &Pat, r#type: Arc<Value>) {
        let db = self.db;
        let r#type = self.elim_ctx().force_value(&r#type);

        match pat {
            Pat::Error => {
                self.local_env.push_param(VarName::UNDERSCORE, r#type);
            }
            Pat::Name(name) => {
                self.local_env.push_param(*name, r#type);
            }
            Pat::Lit(_) => {}
            Pat::Variant(variant, pats) => {
                let (e, enum_args) = r#type.as_enum().unwrap();
                assert_eq!(e, variant.parent(db));

                let mut closure = match synth_enum_variant_expr(db, *variant).as_ref() {
                    Value::FunType(closure) => closure.clone(),
                    r#type if r#type.as_enum().is_some() => return,
                    _ => unreachable!(),
                };

                debug_assert!(enum_args.len() <= closure.args.len());
                for arg in enum_args {
                    match self.elim_ctx().split_fun_closure(closure) {
                        None => unreachable!(),
                        Some((_, cont)) => closure = cont(arg.clone()),
                    }
                }

                assert_eq!(pats.len(), closure.args.len());
                for pat in pats.iter() {
                    match self.elim_ctx().split_fun_closure(closure) {
                        None => unreachable!(),
                        Some((arg, cont)) => {
                            let arg_value = Arc::new(Value::local(self.local_env.len().to_level()));
                            let type_value = arg.r#type;
                            closure = cont(arg_value.clone());
                            self.push_pat_params(pat, type_value);
                        }
                    }
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn push_pat_defs(&mut self, pat: &Pat, r#type: Arc<Value>, value: Arc<Value>) {
        let r#type = self.elim_ctx().force_value(&r#type);
        let value = self.elim_ctx().force_value(&value);

        match pat {
            Pat::Error => {
                self.local_env.push_def(VarName::UNDERSCORE, r#type, value);
            }
            Pat::Name(name) => {
                self.local_env.push_def(*name, r#type, value);
            }
            Pat::Lit(_) => {}
            Pat::Variant(..) => self.push_pat_params(pat, r#type),
        }
    }
}
