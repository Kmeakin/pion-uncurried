use super::*;
use crate::core::elab::ElabCtx;
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
            Pat::Error | Pat::Lit(_) | Pat::Name(_) => self.push(var()),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.push_pat_params(pat)),
        }
    }

    #[debug_ensures(self.len() == old(self.len()) + pat.num_binders())]
    pub fn push_pat_defs(&mut self, pat: &Pat, value: Arc<Value>) {
        match pat {
            Pat::Error | Pat::Lit(_) | Pat::Name(_) => self.push(value),
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
            Pat::Error => self.local_names.push(VarName::Underscore),
            Pat::Name(name) => self.local_names.push(*name),
            Pat::Lit(_) => self.local_names.push(self.name_source.fresh()),
            Pat::Variant(_, pats) => pats.iter().for_each(|pat| self.push_pat_params(pat)),
        }
    }
}

impl ElabCtx<'_> {
    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn push_pat_params(&mut self, pat: &Pat, r#type: Arc<Value>) {
        let r#type = self.elim_ctx().force_value(&r#type);

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
                assert_eq!(args.len(), pats.len());
                for (pat, arg) in pats.iter().zip(args.iter()) {
                    let type_value = self.eval_ctx().eval_expr(&arg.r#type);
                    self.push_pat_params(pat, type_value);
                }
            }
        }
    }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()) + pat.num_binders())]
    pub fn push_pat_defs(&mut self, pat: &Pat, r#type: Arc<Value>, value: Arc<Value>) {
        let db = self.db;
        let r#type = self.elim_ctx().force_value(&r#type);
        let value = self.elim_ctx().force_value(&value);

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
                let (e, _) = r#type.as_enum().unwrap();
                let (v, value_args) = match value.as_variant() {
                    Some(x) => x,
                    None => return self.push_pat_params(pat, r#type),
                };
                assert_eq!(e, variant.parent(db));
                assert_eq!(v, *variant);
                assert_eq!(value_args.len(), pats.len());

                let EnumVariant {
                    args: variant_args, ..
                } = elab_enum_variant(self.db, *variant);
                assert_eq!(value_args.len(), variant_args.len());

                for ((pat, value_arg), variant_arg) in
                    pats.iter().zip(value_args.iter()).zip(variant_args.iter())
                {
                    let type_value = self.eval_ctx().eval_expr(&variant_arg.r#type);
                    self.push_pat_defs(pat, type_value, value_arg.clone());
                }
            }
        }
    }
}
