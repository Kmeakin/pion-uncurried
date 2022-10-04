use std::sync::Arc;

use contracts::debug_ensures;

use super::env::{EnvLen, UniqueEnv};
use super::eval::ElimCtx;
use super::syntax::*;

pub struct QuoteCtx<'env> {
    local_env: EnvLen,
    meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
}

impl<'env> QuoteCtx<'env> {
    pub fn new(local_env: EnvLen, meta_env: &'env UniqueEnv<Option<Arc<Value>>>) -> Self {
        Self {
            local_env,
            meta_env,
        }
    }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(self.meta_env) }

    #[debug_ensures(self.local_env == old(self.local_env))]
    pub fn quote_value(&mut self, value: &Arc<Value>) -> Expr {
        match value.as_ref() {
            Value::Error => Expr::Error,
            Value::Type => Expr::Type,
            Value::BoolType => Expr::BoolType,
            Value::Lit(lit) => Expr::Lit(lit.clone()),
            Value::Stuck(head, spine) => {
                let head_core = match head {
                    Head::Local(level) => match self.local_env.level_to_index(*level) {
                        Some(index) => Expr::Local(index),
                        None => unreachable!("Unbound local variable: {level:?}"),
                    },
                    Head::Meta(level) => Expr::Meta(*level),
                };
                spine.iter().fold(head_core, |head_core, elim| match elim {
                    Elim::FunCall(args) => {
                        let args = args.iter().map(|arg| self.quote_value(arg)).collect();
                        Expr::FunCall(Arc::new(head_core), args)
                    }
                    Elim::Match(arms) => {
                        let mut arms = arms.clone();
                        let mut core_arms = Vec::with_capacity(arms.arms.len());
                        while let Some((pat, value, next_arms)) =
                            self.elim_ctx().split_arms(arms.clone())
                        {
                            core_arms.push((pat, self.quote_value(&value)));
                            arms = next_arms;
                        }
                        Expr::Match(Arc::new(head_core), Arc::from(core_arms))
                    }
                })
            }
            Value::FunType(closure) => {
                let (args, body) = self.quote_closure(closure);
                Expr::FunType(args, Arc::new(body))
            }
            Value::FunValue(closure) => {
                let (args, body) = self.quote_closure(closure);
                Expr::FunExpr(args, Arc::new(body))
            }
        }
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn quote_closure(&mut self, closure: &FunClosure) -> (Arc<[FunArg<Expr>]>, Expr) {
        let initial_len = self.local_env;

        let initial_closure = closure.clone();
        let mut closure = closure.clone();
        let mut fun_args = Vec::with_capacity(closure.arity());
        let mut arg_values = Vec::with_capacity(closure.arity());

        while let Some((FunArg { pat, ty }, cont)) =
            self.elim_ctx().split_fun_closure(closure.clone())
        {
            let arg_value = Arc::new(Value::local(self.local_env.to_level()));
            closure = cont(arg_value.clone());
            let type_core = self.quote_value(&ty);
            fun_args.push(FunArg { pat, ty: type_core });
            arg_values.push(arg_value);
            self.local_env.push();
        }

        let body = self.elim_ctx().apply_closure(&initial_closure, arg_values);
        let body = self.quote_value(&body);

        self.local_env.truncate(initial_len);

        (Arc::from(fun_args), body)
    }
}
