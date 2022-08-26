use std::rc::Rc;

use contracts::debug_ensures;

use super::env::{EnvLen, UniqueEnv};
use super::eval::ElimCtx;
use super::{Elim, Expr, FunClosure, Head, Value};

pub struct QuoteCtx<'env> {
    local_values: EnvLen,
    item_values: &'env UniqueEnv<Rc<Value>>,
    meta_values: &'env UniqueEnv<Option<Rc<Value>>>,
}

impl<'env> QuoteCtx<'env> {
    pub fn new(
        local_values: EnvLen,
        item_values: &'env UniqueEnv<Rc<Value>>,
        meta_values: &'env UniqueEnv<Option<Rc<Value>>>,
    ) -> Self {
        Self {
            local_values,
            item_values,
            meta_values,
        }
    }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(self.item_values, self.meta_values) }

    #[debug_ensures(self.local_values == old(self.local_values))]
    pub fn quote_value(&mut self, value: &Rc<Value>) -> Expr {
        match self.elim_ctx().force_value(value).as_ref() {
            Value::Error => Expr::Error,
            Value::Type => Expr::Type,
            Value::BoolType => Expr::BoolType,
            Value::Bool(b) => Expr::Bool(*b),
            Value::Stuck(head, spine) => {
                let head_core = match head {
                    Head::Local(level) => match self.local_values.level_to_index(*level) {
                        None => unreachable!("Unbound local variable: {level:?}"),
                        Some(index) => Expr::Local(index),
                    },
                    Head::Meta(level) => Expr::Meta(*level),
                };

                spine.iter().fold(head_core, |head_core, elim| match elim {
                    Elim::FunCall(args) => {
                        let args = args.iter().map(|arg| self.quote_value(arg)).collect();
                        Expr::FunCall(Rc::new(head_core), args)
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
                        Expr::Match(Rc::new(head_core), Rc::from(core_arms))
                    }
                })
            }
            Value::FunType(names, closure) => {
                let (args, ret) = self.quote_closure(closure);
                Expr::FunType(names.clone(), args, Rc::new(ret))
            }
            Value::FunValue(names, closure) => {
                let (args, body) = self.quote_closure(closure);
                Expr::FunExpr(names.clone(), args, Rc::new(body))
            }
        }
    }

    #[debug_ensures(self.local_values == old(self.local_values))]
    fn quote_closure(&mut self, closure: &FunClosure) -> (Rc<[Expr]>, Expr) {
        let initial_len = self.local_values;

        let initial_closure = closure.clone();
        let mut closure = closure.clone();
        let mut exprs = Vec::with_capacity(closure.arity());
        let mut args = Vec::with_capacity(closure.arity());

        while let Some((value, cont)) = self.elim_ctx().split_fun_closure(closure.clone()) {
            let arg = Rc::new(Value::local(self.local_values.to_level()));
            closure = cont(arg.clone());
            exprs.push(self.quote_value(&value));
            args.push(arg);
            self.local_values.push();
        }

        let body = self.elim_ctx().call_closure(&initial_closure, args);
        let body = self.quote_value(&body);

        self.local_values.truncate(initial_len);

        (Rc::from(exprs), body)
    }
}
