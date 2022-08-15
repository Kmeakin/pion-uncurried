use std::rc::Rc;

use contracts::debug_ensures;

use super::env::EnvLen;
use super::eval::ElimCtx;
use super::{Elim, Expr, FunClosure, Head, Value};

pub struct QuoteCtx {
    local_values: EnvLen,
}

impl QuoteCtx {
    pub fn new(local_values: EnvLen) -> Self { Self { local_values } }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new() }

    #[debug_ensures(self.local_values == old(self.local_values))]
    pub fn quote_value(&mut self, value: &Rc<Value>) -> Expr {
        match value.as_ref() {
            Value::Error => Expr::Error,
            Value::Type => Expr::Type,
            Value::BoolType => Expr::BoolType,
            Value::Bool(b) => Expr::Bool(*b),
            Value::Stuck(head, spine) => {
                let head_core = match head {
                    Head::Local(level) => match self.local_values.level_to_index(*level) {
                        None => unreachable!(),
                        Some(index) => Expr::Local(index),
                    },
                };

                spine.iter().fold(head_core, |head_core, elim| match elim {
                    Elim::FunCall(args) => {
                        let args = args.iter().map(|arg| self.quote_value(arg)).collect();
                        Expr::FunCall(Rc::new(head_core), args)
                    }
                })
            }
            Value::FunType(closure) => {
                let (args, ret) = self.quote_closure(closure);
                Expr::FunType(args, Rc::new(ret))
            }
            Value::FunValue(closure) => {
                let (args, body) = self.quote_closure(closure);
                Expr::FunExpr(args, Rc::new(body))
            }
        }
    }

    #[debug_ensures(self.local_values == old(self.local_values))]
    #[cfg(FALSE)]
    fn quote_telescope(&mut self, telescope: &Telescope) -> Rc<[Expr]> {
        let initial_len = self.local_values;
        let exprs = telescope
            .types
            .iter()
            .map(|value| {
                let expr = self.quote_value(value);
                self.local_values.push();
                expr
            })
            .collect();
        self.local_values.truncate(initial_len);
        exprs
    }

    #[debug_ensures(self.local_values == old(self.local_values))]
    #[cfg(FALSE)]
    fn quote_closure(&mut self, closure: &FunClosure) -> Expr {
        let initial_len = self.local_values;

        let args = (0..closure.arity())
            .map(|_| Rc::new(Value::local(self.local_values.push().to_level())))
            .collect();
        let value = self.elim_ctx().call_closure(closure, args);
        let expr = self.quote_value(&value);
        self.local_values.truncate(initial_len);

        expr
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
