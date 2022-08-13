use std::rc::Rc;

use contracts::debug_ensures;

use super::env::EnvLen;
use super::eval::ElimCtx;
use super::{Elim, FunClosure, Telescope, Value};

pub struct ConvCtx {
    local_env: EnvLen,
}

impl ConvCtx {
    pub fn new(local_env: EnvLen) -> Self { Self { local_env } }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new() }

    #[debug_ensures(self.local_env == old(self.local_env))]
    pub fn conv_values(&mut self, value1: &Rc<Value>, value2: &Rc<Value>) -> bool {
        if Rc::ptr_eq(value1, value2) {
            return true;
        }

        match (value1.as_ref(), value2.as_ref()) {
            (Value::Error, _) | (_, Value::Error) => true,

            (Value::FunValue(_, closure1), Value::FunValue(_, closure2)) => {
                self.conv_fun_closures(closure1, closure2)
            }
            (Value::FunValue(_, closure1), _) => self.conv_fun_value(closure1, value2.clone()),
            (_, Value::FunValue(_, closure2)) => self.conv_fun_value(closure2, value1.clone()),

            (Value::FunType(telescope1, closure1), Value::FunType(telescope2, closure2)) => {
                self.conv_telescopes(telescope1, telescope2)
                    && self.conv_fun_closures(closure1, closure2)
            }
            (Value::FunType(..), _) | (_, Value::FunType(..)) => false,

            (Value::Type, Value::Type) => true,
            (Value::Type, _) | (_, Value::Type) => false,

            (Value::BoolType, Value::BoolType) => true,
            (Value::BoolType, _) | (_, Value::BoolType) => false,

            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,
            (Value::Bool(_), _) | (_, Value::Bool(_)) => false,

            (Value::Stuck(head1, spine1), Value::Stuck(head2, spine2)) => {
                head1 == head2 && self.conv_spines(spine1, spine2)
            }
            (Value::Stuck(..), _) | (_, Value::Stuck(..)) => false,
        }
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn conv_spines(&mut self, spine1: &[Elim], spine2: &[Elim]) -> bool {
        spine1.len() == spine2.len()
            && Iterator::zip(spine1.iter(), spine2.iter()).all(|(elim1, elim2)| {
                match (elim1, elim2) {
                    (Elim::FunCall(args1), Elim::FunCall(args2)) => self.conv_args(args1, args2),
                }
            })
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn conv_args(&mut self, args1: &[Rc<Value>], args2: &[Rc<Value>]) -> bool {
        args1.len() == args2.len()
            && Iterator::zip(args1.iter(), args2.iter())
                .all(|(arg1, arg2)| self.conv_values(arg1, arg2))
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn conv_telescopes(&mut self, telescope1: &Telescope, telescope2: &Telescope) -> bool {
        let initial_len = self.local_env;
        let res = telescope1.arity() == telescope2.arity()
            && Iterator::zip(telescope1.types.iter(), telescope2.types.iter()).all(
                |(value1, value2)| {
                    let res = self.conv_values(value1, value2);
                    self.local_env.push();
                    res
                },
            );
        self.local_env.truncate(initial_len);
        res
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn conv_fun_closures(&mut self, closure1: &FunClosure, closure2: &FunClosure) -> bool {
        closure1.arity == closure2.arity && {
            let initial_len = self.local_env;
            let args: Vec<_> = (0..closure1.arity)
                .map(|_| Rc::new(Value::local(self.local_env.push().to_level())))
                .collect();
            self.local_env.truncate(initial_len);
            let value1 = self.elim_ctx().call_closure(closure1, args.clone());
            let value2 = self.elim_ctx().call_closure(closure2, args);
            self.local_env.extend(EnvLen(closure1.arity));
            let result = self.conv_values(&value1, &value2);
            self.local_env.truncate(initial_len);
            result
        }
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn conv_fun_value(&mut self, closure: &FunClosure, fun: Rc<Value>) -> bool {
        let initial_len = self.local_env;
        let args: Vec<_> = (0..closure.arity)
            .map(|_| Rc::new(Value::local(self.local_env.push().to_level())))
            .collect();
        self.local_env.truncate(initial_len);

        let value1 = self.elim_ctx().call_fun(fun, args.clone());
        let value2 = self.elim_ctx().call_closure(closure, args);
        self.local_env.extend(EnvLen(closure.arity));
        let result = self.conv_values(&value1, &value2);
        self.local_env.truncate(initial_len);
        result
    }
}
