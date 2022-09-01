use std::rc::Rc;

use contracts::debug_ensures;

use super::env::{EnvLen, UniqueEnv};
use super::eval::ElimCtx;
use super::{Elim, FunClosure, MatchArms, Pat, Value};

pub struct ConvCtx<'env> {
    local_env: EnvLen,
    item_values: &'env UniqueEnv<Rc<Value>>,
    meta_values: &'env UniqueEnv<Option<Rc<Value>>>,
}

impl<'env> ConvCtx<'env> {
    pub fn new(
        local_env: EnvLen,
        item_values: &'env UniqueEnv<Rc<Value>>,
        meta_values: &'env UniqueEnv<Option<Rc<Value>>>,
    ) -> Self {
        Self {
            local_env,
            item_values,
            meta_values,
        }
    }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(self.item_values, self.meta_values) }

    #[debug_ensures(self.local_env == old(self.local_env))]
    pub fn conv_values(&mut self, value1: &Rc<Value>, value2: &Rc<Value>) -> bool {
        if Rc::ptr_eq(value1, value2) {
            return true;
        }

        match (value1.as_ref(), value2.as_ref()) {
            (Value::Error, _) | (_, Value::Error) => true,
            (Value::Type, Value::Type) => true,
            (Value::BoolType, Value::BoolType) => true,
            (Value::Bool(b1), Value::Bool(b2)) => b1 == b2,

            (Value::FunValue(_, closure1), Value::FunValue(_, closure2)) => {
                self.conv_fun_closures(closure1, closure2)
            }
            (Value::FunValue(_, closure1), _) => self.conv_fun_value(closure1, value2.clone()),
            (_, Value::FunValue(_, closure2)) => self.conv_fun_value(closure2, value1.clone()),

            (Value::FunType(_, closure1), Value::FunType(_, closure2)) => {
                self.conv_fun_closures(closure1, closure2)
            }
            (Value::FunType(..), _) | (_, Value::FunType(..)) => false,

            (Value::Stuck(head1, spine1), Value::Stuck(head2, spine2)) => {
                head1 == head2 && self.conv_spines(spine1, spine2)
            }

            _ => false,
        }
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn conv_spines(&mut self, spine1: &[Elim], spine2: &[Elim]) -> bool {
        spine1.len() == spine2.len()
            && Iterator::zip(spine1.iter(), spine2.iter()).all(|(elim1, elim2)| {
                match (elim1, elim2) {
                    (Elim::FunCall(args1), Elim::FunCall(args2)) => self.conv_args(args1, args2),
                    (Elim::Match(arms1), Elim::Match(arms2)) => self.conv_arms(arms1, arms2),
                    _ => false,
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
    fn conv_arms(&mut self, arms1: &MatchArms, arms2: &MatchArms) -> bool {
        let mut arms1 = arms1.clone();
        let mut arms2 = arms2.clone();

        loop {
            match (
                self.elim_ctx().split_arms(arms1),
                self.elim_ctx().split_arms(arms2),
            ) {
                (Some((pat1, value1, next_arms1)), Some((pat2, value2, next_arms2)))
                    if self.conv_pats(&pat1, &pat2) && self.conv_values(&value1, &value2) =>
                {
                    arms1 = next_arms1;
                    arms2 = next_arms2;
                }
                (None, None) => return true,
                _ => return false,
            }
        }
    }

    fn conv_pats(&mut self, pat1: &Pat, pat2: &Pat) -> bool {
        match (pat1, pat2) {
            (Pat::Error, _) | (_, Pat::Error) => true,
            (Pat::Wildcard, _) | (_, Pat::Wildcard) => true,
            (Pat::Name(_), Pat::Name(_)) => true,
            (Pat::Bool(b1), Pat::Bool(b2)) => b1 == b2,
            _ => false,
        }
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn conv_fun_closures(&mut self, closure1: &FunClosure, closure2: &FunClosure) -> bool {
        if closure1.arity() != closure2.arity() {
            return false;
        }

        let initial_len = self.local_env;
        let mut args = Vec::with_capacity(closure1.arity());

        let mut closure1 = closure1.clone();
        let mut closure2 = closure2.clone();

        while let Some(((arg1, cont1), (arg2, cont2))) = Option::zip(
            self.elim_ctx().split_fun_closure(closure1.clone()),
            self.elim_ctx().split_fun_closure(closure2.clone()),
        ) {
            if !self.conv_values(&arg1, &arg2) {
                self.local_env.truncate(initial_len);
                return false;
            }

            let arg = Rc::new(Value::local(self.local_env.to_level()));
            closure1 = cont1(arg.clone());
            closure2 = cont2(arg.clone());
            self.local_env.push();
            args.push(arg);
        }

        let body1 = self.elim_ctx().call_closure(&closure1, args.clone());
        let body2 = self.elim_ctx().call_closure(&closure2, args);
        let result = self.conv_values(&body1, &body2);

        self.local_env.truncate(initial_len);
        result
    }

    #[debug_ensures(self.local_env == old(self.local_env))]
    fn conv_fun_value(&mut self, closure: &FunClosure, fun: Rc<Value>) -> bool {
        let initial_len = self.local_env;
        let args: Vec<_> = (0..closure.arity())
            .map(|_| Rc::new(Value::local(self.local_env.push().to_level())))
            .collect();
        self.local_env.truncate(initial_len);

        let value1 = self.elim_ctx().call_fun(fun, args.clone());
        let value2 = self.elim_ctx().call_closure(closure, args);
        self.local_env.extend(EnvLen(closure.arity()));
        let result = self.conv_values(&value1, &value2);
        self.local_env.truncate(initial_len);
        result
    }
}
