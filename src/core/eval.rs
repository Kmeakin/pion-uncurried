use std::rc::Rc;

use contracts::debug_ensures;

use super::env::{SharedEnv, UniqueEnv};
use super::*;

pub struct EvalCtx<'env> {
    local_values: &'env mut SharedEnv<Rc<Value>>,
    item_values: &'env UniqueEnv<Rc<Value>>,
    meta_values: &'env UniqueEnv<Option<Rc<Value>>>,
}

impl<'env> EvalCtx<'env> {
    pub fn new(
        local_values: &'env mut SharedEnv<Rc<Value>>,
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

    #[debug_ensures(self.local_values.len() == old(self.local_values.len()))]
    pub fn eval_expr(&mut self, expr: &Expr) -> Rc<Value> {
        match expr {
            Expr::Error => Rc::new(Value::Error),
            Expr::Type => Rc::new(Value::Type),
            Expr::BoolType => Rc::new(Value::BoolType),
            Expr::Bool(b) => Rc::new(Value::Bool(*b)),
            Expr::Local(index) => self.eval_local_var(*index),
            Expr::Item(level) => match self.item_values.get_by_level(*level) {
                Some(value) => value.clone(),
                None => unreachable!("Unbound item variable: {level:?}"),
            },
            Expr::Meta(level) => match self.meta_values.get_by_level(*level) {
                Some(Some(value)) => value.clone(),
                Some(None) => Rc::new(Value::meta(*level)),
                None => unreachable!("Unbound meta variable: {level:?}"),
            },
            Expr::MetaInsertion(level, infos) => {
                let mut head = self.eval_expr(&Expr::Meta(*level));

                for (info, value) in infos.iter().zip(self.local_values.iter()) {
                    head = match info {
                        EntryInfo::Def => head,
                        EntryInfo::Param => self.elim_ctx().call_fun(head, vec![value.clone()]),
                    };
                }
                head
            }
            Expr::FunType(names, args, ret) => Rc::new(Value::FunType(
                names.clone(),
                FunClosure::new(self.local_values.clone(), args.clone(), ret.clone()),
            )),
            Expr::FunExpr(names, args, body) => Rc::new(Value::FunValue(
                names.clone(),
                FunClosure::new(self.local_values.clone(), args.clone(), body.clone()),
            )),
            Expr::FunCall(fun, args) => {
                let fun = self.eval_expr(fun);
                let args = args.iter().map(|arg| self.eval_expr(arg)).collect();
                self.elim_ctx().call_fun(fun, args)
            }
            Expr::Match(scrut, arms) => {
                let scrut = self.eval_expr(scrut);
                let arms = MatchArms::new(self.local_values.clone(), arms.clone());
                self.elim_ctx().do_match(scrut, arms)
            }
            Expr::Let(_, _ty, init, body) => {
                let init_value = self.eval_expr(init);
                self.local_values.push(init_value);
                let body_value = self.eval_expr(body);
                self.local_values.pop();
                body_value
            }
            Expr::Ann(expr, _) => self.eval_expr(expr),
        }
    }

    fn eval_local_var(&mut self, index: VarIndex) -> Rc<Value> {
        match self.local_values.get_by_index(index) {
            Some(value) => value.clone(),
            None => unreachable!("Unbound local variable: {index:?}"),
        }
    }

    pub fn force_value(&self, value: &Rc<Value>) -> Rc<Value> {
        let mut forced_value = value.clone();
        while let Value::Stuck(head, spine) = forced_value.as_ref() {
            let head_value = match head {
                Head::Local(level) => match self.local_values.get_by_level(*level) {
                    Some(value) => value,
                    None => unreachable!("Unbound local variable: {level:?}"),
                },
                Head::Meta(level) => match self.meta_values.get_by_level(*level) {
                    Some(Some(value)) => value,
                    Some(None) => break,
                    None => unreachable!("Unbound meta variable: {level:?}"),
                },
            };
            dbg!(head_value);
            forced_value = self.elim_ctx().apply_spine(head_value.clone(), spine)
        }
        forced_value
    }
}

pub struct ElimCtx<'env> {
    item_values: &'env UniqueEnv<Rc<Value>>,
    meta_values: &'env UniqueEnv<Option<Rc<Value>>>,
}

impl<'env> ElimCtx<'env> {
    pub fn new(
        item_values: &'env UniqueEnv<Rc<Value>>,
        meta_values: &'env UniqueEnv<Option<Rc<Value>>>,
    ) -> Self {
        Self {
            item_values,
            meta_values,
        }
    }

    pub fn eval_ctx(&self, local_values: &'env mut SharedEnv<Rc<Value>>) -> EvalCtx<'env> {
        EvalCtx::new(local_values, self.item_values, self.meta_values)
    }

    pub fn call_fun(&self, mut fun: Rc<Value>, args: Vec<Rc<Value>>) -> Rc<Value> {
        match Rc::make_mut(&mut fun) {
            Value::Error => fun,
            Value::FunValue(_, closure) => self.call_closure(closure, args),
            Value::Stuck(_, spine) => {
                spine.push(Elim::FunCall(args));
                fun
            }
            _ => unreachable!("tried to apply non-fun value: {fun:?}"),
        }
    }

    pub fn do_match(&self, mut scrut: Rc<Value>, arms: MatchArms) -> Rc<Value> {
        match Rc::make_mut(&mut scrut) {
            Value::Error => return scrut,
            Value::Stuck(_, spine) => {
                spine.push(Elim::Match(arms));
                return scrut;
            }
            _ => {}
        };

        let MatchArms {
            mut local_values,
            arms,
        } = arms;
        for (pat, expr) in arms.iter() {
            match (pat, scrut.clone()) {
                (Pat::Error, _) => return Rc::new(Value::Error),
                (Pat::Name(_), scrut) => {
                    local_values.push(scrut);
                    return self.eval_ctx(&mut local_values).eval_expr(expr);
                }
                (Pat::Bool(b1), scrut) => match scrut.as_ref() {
                    Value::Bool(b2) if b1 == b2 => {
                        return self.eval_ctx(&mut local_values).eval_expr(expr);
                    }
                    _ => continue,
                },
            }
        }

        unreachable!("non-exhaustive match: {scrut:?}")
    }

    pub fn call_closure(&self, closure: &FunClosure, args: Vec<Rc<Value>>) -> Rc<Value> {
        assert_eq!(closure.arity(), args.len());
        let mut local_values = closure.local_values.clone();
        local_values.extend(args);
        self.eval_ctx(&mut local_values).eval_expr(&closure.body)
    }

    pub fn split_fun_closure(
        &self,
        mut closure: FunClosure,
    ) -> Option<(Rc<Value>, impl FnOnce(Rc<Value>) -> FunClosure)> {
        let (arg, args) = closure.args.split_first()?;
        let mut ctx = self.eval_ctx(&mut closure.local_values);
        let value = ctx.eval_expr(arg);

        let args = Rc::from(args);
        Some((value, move |prev| {
            closure.local_values.push(prev);
            closure.args = args;
            closure
        }))
    }

    pub fn split_arms(&self, mut arms: MatchArms) -> Option<(Pat, Rc<Value>, MatchArms)> {
        match arms.arms.split_first() {
            None => None,
            Some((first, rest)) => {
                let first = first.clone();
                arms.arms = Rc::from(rest);

                let mut ctx = self.eval_ctx(&mut arms.local_values);
                let (pat, expr) = first;
                Some((pat, ctx.eval_expr(&expr), arms))
            }
        }
    }

    pub fn force_value(&self, value: &Rc<Value>) -> Rc<Value> {
        let mut forced_value = value.clone();
        while let Value::Stuck(Head::Meta(level), spine) = forced_value.as_ref() {
            match self.meta_values.get_by_level(*level) {
                Some(Some(value)) => forced_value = self.apply_spine(value.clone(), spine),
                Some(None) => break,
                None => unreachable!("Unbound meta variable: {level:?}"),
            }
        }
        forced_value
    }

    fn apply_spine(&self, head: Rc<Value>, spine: &[Elim]) -> Rc<Value> {
        spine.iter().fold(head, |head, elim| match elim {
            Elim::FunCall(args) => self.call_fun(head, args.clone()),
            Elim::Match(arms) => self.do_match(head, arms.clone()),
        })
    }
}
