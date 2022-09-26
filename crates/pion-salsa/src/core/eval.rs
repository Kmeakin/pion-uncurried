use std::sync::Arc;

use contracts::debug_ensures;

use super::env::{LocalSource, SharedEnv, UniqueEnv};
use super::syntax::*;

pub struct EvalCtx<'env> {
    local_env: &'env mut SharedEnv<Arc<Value>>,
    meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
}

impl<'env> EvalCtx<'env> {
    pub fn new(
        local_env: &'env mut SharedEnv<Arc<Value>>,
        meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
    ) -> Self {
        Self {
            local_env,
            meta_env,
        }
    }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new(self.meta_env) }

    #[debug_ensures(self.local_env.len() == old(self.local_env.len()))]
    pub fn eval_expr(&mut self, expr: &Expr) -> Arc<Value> {
        match expr {
            Expr::Error => Arc::new(Value::Error),
            Expr::Type => Arc::new(Value::Type),
            Expr::BoolType => Arc::new(Value::BoolType),
            Expr::Lit(lit) => Arc::new(Value::Lit(lit.clone())),
            Expr::Local(index) => match self.local_env.get(*index) {
                Some(value) => value.clone(),
                None => unreachable!("Unbound local variable: {index:?}"),
            },
            Expr::Meta(level) => match self.meta_env.get(*level) {
                Some(Some(value)) => value.clone(),
                Some(None) => Arc::new(Value::meta(*level)),
                None => unreachable!("Unbound meta variable: {level:?}"),
            },
            Expr::MetaInsertion(level, sources) => {
                let mut head = self.eval_expr(&Expr::Meta(*level));
                for (source, value) in sources.iter().zip(self.local_env.iter()) {
                    head = match source {
                        LocalSource::Def => head,
                        LocalSource::Param => {
                            self.elim_ctx().do_fun_call(head, vec![value.clone()])
                        }
                    };
                }
                head
            }
            Expr::FunType(args, ret) => Arc::new(Value::FunType(FunClosure::new(
                self.local_env.clone(),
                args.clone(),
                ret.clone(),
            ))),
            Expr::FunExpr(args, body) => Arc::new(Value::FunValue(FunClosure::new(
                self.local_env.clone(),
                args.clone(),
                body.clone(),
            ))),
            Expr::FunCall(fun, args) => {
                let fun = self.eval_expr(fun);
                let args = args.iter().map(|arg| self.eval_expr(arg)).collect();
                self.elim_ctx().do_fun_call(fun, args)
            }
            Expr::Let(_, _, init, body) => {
                let init_value = self.eval_expr(init);
                self.local_env.push(init_value);
                let body_value = self.eval_expr(body);
                self.local_env.pop();
                body_value
            }
            Expr::Match(..) => todo!(),
        }
    }
}

pub struct ElimCtx<'env> {
    meta_env: &'env UniqueEnv<Option<Arc<Value>>>,
}

impl<'env> ElimCtx<'env> {
    pub fn new(meta_env: &'env UniqueEnv<Option<Arc<Value>>>) -> Self { Self { meta_env } }

    pub fn eval_ctx(&self, local_values: &'env mut SharedEnv<Arc<Value>>) -> EvalCtx<'env> {
        EvalCtx::new(local_values, self.meta_env)
    }

    pub fn force_value(&self, value: &Arc<Value>) -> Arc<Value> {
        let mut forced_value = value.clone();
        while let Value::Stuck(Head::Meta(level), spine) = forced_value.as_ref() {
            match self.meta_env.get(*level) {
                Some(Some(value)) => forced_value = self.apply_spine(value.clone(), spine),
                Some(None) => break,
                None => unreachable!("Unbound meta variable: {level:?}"),
            }
        }
        forced_value
    }

    fn apply_spine(&self, head: Arc<Value>, spine: &[Elim]) -> Arc<Value> {
        spine.iter().fold(head, |head, elim| match elim {
            Elim::FunCall(args) => self.do_fun_call(head, args.clone()),
        })
    }

    pub fn do_fun_call(&self, mut fun: Arc<Value>, args: Vec<Arc<Value>>) -> Arc<Value> {
        if fun.as_ref() == &Value::Error {
            return fun;
        }

        match Arc::make_mut(&mut fun) {
            Value::FunValue(closure) => self.apply_closure(closure, args),
            Value::Stuck(_, spine) => {
                spine.push(Elim::FunCall(args));
                fun
            }
            _ => unreachable!("tried to call non-fun value: {fun:?}"),
        }
    }

    pub fn apply_closure(&self, closure: &FunClosure, args: Vec<Arc<Value>>) -> Arc<Value> {
        assert_eq!(closure.arity(), args.len());
        let mut env = closure.env.clone();
        env.extend(args);
        self.eval_ctx(&mut env).eval_expr(&closure.body)
    }

    pub fn split_fun_closure(
        &self,
        mut closure: FunClosure,
    ) -> Option<(Arc<Value>, impl FnOnce(Arc<Value>) -> FunClosure)> {
        let (arg, args) = closure.args.split_first()?;
        let mut ctx = self.eval_ctx(&mut closure.env);
        let value = ctx.eval_expr(arg);

        let args = Arc::from(args);
        Some((value, move |prev| {
            closure.env.push(prev);
            closure.args = args;
            closure
        }))
    }
}
