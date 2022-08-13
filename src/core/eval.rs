use std::rc::Rc;

use contracts::debug_ensures;

use super::env::SharedEnv;
use super::*;

pub struct EvalCtx<'env> {
    local_values: &'env mut SharedEnv<Rc<Value>>,
}

impl<'env> EvalCtx<'env> {
    pub fn new(local_values: &'env mut SharedEnv<Rc<Value>>) -> Self { Self { local_values } }

    fn elim_ctx(&self) -> ElimCtx { ElimCtx::new() }

    #[debug_ensures(self.local_values.len() == old(self.local_values.len()))]
    pub fn eval_expr(&mut self, expr: &Expr) -> Rc<Value> {
        match expr {
            Expr::Error => Rc::new(Value::Error),
            Expr::Type => Rc::new(Value::Type),
            Expr::Local(idx) => match self.local_values.get_by_index(*idx) {
                Some(value) => value.clone(),
                None => unreachable!("Unbound local variable: {idx:?}"),
            },
            Expr::FunType(names, args, ret) => {
                let args: Rc<_> = args.iter().map(|arg| self.eval_expr(arg)).collect();
                let arity = args.len();
                let telescope = Telescope::new(names.clone(), args);
                let closure = FunClosure::new(self.local_values.clone(), arity, ret.clone());
                Rc::new(Value::FunType(telescope, closure))
            }
            Expr::FunExpr(names, args, body) => {
                let args: Rc<_> = args.iter().map(|arg| self.eval_expr(arg)).collect();
                let arity = args.len();
                let telescope = Telescope::new(names.clone(), args);
                let closure = FunClosure::new(self.local_values.clone(), arity, body.clone());
                Rc::new(Value::FunValue(telescope, closure))
            }
            Expr::FunCall(fun, args) => {
                let fun = self.eval_expr(fun);
                let args = args.iter().map(|arg| self.eval_expr(arg)).collect();
                self.elim_ctx().call_fun(fun, args)
            }
            Expr::Let(_, init, body) => {
                let init_value = self.eval_expr(init);
                self.local_values.push(init_value);
                let body_value = self.eval_expr(body);
                self.local_values.pop();
                body_value
            }
            Expr::Ann(expr, _) => self.eval_expr(expr),
        }
    }
}

pub struct ElimCtx {}

impl ElimCtx {
    pub fn new() -> Self { Self {} }

    fn eval_ctx<'env>(&self, local_values: &'env mut SharedEnv<Rc<Value>>) -> EvalCtx<'env> {
        EvalCtx::new(local_values)
    }

    pub fn call_fun(&self, mut fun: Rc<Value>, args: Vec<Rc<Value>>) -> Rc<Value> {
        match Rc::make_mut(&mut fun) {
            Value::FunValue(telescope, closure) => self.call_closure(closure, args),
            Value::Stuck(_, spine) => {
                spine.push(Elim::FunCall(args));
                fun
            }
            _ => unreachable!("tried to apply non-fun value: {fun:?}"),
        }
    }

    pub fn call_closure(&self, closure: &FunClosure, args: Vec<Rc<Value>>) -> Rc<Value> {
        assert_eq!(closure.arity, args.len());
        let mut local_values = closure.local_values.clone();
        local_values.extend(args);
        self.eval_ctx(&mut local_values).eval_expr(&closure.body)
    }
}
