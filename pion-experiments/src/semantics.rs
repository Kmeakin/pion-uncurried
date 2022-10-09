use std::rc::Rc;

use contracts::debug_requires;

use crate::env::{Env, EnvLen, VarIndex, VarLevel};

pub type RcExpr = Rc<Expr>;
pub type RcValue = Rc<Value>;
pub type ValueEnv = Env<RcValue>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Local(VarIndex),
    Lambda(Rc<Self>),
    App(Rc<Self>, Rc<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Local(VarLevel),
    Lambda(Closure),
    App(Rc<Self>, Rc<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Closure {
    pub env: ValueEnv,
    pub body: Rc<Expr>,
}

impl Expr {
    pub fn local(var: VarIndex) -> Self { Self::Local(var) }
    pub fn lambda(body: Self) -> Self { Self::Lambda(Rc::new(body)) }
    pub fn app(fun: Self, arg: Self) -> Self { Self::App(Rc::new(fun), Rc::new(arg)) }

    pub fn is_closed(&self, len: EnvLen) -> bool {
        match self {
            Self::Local(var) => var.0 < len.0,
            Self::Lambda(body) => body.is_closed(len.next()),
            Self::App(fun, arg) => fun.is_closed(len) && arg.is_closed(len),
        }
    }
}

impl Value {
    pub fn local(var: VarLevel) -> Self { Self::Local(var) }
    pub fn lambda(closure: Closure) -> Self { Self::Lambda(closure) }
    pub fn app(fun: Self, arg: Self) -> Self { Self::App(Rc::new(fun), Rc::new(arg)) }

    pub fn is_closed(&self, len: EnvLen) -> bool {
        match self {
            Self::Local(var) => var.0 < len.0,
            Self::Lambda(closure) => closure.is_closed(),
            Self::App(fun, arg) => fun.is_closed(len) && arg.is_closed(len),
        }
    }
}

impl Closure {
    pub fn new(env: ValueEnv, body: Rc<Expr>) -> Self { Self { env, body } }

    pub fn apply(&self, arg: RcValue) -> RcValue { eval(&self.env.pushed(arg), &self.body) }

    pub fn is_closed(&self) -> bool { self.body.is_closed(self.env.len().next()) }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Hash)]
pub enum EvalStrategy {
    /// Normal Form
    /// - Reduce arguments before application: yes
    /// - Reduce inside lambda: yes
    NF,

    /// Head Normal Form
    /// - Reduce arguments before application: no
    /// - Reduce inside lambda: yes
    HNF,

    /// Weak Normal Form (aka call by value)
    /// - Reduce arguments before application: yes
    /// - Reduce inside lambda: no
    WNF,

    /// Weak Head Normal Form (aka call by name)
    /// - Reduce arguments before application: no
    /// - Reduce inside lambda: no
    WHNF,
}

#[debug_requires(expr.is_closed(env.len()))]
#[debug_ensures(ret.is_closed(env.len()))]
pub fn normalise(env: &ValueEnv, expr: &RcExpr) -> RcExpr {
    let value = eval(env, expr);
    quote(env.len(), &value)
}

#[debug_requires(expr.is_closed(env.len()))]
#[debug_ensures(ret.is_closed(env.len()))]
pub fn eval(env: &ValueEnv, expr: &RcExpr) -> RcValue {
    match expr.as_ref() {
        Expr::Local(var) => match env.get(*var) {
            Some(value) => value.clone(),
            None => unreachable!("Unbound local variable {var:?}"),
        },
        Expr::Lambda(body) => Rc::new(Value::Lambda(Closure::new(env.clone(), body.clone()))),
        Expr::App(fun, arg) => {
            let fun = eval(env, fun);
            let arg = eval(env, arg);
            match fun.as_ref() {
                Value::Lambda(closure) => closure.apply(arg),
                _ => Rc::new(Value::App(fun, arg)),
            }
        }
    }
}

#[debug_requires(value.is_closed(len))]
#[debug_ensures(ret.is_closed(len))]
pub fn quote(len: EnvLen, value: &RcValue) -> RcExpr {
    match value.as_ref() {
        Value::Local(var) => match len.level_to_index(*var) {
            Some(var) => Rc::new(Expr::local(var)),
            None => unreachable!("Unbound local variable {var:?}"),
        },
        Value::Lambda(closure) => Rc::new(Expr::Lambda(quote(
            len.next(),
            &apply(closure, Rc::new(Value::local(len.to_level()))),
        ))),
        Value::App(fun, arg) => Rc::new(Expr::App(quote(len, fun), quote(len, arg))),
    }
}

fn apply(closure: &Closure, arg: RcValue) -> RcValue {
    eval(&closure.env.pushed(arg), &closure.body)
}
