use std::cell::OnceCell;
use std::rc::Rc;

use contracts::debug_requires;

use crate::env::{Env, EnvLen, VarIndex, VarLevel};

pub type RcExpr = Rc<Expr>;
pub type RcValue = Rc<Value>;
pub type ValueEnv = Env<RcValue>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Local(VarIndex),
    App(Rc<Self>, Rc<Self>),
    Lambda(Rc<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Local(VarLevel),
    App(Rc<Self>, Rc<Self>),
    Lambda(FunClosure),
    Lazy(LazyClosure),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunClosure {
    pub env: ValueEnv,
    pub body: Rc<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LazyClosure {
    cell: OnceCell<RcValue>,
    env: ValueEnv,
    body: RcExpr,
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
    pub fn lambda(closure: FunClosure) -> Self { Self::Lambda(closure) }
    pub fn app(fun: Self, arg: Self) -> Self { Self::App(Rc::new(fun), Rc::new(arg)) }
    pub fn lazy(value: LazyClosure) -> Self { Self::Lazy(value) }

    pub fn is_closed(&self, len: EnvLen) -> bool {
        match self {
            Self::Local(var) => var.0 < len.0,
            Self::App(fun, arg) => fun.is_closed(len) && arg.is_closed(len),
            Self::Lambda(closure) => closure.is_closed(),
            Self::Lazy(closure) => closure.is_closed(),
        }
    }
}

impl FunClosure {
    pub fn new(env: ValueEnv, body: Rc<Expr>) -> Self { Self { env, body } }

    pub fn apply(&self, flags: EvalFlags, arg: RcValue) -> RcValue {
        reduce(flags, &self.env.pushed(arg), &self.body)
    }

    pub fn is_closed(&self) -> bool { self.body.is_closed(self.env.len().next()) }
}

impl LazyClosure {
    pub fn new(env: ValueEnv, body: RcExpr) -> Self {
        Self {
            cell: OnceCell::new(),
            env,
            body,
        }
    }

    pub fn force(&self, flags: EvalFlags) -> RcValue {
        self.cell
            .get_or_init(|| reduce(flags, &self.env, &self.body))
            .clone()
    }

    pub fn is_closed(&self) -> bool { self.body.is_closed(self.env.len()) }
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

impl EvalStrategy {
    pub fn is_strong(&self) -> bool {
        match self {
            Self::NF | Self::HNF => true,
            Self::WNF | Self::WHNF => false,
        }
    }

    pub fn is_strict(&self) -> bool {
        match self {
            Self::NF | Self::WNF => true,
            Self::HNF | Self::WHNF => false,
        }
    }
}

bitflags::bitflags! {
    pub struct EvalFlags: u16 {
        /// Reduce arguments before application
        const STRICT = 0b0000_0000_0000_0001;
        /// Reduce inside lambda
        const STRONG = 0b0000_0000_0000_0010;
        /// Weak Head Normal Form (aka call by name)
        /// - Reduce arguments before application: no
        /// - Reduce inside lambda: no
        const WHNF   = 0b0000_0000_0000_0000;
        /// Weak Normal Form (aka call by value)
        /// - Reduce arguments before application: yes
        /// - Reduce inside lambda: no
        const WNF    = 0b0000_0000_0000_0001;
        /// Head Normal Form
        /// - Reduce arguments before application: no
        /// - Reduce inside lambda: yes
        const HNF    = 0b0000_0000_0000_0010;
        /// Normal Form
        /// - Reduce arguments before application: yes
        /// - Reduce inside lambda: yes
        const NF     = 0b0000_0000_0000_0011;

        const BETA_LAMBDA = 0b0000_0000_0001_0000;

        const DELTA_LOCAL = 0b0000_0001_0000_0000;
    }
}

impl EvalFlags {
    pub fn strict(&self) -> bool { self.contains(Self::STRICT) }
    pub fn strong(&self) -> bool { self.contains(Self::STRONG) }

    pub fn beta_lambda(&self) -> bool { self.contains(Self::BETA_LAMBDA) }
    pub fn delta_local(&self) -> bool { self.contains(Self::DELTA_LOCAL) }
}

#[debug_requires(expr.is_closed(env.len()))]
#[debug_ensures(ret.is_closed(env.len()))]
pub fn normalise(flags: EvalFlags, env: &ValueEnv, expr: &RcExpr) -> RcExpr {
    let value = reduce(flags, env, expr);
    quote(flags, env.len(), &value)
}

#[debug_requires(expr.is_closed(env.len()))]
#[debug_ensures(ret.is_closed(env.len()))]
pub fn reduce(flags: EvalFlags, env: &ValueEnv, expr: &RcExpr) -> RcValue {
    match expr.as_ref() {
        Expr::Local(var) => match flags.delta_local() {
            true => match env.get(*var) {
                Some(value) => value.clone(),
                None => unreachable!("Unbound local variable {var:?}"),
            },
            false => match env.len().index_to_level(*var) {
                Some(var) => Rc::new(Value::Local(var)),
                None => unreachable!("Unbound local variable {var:?}"),
            },
        },
        Expr::Lambda(body) => match flags.strong() {
            true => {
                let arg = Rc::new(Value::Local(env.len().to_level()));
                let body = reduce(flags, &env.pushed(arg), body);
                let body = quote(flags, env.len().next(), &body);
                let closure = FunClosure::new(env.clone(), body);
                Rc::new(Value::Lambda(closure))
            }
            false => Rc::new(Value::Lambda(FunClosure::new(env.clone(), body.clone()))),
        },
        Expr::App(fun, arg) => {
            let fun = reduce(flags, env, fun);
            let arg = match flags.strict() {
                true => reduce(flags, env, arg),
                false => Rc::new(Value::lazy(LazyClosure::new(env.clone(), arg.clone()))),
            };
            match fun.as_ref() {
                Value::Lambda(closure) if flags.beta_lambda() => closure.apply(flags, arg),
                _ => Rc::new(Value::App(fun, arg)),
            }
        }
    }
}

#[debug_requires(value.is_closed(len))]
#[debug_ensures(ret.is_closed(len))]
pub fn quote(flags: EvalFlags, len: EnvLen, value: &RcValue) -> RcExpr {
    match value.as_ref() {
        Value::Local(var) => match len.level_to_index(*var) {
            Some(var) => Rc::new(Expr::local(var)),
            None => unreachable!("Unbound local variable {var:?}"),
        },
        Value::Lambda(closure) => Rc::new(Expr::Lambda(quote(
            flags,
            len.next(),
            &closure.apply(flags, Rc::new(Value::local(len.to_level()))),
        ))),
        Value::App(fun, arg) => Rc::new(Expr::App(quote(flags, len, fun), quote(flags, len, arg))),
        Value::Lazy(closure) => quote(flags, len, &closure.force(flags)),
    }
}
