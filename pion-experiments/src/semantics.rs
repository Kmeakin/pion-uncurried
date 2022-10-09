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
    Const(Const),
    App(Rc<Self>, Rc<Self>),
    FunType(Rc<Self>, Rc<Self>),
    FunExpr(Rc<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Const {
    Type,
    StringType,
    StringValue(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Local(VarLevel),
    Const(Const),
    FunType(Rc<Self>, FunClosure),
    FunValue(FunClosure),
    App(Rc<Self>, Rc<Self>),
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
    pub const TYPE: Self = Self::Const(Const::Type);

    pub fn is_closed(&self, len: EnvLen) -> bool {
        match self {
            Self::Const(_) => true,
            Self::Local(var) => var.0 < len.0,
            Self::App(fun, arg) => fun.is_closed(len) && arg.is_closed(len),
            Self::FunExpr(body) => body.is_closed(len.next()),
            Self::FunType(domain, range) => domain.is_closed(len) && range.is_closed(len.next()),
        }
    }
}

impl Value {
    pub const TYPE: Self = Self::Const(Const::Type);

    pub fn is_closed(&self, len: EnvLen) -> bool {
        match self {
            Self::Const(_) => true,
            Self::Local(var) => var.0 < len.0,
            Self::App(fun, arg) => fun.is_closed(len) && arg.is_closed(len),
            Self::Lazy(closure) => closure.is_closed(),
            Self::FunValue(closure) => closure.is_closed(),
            Self::FunType(domain, closure) => domain.is_closed(len) && closure.is_closed(),
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

        const ALL_REDUCTIONS = Self::BETA_LAMBDA.bits | Self::DELTA_LOCAL.bits;

        const EVAL_WHNF = Self::WHNF.bits | Self::ALL_REDUCTIONS.bits;
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
pub fn normalise(flags: EvalFlags, env: &ValueEnv, expr: &RcExpr) -> Expr {
    let value = reduce(flags, env, expr);
    quote(flags, env.len(), &value)
}

#[debug_requires(expr.is_closed(env.len()))]
#[debug_ensures(ret.is_closed(env.len()))]
pub fn reduce(flags: EvalFlags, env: &ValueEnv, expr: &Expr) -> RcValue {
    match expr {
        Expr::Const(c) => Rc::new(Value::Const(c.clone())),
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
        Expr::FunType(domain, range) => match flags.strong() {
            true => {
                let arg = Rc::new(Value::Local(env.len().to_level()));
                let domain = reduce(flags, env, domain);
                let range = reduce(flags, &env.pushed(arg), range);
                let range = quote(flags, env.len().next(), &range);
                let closure = FunClosure::new(env.clone(), Rc::new(range));
                Rc::new(Value::FunType(domain, closure))
            }
            false => Rc::new(Value::FunType(
                reduce(flags, env, domain),
                FunClosure::new(env.clone(), range.clone()),
            )),
        },
        Expr::FunExpr(body) => match flags.strong() {
            true => {
                let arg = Rc::new(Value::Local(env.len().to_level()));
                let body = reduce(flags, &env.pushed(arg), body);
                let body = quote(flags, env.len().next(), &body);
                let closure = FunClosure::new(env.clone(), Rc::new(body));
                Rc::new(Value::FunValue(closure))
            }
            false => Rc::new(Value::FunValue(FunClosure::new(env.clone(), body.clone()))),
        },
        Expr::App(fun, arg) => {
            let fun = reduce(flags, env, fun);
            let arg = match flags.strict() {
                true => reduce(flags, env, arg),
                false => Rc::new(Value::Lazy(LazyClosure::new(env.clone(), arg.clone()))),
            };
            match fun.as_ref() {
                Value::FunValue(closure) if flags.beta_lambda() => closure.apply(flags, arg),
                _ => Rc::new(Value::App(fun, arg)),
            }
        }
    }
}

#[debug_requires(value.is_closed(len))]
#[debug_ensures(ret.is_closed(len))]
pub fn quote(flags: EvalFlags, len: EnvLen, value: &RcValue) -> Expr {
    match value.as_ref() {
        Value::Const(c) => Expr::Const(c.clone()),
        Value::Local(var) => match len.level_to_index(*var) {
            Some(var) => Expr::Local(var),
            None => unreachable!("Unbound local variable {var:?}"),
        },
        Value::FunValue(closure) => {
            let closure = quote_closure(flags, len, closure);
            Expr::FunExpr(Rc::new(closure))
        }
        Value::FunType(domain, closure) => {
            let domain = quote(flags, len, domain);
            let closure = quote_closure(flags, len, closure);
            Expr::FunType(Rc::new(domain), Rc::new(closure))
        }
        Value::App(fun, arg) => {
            let fun = quote(flags, len, fun);
            let arg = quote(flags, len, arg);
            Expr::App(Rc::new(fun), Rc::new(arg))
        }
        Value::Lazy(closure) => quote(flags, len, &closure.force(flags)),
    }
}

#[debug_requires(closure.is_closed())]
#[debug_ensures(ret.is_closed(len.next()))]
fn quote_closure(flags: EvalFlags, len: EnvLen, closure: &FunClosure) -> Expr {
    let arg = Rc::new(Value::Local(len.to_level()));
    let body = closure.apply(flags, arg);
    quote(flags, len.next(), &body)
}
