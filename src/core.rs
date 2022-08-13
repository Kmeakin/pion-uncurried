use std::cell::LazyCell;
use std::rc::Rc;

use contracts::debug_invariant;

use self::env::{SharedEnv, VarIndex, VarLevel};
use crate::RcStr;

pub mod conv;
pub mod elab;
pub mod env;
pub mod errors;
pub mod eval;
pub mod quote;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Type,
    /// `x`
    Local(VarIndex),
    /// `fn (x1: e1, x2: e2, ...) -> en`
    FunType(Rc<[Option<RcStr>]>, Rc<[Self]>, Rc<Self>),
    /// `fn (x1: e1, x2: e2, ...) => en`
    FunExpr(Rc<[Option<RcStr>]>, Rc<[Self]>, Rc<Self>),
    /// `e1(e2, e3, ...)`
    FunCall(Rc<Self>, Rc<[Self]>),
    /// `let x = e1 in e2`
    Let(Option<RcStr>, Rc<Self>, Rc<Self>),
    /// `e1: e2`
    Ann(Rc<Self>, Rc<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Error,
    Wildcard,
    Name(RcStr),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Error,
    Type,
    Stuck(Head, Vec<Elim>),
    FunType(Telescope, FunClosure),
    FunValue(Telescope, FunClosure),
}

impl Value {
    pub fn local(level: VarLevel) -> Value { Self::Stuck(Head::Local(level), Vec::new()) }
}

type LazyValue = LazyCell<Value, LazyEval>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LazyEval {
    pub local_values: SharedEnv<Rc<Value>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    Local(VarLevel),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Elim {
    FunCall(Vec<Rc<Value>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Telescope {
    pub names: Rc<[Option<RcStr>]>,
    pub types: Rc<[Rc<Value>]>,
}

#[debug_invariant(self.names.len() == self.types.len())]
impl Telescope {
    pub fn new(names: Rc<[Option<RcStr>]>, types: Rc<[Rc<Value>]>) -> Self { Self { names, types } }

    pub fn arity(&self) -> usize { self.types.len() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunClosure {
    pub local_values: SharedEnv<Rc<Value>>,
    pub arity: usize,
    pub body: Rc<Expr>,
}

impl FunClosure {
    pub fn new(local_values: SharedEnv<Rc<Value>>, arity: usize, body: Rc<Expr>) -> Self {
        Self {
            local_values,
            arity,
            body,
        }
    }
}
