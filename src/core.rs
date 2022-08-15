use std::rc::Rc;

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
    BoolType,
    Bool(bool),

    /// `x`
    Local(VarIndex),
    /// `fn (x1: e1, x2: e2, ...) -> en`
    FunType(Rc<[Self]>, Rc<Self>),
    /// `fn (x1: e1, x2: e2, ...) => en`
    FunExpr(Rc<[Self]>, Rc<Self>),
    /// `e1(e2, e3, ...)`
    FunCall(Rc<Self>, Rc<[Self]>),
    /// `let x = e1 in e2`
    Let(Rc<Self>, Rc<Self>),
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
    BoolType,
    Bool(bool),
    Stuck(Head, Vec<Elim>),
    FunType(FunClosure),
    FunValue(FunClosure),
}

impl Value {
    pub fn local(level: VarLevel) -> Self { Self::Stuck(Head::Local(level), Vec::new()) }
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
pub struct FunClosure {
    pub local_values: SharedEnv<Rc<Value>>,
    pub args: Rc<[Expr]>,
    pub body: Rc<Expr>,
}

impl FunClosure {
    pub fn new(local_values: SharedEnv<Rc<Value>>, args: Rc<[Expr]>, body: Rc<Expr>) -> Self {
        Self {
            local_values,
            args,
            body,
        }
    }

    pub fn arity(&self) -> usize { self.args.len() }
}
