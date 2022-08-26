use std::rc::Rc;

use text_size::TextRange;

use self::env::{SharedEnv, VarIndex, VarLevel};
use crate::{FileId, RcStr};

pub mod conv;
pub mod elab;
pub mod env;
pub mod errors;
pub mod eval;
pub mod quote;
pub mod unelab;
mod unify;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Type,
    BoolType,
    Bool(bool),

    /// `x`
    Local(VarIndex),
    Meta(VarLevel),
    MetaInsertion(VarLevel, SharedEnv<EntryInfo>),

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

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EntryInfo {
    Def,
    Param(usize),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MetaSource {
    PlaceholderType(FileId, TextRange),
    PlaceholderExpr(FileId, TextRange),
    PatType(FileId, TextRange),
    Error,
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
    FunType(Rc<[Option<RcStr>]>, FunClosure),
    FunValue(Rc<[Option<RcStr>]>, FunClosure),
}

impl Value {
    pub fn local(level: VarLevel) -> Self { Self::Stuck(Head::Local(level), Vec::new()) }
    pub fn meta(level: VarLevel) -> Self { Self::Stuck(Head::Meta(level), Vec::new()) }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    Local(VarLevel),
    Meta(VarLevel),
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
