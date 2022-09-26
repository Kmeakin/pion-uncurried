use std::sync::Arc;

use super::env::{LocalSource, SharedEnv, VarIndex, VarLevel};
use crate::surface::syntax as surface;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Type,
    BoolType,
    Lit(Lit),
    Local(VarIndex),
    Meta(VarLevel),
    MetaInsertion(VarLevel, SharedEnv<LocalSource>),
    FunType(Arc<[Self]>, Arc<Self>),
    FunExpr(Arc<[Self]>, Arc<Self>),
    FunCall(Arc<Self>, Arc<[Self]>),
    Let(Arc<Pat>, Arc<Self>, Arc<Self>, Arc<Self>),
    Match(Arc<Self>, Arc<[(Pat, Self)]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Error,
    Lit(Lit),
    Name(VarName),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Error,
    Type,
    BoolType,
    Lit(Lit),
    Stuck(Head, Vec<Elim>),
    FunType(FunClosure),
    FunValue(FunClosure),
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
    FunCall(Vec<Arc<Value>>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunClosure {
    pub env: SharedEnv<Arc<Value>>,
    pub args: Arc<[Expr]>,
    pub body: Arc<Expr>,
}

impl FunClosure {
    pub fn new(env: SharedEnv<Arc<Value>>, args: Arc<[Expr]>, body: Arc<Expr>) -> Self {
        Self { env, args, body }
    }

    pub fn arity(&self) -> usize { self.args.len() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarName {
    User(String),
    Synth(u32),
    Underscore,
}

impl surface::Pat {
    pub fn name(&self) -> VarName {
        match self {
            Self::Error => VarName::Underscore,
            Self::Wildcard => VarName::Underscore,
            Self::Lit(_) => VarName::Underscore,
            Self::Paren(pat) => pat.name(),
            Self::Name(name) => VarName::User(name.clone()),
        }
    }
}
