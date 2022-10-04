use std::sync::Arc;

use codespan_reporting::diagnostic::Diagnostic;

use super::env::{LocalSource, SharedEnv, VarIndex, VarLevel};
use crate::ir::input_file::InputFile;
use crate::ir::symbol::Symbol;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDef {
    pub body: (Expr, Arc<Value>),
    pub ty: (Expr, Arc<Value>),
    pub diagnostics: Vec<Diagnostic<InputFile>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Type,
    BoolType,
    Lit(Lit),
    Local(VarIndex),
    Meta(VarLevel),
    MetaInsertion(VarLevel, SharedEnv<LocalSource>),
    FunType(Arc<[FunArg<Self>]>, Arc<Self>),
    FunExpr(Arc<[FunArg<Self>]>, Arc<Self>),
    FunCall(Arc<Self>, Arc<[Self]>),
    Let(Arc<Pat>, Arc<Self>, Arc<Self>, Arc<Self>),
    Match(Arc<Self>, Arc<[(Pat, Self)]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunArg<Type> {
    pub pat: Pat,
    pub ty: Type,
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
    Match(MatchArms),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunClosure {
    pub env: SharedEnv<Arc<Value>>,
    pub args: Arc<[FunArg<Expr>]>,
    pub body: Arc<Expr>,
}

impl FunClosure {
    pub fn new(env: SharedEnv<Arc<Value>>, args: Arc<[FunArg<Expr>]>, body: Arc<Expr>) -> Self {
        Self { env, args, body }
    }

    pub fn arity(&self) -> usize { self.args.len() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArms {
    pub env: SharedEnv<Arc<Value>>,
    pub arms: Arc<[(Pat, Expr)]>,
}

impl MatchArms {
    pub fn new(env: SharedEnv<Arc<Value>>, arms: Arc<[(Pat, Expr)]>) -> Self { Self { env, arms } }

    pub fn len(&self) -> usize { self.arms.len() }

    pub fn is_empty(&self) -> bool { self.len() == 0 }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum VarName {
    User(Symbol),
    Synth(u32),
    Underscore,
}
