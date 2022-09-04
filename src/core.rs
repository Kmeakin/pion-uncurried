use std::rc::Rc;

use text_size::TextRange;

use self::env::{SharedEnv, VarIndex, VarLevel};
use crate::{surface, FileId, RcStr};

pub mod conv;
pub mod elab;
pub mod env;
pub mod errors;
pub mod eval;
pub mod quote;
pub mod unelab;
mod unify;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub decls: Rc<[Decl]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl {
    Error,
    Let(LetDecl),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDecl {
    pub name: Option<RcStr>,
    pub ty: Rc<Expr>,
    pub expr: Rc<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Type,
    BoolType,
    Bool(bool),

    /// `x`
    Local(VarIndex),
    Item(VarLevel),
    Meta(VarLevel),
    MetaInsertion(VarLevel, SharedEnv<EntryInfo>),

    /// `fn (x1: e1, x2: e2, ...) -> en`
    FunType(Rc<[VarName]>, Rc<[Self]>, Rc<Self>),
    /// `fn (x1: e1, x2: e2, ...) => en`
    FunExpr(Rc<[VarName]>, Rc<[Self]>, Rc<Self>),
    /// `e1(e2, e3, ...)`
    FunCall(Rc<Self>, Rc<[Self]>),
    /// `match e1 {p2 => e2, ..., pn => en}`
    Match(Rc<Self>, Rc<[(Pat, Self)]>),
    /// `let x: e2 = e2 in e3`
    Let(VarName, Rc<Self>, Rc<Self>, Rc<Self>),
    /// `e1: e2`
    Ann(Rc<Self>, Rc<Self>),
}

impl Expr {
    pub fn binds_local(&self, mut var: VarIndex) -> bool {
        match self {
            Self::Local(v) => *v == var,
            Self::Error
            | Self::Type
            | Self::BoolType
            | Self::Bool(_)
            | Self::Item(_)
            | Self::Meta(_)
            | Self::MetaInsertion(..) => false,
            Self::FunType(_, args, body) | Self::FunExpr(_, args, body) => {
                args.iter().any(|arg| {
                    let res = arg.binds_local(var);
                    var = var.succ();
                    res
                }) || body.binds_local(var.succ())
            }
            Self::FunCall(fun, args) => {
                fun.binds_local(var) || args.iter().any(|arg| arg.binds_local(var))
            }
            Self::Match(scrut, arms) => {
                scrut.binds_local(var)
                    || arms
                        .iter()
                        .any(|(pat, expr)| expr.binds_local(var.succ_by(pat.num_names())))
            }
            Self::Let(_, ty, init, body) => {
                ty.binds_local(var) || init.binds_local(var) || body.binds_local(var.succ())
            }
            Self::Ann(expr, ty) => expr.binds_local(var) || ty.binds_local(var),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum VarName {
    User(RcStr),
    Underscore,
    Fresh(u32),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, Default)]
pub struct NameSource(u32);

impl NameSource {
    pub fn new(counter: u32) -> Self { Self(counter) }

    pub fn next(&mut self) -> VarName {
        let next = VarName::Fresh(self.0);
        self.0 += 1;
        next
    }

    pub fn current(&self) -> VarName { VarName::Fresh(self.0) }

    pub fn truncate(&mut self, new_count: u32) { self.0 = new_count; }

    pub fn reset(&mut self) { self.0 = 0; }
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum EntryInfo {
    Def,
    Param,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum MetaSource {
    LetDeclType(FileId, TextRange),
    HoleType(FileId, TextRange),
    HoleExpr(FileId, TextRange),
    MatchType(FileId, TextRange),
    PatType(FileId, TextRange),
    Error,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Error,
    Bool(bool),
    Name(VarName),
}

impl Pat {
    pub fn num_names(&self) -> usize {
        match self {
            Self::Error | Self::Bool(_) => 0,
            Self::Name(_) => 1,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Error,
    Type,
    BoolType,
    Bool(bool),
    Stuck(Head, Vec<Elim>),
    FunType(Rc<[VarName]>, FunClosure),
    FunValue(Rc<[VarName]>, FunClosure),
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
    Match(MatchArms),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchArms {
    pub local_values: SharedEnv<Rc<Value>>,
    pub arms: Rc<[(Pat, Expr)]>,
}

impl MatchArms {
    pub fn new(local_values: SharedEnv<Rc<Value>>, arms: Rc<[(Pat, Expr)]>) -> Self {
        Self { local_values, arms }
    }
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
