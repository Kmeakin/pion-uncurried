use std::sync::Arc;

use super::env::{EnvLen, LocalSource, SharedEnv, VarIndex, VarLevel};
use crate::ir::symbol::Symbol;
use crate::ir::syntax as ir;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Let(LetDef),
    Enum(EnumDef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDef {
    pub name: Symbol,
    pub body: (Expr, Arc<Value>),
    pub ty: (Expr, Arc<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDefSig {
    pub args: Arc<[FunArg<Expr>]>,
    pub ret_type: Expr,
    pub self_type: Arc<Value>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: Symbol,
    pub sig: EnumDefSig,
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: Symbol,
    pub args: Arc<[FunArg<(Expr, Arc<Value>)>]>,
    pub ret_type: (Expr, Arc<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Type,
    BoolType,
    Lit(Lit),
    LetDef(ir::LetDef),
    EnumDef(ir::EnumDef),
    EnumVariant(ir::EnumVariant),
    Local(VarIndex),
    Meta(VarLevel),
    MetaInsertion(VarLevel, SharedEnv<LocalSource>),
    FunType(Arc<[FunArg<Self>]>, Arc<Self>),
    FunExpr(Arc<[FunArg<Self>]>, Arc<Self>),
    FunCall(Arc<Self>, Arc<[Self]>),
    Let(Arc<Pat>, Arc<Self>, Arc<Self>, Arc<Self>),
    Match(Arc<Self>, Arc<[(Pat, Self)]>),
}

impl Expr {
    /// Returns true if `self` is "closed" with respect to `local_len` and
    /// `meta_len` - ie `self` would not have any unbound local or meta
    /// variables when evaluated in a local environment of length `local_len`
    /// and a meta environment of length `local_len`.
    pub fn is_closed(&self, mut local_len: EnvLen, meta_len: EnvLen) -> bool {
        match self {
            Self::Error
            | Self::Type
            | Self::BoolType
            | Self::Lit(_)
            | Self::LetDef(_)
            | Self::EnumDef(_)
            | Self::EnumVariant(_) => true,
            Self::Local(index) => index.0 < local_len.0,
            Self::Meta(level) | Self::MetaInsertion(level, ..) => level.0 < meta_len.0,
            Self::FunType(args, ret) | Self::FunExpr(args, ret) => {
                args.iter().all(|FunArg { pat, ty }| {
                    let ret = ty.is_closed(local_len, meta_len);
                    local_len += pat.num_binders();
                    ret
                }) && ret.is_closed(local_len, meta_len)
            }
            Self::FunCall(fun, args) => {
                fun.is_closed(local_len, meta_len)
                    && args.iter().all(|arg| arg.is_closed(local_len, meta_len))
            }
            Self::Let(pat, ty, init, body) => {
                ty.is_closed(local_len, meta_len)
                    && init.is_closed(local_len, meta_len)
                    && body.is_closed(local_len + pat.num_binders(), meta_len)
            }
            Self::Match(scrut, arms) => {
                scrut.is_closed(local_len, meta_len)
                    && arms
                        .iter()
                        .all(|(pat, expr)| expr.is_closed(local_len + pat.num_binders(), meta_len))
            }
        }
    }
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
    Variant(ir::EnumVariant, Arc<[Self]>),
}

impl Pat {
    pub fn num_binders(&self) -> EnvLen {
        match self {
            Self::Error | Self::Lit(_) | Self::Name(_) => EnvLen(1),
            Self::Variant(_, pats) => pats.iter().map(Pat::num_binders).sum(),
        }
    }
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
    pub fn let_def(def: ir::LetDef) -> Self { Self::Stuck(Head::LetDef(def), Vec::new()) }
    pub fn enum_def(def: ir::EnumDef) -> Self { Self::Stuck(Head::EnumDef(def), Vec::new()) }
    pub fn enum_variant(def: ir::EnumVariant) -> Self {
        Self::Stuck(Head::EnumVariant(def), Vec::new())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    Local(VarLevel),
    Meta(VarLevel),
    LetDef(ir::LetDef),
    EnumDef(ir::EnumDef),
    EnumVariant(ir::EnumVariant),
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
