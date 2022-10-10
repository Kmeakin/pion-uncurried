use std::sync::Arc;

use super::env::{EnvLen, LocalSource, SharedEnv, VarIndex, VarLevel};
use crate::ir;
use crate::symbol::Symbol;

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
    pub ty: (Expr, Arc<Value>),
    pub body: (Expr, Arc<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: Symbol,
    pub args: Arc<[FunArg<Expr>]>,
    pub ret_type: (Expr, Arc<Value>),
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: Symbol,
    pub args: Arc<[FunArg<Expr>]>,
    pub ret_type: (Expr, Arc<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Prim(Prim),
    Lit(Lit),
    Local(VarIndex),
    Global(GlobalVar),
    Meta(VarLevel),
    MetaInsertion(VarLevel, SharedEnv<LocalSource>),
    FunType(Arc<[FunArg<Self>]>, Arc<Self>),
    FunExpr(Arc<[FunArg<Self>]>, Arc<Self>),
    FunCall(Arc<Self>, Arc<[Self]>),
    Match(Arc<Self>, Arc<[(Pat, Self)]>),
    Let(Arc<Pat>, Arc<Self>, Arc<Self>, Arc<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prim {
    Type,
    BoolType,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GlobalVar {
    Let(ir::LetDef),
    Enum(ir::EnumDef),
    Variant(ir::EnumVariant),
}

impl Expr {
    pub const TYPE: Self = Self::Prim(Prim::Type);

    /// Returns true if `self` is "closed" with respect to `local_len` and
    /// `meta_len` - ie `self` would not have any unbound local or meta
    /// variables when evaluated in a local environment of length `local_len`
    /// and a meta environment of length `local_len`.
    pub fn is_closed(&self, mut local_len: EnvLen, meta_len: EnvLen) -> bool {
        match self {
            Self::Error | Self::Prim(_) | Self::Lit(_) | Self::Global(_) => true,
            Self::Local(var) => {
                let ret = var.0 < local_len.0;
                if !ret {
                    eprintln!("Not closed: {var:?} in {local_len:?}");
                }
                ret
            }
            Self::Meta(var) | Self::MetaInsertion(var, ..) => {
                let ret = var.0 < meta_len.0;
                if !ret {
                    eprintln!("Not closed: {var:?} in {meta_len:?}");
                }
                ret
            }
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
            Self::Variant(_, pats) => pats.iter().map(Self::num_binders).sum(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Value {
    Error,
    Prim(Prim),
    Lit(Lit),
    Stuck(Head, Vec<Elim>),
    FunType(FunClosure),
    FunValue(FunClosure),
}

impl Value {
    pub const TYPE: Self = Self::Prim(Prim::Type);

    pub fn local(level: VarLevel) -> Self { Self::Stuck(Head::Local(level), Vec::new()) }
    pub fn meta(level: VarLevel) -> Self { Self::Stuck(Head::Meta(level), Vec::new()) }
    pub fn let_def(def: ir::LetDef) -> Self {
        Self::Stuck(Head::Global(GlobalVar::Let(def)), Vec::new())
    }
    pub fn enum_def(def: ir::EnumDef) -> Self {
        Self::Stuck(Head::Global(GlobalVar::Enum(def)), Vec::new())
    }
    pub fn enum_variant(def: ir::EnumVariant) -> Self {
        Self::Stuck(Head::Global(GlobalVar::Variant(def)), Vec::new())
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    Local(VarLevel),
    Meta(VarLevel),
    Global(GlobalVar),
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
