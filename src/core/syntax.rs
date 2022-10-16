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
    pub r#type: (Expr, Arc<Value>),
    pub body: (Expr, Arc<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: Symbol,
    pub args: Telescope<Expr>,
    pub ret_type: (Expr, Arc<Value>),
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: Symbol,
    pub args: Telescope<Expr>,
    pub arg_values: Telescope<Arc<Value>>,
    pub ret_type: (Expr, Arc<Value>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Prim(Prim),
    Lit(Lit),
    Local(VarIndex),
    Global(GlobalVar),
    Meta(VarLevel),
    MetaInsertion(VarLevel, SharedEnv<LocalSource>),
    FunType(Telescope<Self>, Arc<Self>),
    FunExpr(Telescope<Self>, Arc<Self>),
    FunCall(Arc<Self>, Arc<[Self]>),
    Match(Arc<Self>, Arc<[(Pat, Self)]>),
    Let(Arc<Pat>, Arc<Self>, Arc<Self>, Arc<Self>),
}

impl Expr {
    pub const ERROR: Self = Self::Prim(Prim::Error);
    pub const TYPE: Self = Self::Prim(Prim::Type);
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Prim {
    Error,
    Type,
    BoolType,
}

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum GlobalVar {
    Let(ir::LetDef),
    Enum(ir::EnumDef),
    Variant(ir::EnumVariant),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunArg<Type> {
    pub pat: Pat,
    pub r#type: Type,
}

impl<Type> FunArg<Type> {
    pub fn num_binders(&self) -> EnvLen { self.pat.num_binders() }
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
    Lit(Lit),
    Stuck(Head, Vec<Elim>),
    FunType(FunClosure),
    FunValue(FunClosure),
}

impl Value {
    pub const ERROR: Self = Self::prim(Prim::Error);
    pub const TYPE: Self = Self::prim(Prim::Type);

    pub const fn prim(prim: Prim) -> Self { Self::Stuck(Head::Prim(prim), Vec::new()) }
    pub const fn local(level: VarLevel) -> Self { Self::Stuck(Head::Local(level), Vec::new()) }
    pub const fn meta(level: VarLevel) -> Self { Self::Stuck(Head::Meta(level), Vec::new()) }
    pub const fn let_def(def: ir::LetDef) -> Self {
        Self::Stuck(Head::Global(GlobalVar::Let(def)), Vec::new())
    }
    pub const fn enum_def(def: ir::EnumDef) -> Self {
        Self::Stuck(Head::Global(GlobalVar::Enum(def)), Vec::new())
    }
    pub const fn enum_variant(def: ir::EnumVariant) -> Self {
        Self::Stuck(Head::Global(GlobalVar::Variant(def)), Vec::new())
    }
    pub const fn global(var: GlobalVar) -> Self { Self::Stuck(Head::Global(var), Vec::new()) }

    pub fn as_enum(&self) -> Option<(ir::EnumDef, &[Arc<Self>])> {
        match self {
            Self::Stuck(Head::Global(GlobalVar::Enum(r#enum)), spine) => match spine.as_slice() {
                [] => Some((*r#enum, &[])),
                [Elim::FunCall(args)] => Some((*r#enum, args)),
                _ => None,
            },
            _ => None,
        }
    }

    pub fn as_variant(&self) -> Option<(ir::EnumVariant, &[Arc<Self>])> {
        match self {
            Self::Stuck(Head::Global(GlobalVar::Variant(variant)), spine) => {
                match spine.as_slice() {
                    [] => Some((*variant, &[])),
                    [Elim::FunCall(args)] => Some((*variant, args)),
                    _ => None,
                }
            }
            _ => None,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    Prim(Prim),
    Local(VarLevel),
    Meta(VarLevel),
    Global(GlobalVar),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Elim {
    FunCall(Vec<Arc<Value>>),
    Match(MatchClosure),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct FunClosure {
    pub env: SharedEnv<Arc<Value>>,
    pub args: Telescope<Expr>,
    pub body: Arc<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Telescope<Type>(pub Arc<[FunArg<Type>]>);

impl<Type> Telescope<Type> {
    pub fn new(args: Arc<[FunArg<Type>]>) -> Self { Self(args) }
    pub fn len(&self) -> usize { self.0.len() }
    pub fn is_empty(&self) -> bool { self.len() == 0 }
    pub fn iter(&self) -> impl ExactSizeIterator<Item = &FunArg<Type>> { self.0.iter() }
    pub fn num_binders(&self) -> EnvLen { self.0.iter().map(|arg| arg.num_binders()).sum() }
}

impl<Type> FromIterator<FunArg<Type>> for Telescope<Type> {
    fn from_iter<T: IntoIterator<Item = FunArg<Type>>>(iter: T) -> Self {
        Self(Arc::<[FunArg<Type>]>::from_iter(iter))
    }
}

impl FunClosure {
    pub fn new(env: SharedEnv<Arc<Value>>, args: Telescope<Expr>, body: Arc<Expr>) -> Self {
        Self { env, args, body }
    }

    pub fn arity(&self) -> usize { self.args.len() }

    pub fn num_binders(&self) -> EnvLen { self.args.num_binders() }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MatchClosure {
    pub env: SharedEnv<Arc<Value>>,
    pub branches: Arc<[(Pat, Expr)]>,
}

impl MatchClosure {
    pub fn new(env: SharedEnv<Arc<Value>>, branches: Arc<[(Pat, Expr)]>) -> Self {
        Self { env, branches }
    }

    pub fn len(&self) -> usize { self.branches.len() }

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
