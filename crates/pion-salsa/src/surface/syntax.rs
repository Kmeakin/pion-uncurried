#![allow(clippy::use_self)]

use std::sync::Arc;

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SourceFile {
    pub text: String,
}

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Let(LetDef),
    Enum(EnumDef),
}

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDef {
    pub name: String,
    #[return_ref]
    pub ty: Option<Expr>,
    #[return_ref]
    pub expr: Expr,
}

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: String,
    #[return_ref]
    pub args: Vec<AnnPat>,
    #[return_ref]
    pub variants: Vec<EnumVariant>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
    pub args: Vec<AnnPat>,
    pub ty: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Paren(Arc<Self>),
    Lit(Lit),
    Name(String),
    FunType(Vec<AnnPat>, Arc<Self>),
    FunExpr(Vec<AnnPat>, Arc<Self>),
    FunCall(Arc<Self>, Vec<Self>),
    Let(Arc<AnnPat>, Arc<Self>, Arc<Self>),
    Match(Arc<Self>, Vec<(Pat, Self)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Error,
    Wildcard,
    Paren(Arc<Self>),
    Lit(Lit),
    Name(String),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnPat {
    pub pat: Pat,
    pub ty: Option<Expr>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
}
