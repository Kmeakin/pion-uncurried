#![allow(clippy::use_self)]

use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module<Span> {
    pub items: Vec<Item<Span>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item<Span> {
    Let(LetDef<Span>),
    Enum(EnumDef<Span>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDef<Span> {
    pub name: String,
    pub ty: Option<Expr<Span>>,
    pub expr: Expr<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef<Span> {
    pub name: String,
    pub args: Vec<AnnPat<Span>>,
    pub variants: Vec<EnumVariant<Span>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant<Span> {
    pub name: String,
    pub args: Vec<AnnPat<Span>>,
    pub ty: Option<Expr<Span>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<Span> {
    Error(Span),
    Lit(Span, Lit),
    Name(Span, String),
    FunType(Span, Vec<AnnPat<Span>>, Arc<Self>),
    FunExpr(Span, Vec<AnnPat<Span>>, Arc<Self>),
    FunCall(Span, Arc<Self>, Vec<Self>),
    Let(Span, Arc<AnnPat<Span>>, Arc<Self>, Arc<Self>),
    Match(Span, Arc<Self>, Vec<(Pat<Span>, Self)>),
}

impl<Span> Expr<Span> {
    pub fn span(&self) -> Span
    where
        Span: Clone,
    {
        match self {
            Self::Error(span, ..)
            | Self::Lit(span, ..)
            | Self::Name(span, ..)
            | Self::FunType(span, ..)
            | Self::FunExpr(span, ..)
            | Self::FunCall(span, ..)
            | Self::Let(span, ..)
            | Self::Match(span, ..) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat<Span> {
    Error(Span),
    Wildcard(Span),
    Lit(Span, Lit),
    Name(Span, String),
}

impl<Span> Pat<Span> {
    pub fn span(&self) -> Span
    where
        Span: Clone,
    {
        match self {
            Pat::Error(span, ..)
            | Pat::Wildcard(span, ..)
            | Pat::Lit(span, ..)
            | Pat::Name(span, ..) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnPat<Span> {
    pub pat: Pat<Span>,
    pub ty: Option<Expr<Span>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
}
