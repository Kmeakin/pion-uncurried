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
    pub name: (Span, String),
    pub type_: Option<Expr<Span>>,
    pub body: Expr<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef<Span> {
    pub name: String,
    pub args: Vec<AnnPat<Span>>,
    pub ret_type: Option<Expr<Span>>,
    pub variants: Vec<EnumVariant<Span>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant<Span> {
    pub name: String,
    pub args: Vec<AnnPat<Span>>,
    pub ret_type: Option<Expr<Span>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<Span> {
    Error(Span),
    Lit(Span, Lit<Span>),
    Name(Span, String),
    Hole(Span, Hole),
    FunType(Span, Vec<AnnPat<Span>>, Arc<Self>),
    FunExpr(Span, Vec<AnnPat<Span>>, Arc<Self>),
    FunCall(Span, Arc<Self>, Vec<Self>),
    Let(Span, Arc<AnnPat<Span>>, Arc<Self>, Arc<Self>),
    Match(Span, Arc<Self>, Vec<(Pat<Span>, Self)>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Hole {
    Underscore,
    Name(String),
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
            | Self::Hole(span, ..)
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
    Lit(Span, Lit<Span>),
    Name(Span, String),
    Variant(Span, String, Vec<Self>),
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
            | Pat::Name(span, ..)
            | Pat::Variant(span, ..) => span.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AnnPat<Span> {
    pub pat: Pat<Span>,
    pub type_: Option<Expr<Span>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit<Span> {
    Bool(Span, bool),
    String(Span, String),
}
