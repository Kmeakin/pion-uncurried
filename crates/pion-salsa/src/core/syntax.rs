use std::sync::Arc;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr {
    Error,
    Type,
    BoolType,
    Lit(Lit),
    Local(VarIndex),
    FunType(Arc<[Self]>, Arc<Self>),
    FunExpr(Arc<[Self]>, Arc<Self>),
    FunCall(Arc<Self>, Arc<[Self]>),
    Let(Arc<Self>, Arc<Self>, Arc<Self>),
    Match(Arc<Self>, Arc<[(Pat, Self)]>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat {
    Error,
    Lit(Lit),
    Name,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Head {
    Local(VarLevel),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Lit {
    Bool(bool),
}

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarIndex(u32);

#[derive(Debug, Copy, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub struct VarLevel(u32);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct UniqueEnv<T> {
    entries: Vec<T>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SharedEnv<T> {
    entries: Vec<T>,
}
