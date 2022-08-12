use std::rc::Rc;

use lalrpop_util::lalrpop_mod;

use crate::RcStr;

pub mod errors;
pub mod lexer;
lalrpop_mod!(
    #[allow(clippy::all, clippy::nursery, unused_qualifications, dead_code)]
    grammar
);

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module<Range> {
    pub decls: Rc<[Decl<Range>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl<Range> {
    Let(Range, LetDecl<Range>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDecl<Range> {
    pub pat: Pat<Range>,
    pub expr: Expr<Range>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<Range> {
    Error(Range),
    Name(Range, RcStr),
    FunType(Range, Rc<[Pat<Range>]>, Rc<Self>),
    FunExpr(Range, Rc<[Pat<Range>]>, Rc<Self>),
    FunCall(Range, Rc<Self>, Rc<[Self]>),
    Let(Range, Rc<Pat<Range>>, Rc<Self>, Rc<Self>),
    Ann(Range, Rc<Self>, Rc<Self>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat<Range> {
    Error(Range),
    Wildcard(Range),
    Name(Range, RcStr),
    Ann(Range, Rc<Self>, Rc<Expr<Range>>),
}
