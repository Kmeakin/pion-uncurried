use std::rc::Rc;

use lalrpop_util::lalrpop_mod;
use text_size::TextRange;

use self::errors::ParseError;
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

impl Module<TextRange> {
    pub fn parse(src: &str) -> (Self, Vec<ParseError>) {
        let mut errors = Vec::new();
        let tokens = match lexer::lex(src) {
            Err(error) => {
                let error = ParseError::from(error);
                errors.push(error);
                let module = Self {
                    decls: Rc::from([]),
                };
                return (module, errors);
            }
            Ok(tokens) => tokens,
        };

        let module = grammar::ModuleParser::new()
            .parse(&mut errors, tokens)
            .unwrap_or_else(|error| {
                let error = ParseError::from(error);
                errors.push(error);
                Self {
                    decls: Rc::from([]),
                }
            });

        (module, errors)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Decl<Range> {
    Error(Range),
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
    Bool(Range, bool),
    FunType(Range, Rc<[Pat<Range>]>, Rc<Self>),
    FunExpr(Range, Rc<[Pat<Range>]>, Rc<Self>),
    FunCall(Range, Rc<Self>, Rc<[Self]>),
    Let(Range, Rc<Pat<Range>>, Rc<Self>, Rc<Self>),
    Ann(Range, Rc<Self>, Rc<Self>),
}

impl Expr<TextRange> {
    pub fn parse(src: &str) -> (Self, Vec<ParseError>) {
        let mut errors = Vec::new();
        let tokens = match lexer::lex(src) {
            Err(error) => {
                let error = ParseError::from(error);
                let range = error.range();
                errors.push(error);
                let expr = Self::Error(range);
                return (expr, errors);
            }
            Ok(tokens) => tokens,
        };

        let expr = grammar::ExprParser::new()
            .parse(&mut errors, tokens)
            .unwrap_or_else(|error| {
                let error = ParseError::from(error);
                let range = error.range();
                errors.push(error);
                Self::Error(range)
            });

        (expr, errors)
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat<Range> {
    Error(Range),
    Wildcard(Range),
    Name(Range, RcStr),
    Ann(Range, Rc<Self>, Rc<Expr<Range>>),
}

impl<Range> Pat<Range> {
    pub fn name(&self) -> Option<RcStr> {
        match self {
            Pat::Error(_) => None,
            Pat::Wildcard(_) => None,
            Pat::Name(_, name) => Some(name.clone()),
            Pat::Ann(_, pat, _) => pat.name(),
        }
    }
}
