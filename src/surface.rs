use std::rc::Rc;

use lalrpop_util::lalrpop_mod;
use text_size::TextRange;

use self::errors::ParseError;
use crate::RcStr;

pub mod errors;
pub mod lexer;
pub mod pretty;
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
    pub name: (Range, Option<RcStr>),
    pub ty: Option<Rc<Expr<Range>>>,
    pub expr: Rc<Expr<Range>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Expr<Range> {
    Error(Range),
    Placeholder(Range),
    Name(Range, RcStr),
    Bool(Range, bool),
    FunType(Range, Rc<[SimplePat<Range>]>, Rc<Self>),
    FunExpr(Range, Rc<[SimplePat<Range>]>, Rc<Self>),
    FunCall(Range, Rc<Self>, Rc<[Self]>),
    Match(Range, Rc<Self>, Rc<[(Pat<Range>, Self)]>),
    Let(Range, Rc<SimplePat<Range>>, Rc<Self>, Rc<Self>),
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

    pub fn range(&self) -> TextRange {
        match self {
            Self::Error(range, ..)
            | Self::Placeholder(range, ..)
            | Self::Name(range, ..)
            | Self::Bool(range, ..)
            | Self::FunType(range, ..)
            | Self::FunExpr(range, ..)
            | Self::FunCall(range, ..)
            | Self::Match(range, ..)
            | Self::Let(range, ..)
            | Self::Ann(range, ..) => *range,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SimplePat<Range> {
    pub name: (Range, Option<RcStr>),
    pub ty: Option<Rc<Expr<Range>>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Pat<Range> {
    Error(Range),
    Wildcard(Range),
    Name(Range, RcStr),
    Bool(Range, bool),
    Ann(Range, Rc<Self>, Rc<Expr<Range>>),
}

impl Pat<TextRange> {
    pub fn range(&self) -> TextRange {
        match self {
            Self::Error(range, ..)
            | Self::Wildcard(range, ..)
            | Self::Name(range, ..)
            | Self::Bool(range, ..)
            | Self::Ann(range, ..) => *range,
        }
    }
}
