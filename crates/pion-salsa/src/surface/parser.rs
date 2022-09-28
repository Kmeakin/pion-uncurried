#![allow(clippy::use_self)]

use lalrpop_util::lalrpop_mod;

use super::syntax::*;
use crate::ir::span::Span;

pub mod lexer;

lalrpop_mod!(
    #[allow(clippy::all, clippy::nursery, unused_qualifications, dead_code)]
    grammar,
    "/surface/parser/grammar.rs"
);

pub fn parse_module(text: &str) -> Module<Span> {
    let (tokens, errors) = lexer::lex(&text);

    for error in errors {
        todo!()
    }

    match grammar::ModuleParser::new().parse(tokens) {
        Ok(module) => module,
        Err(error) => {
            todo!()
        }
    }
}
