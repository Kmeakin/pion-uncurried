#![allow(clippy::use_self)]

use lalrpop_util::lalrpop_mod;

use super::errors::{ParseError, ParseErrors};
use super::syntax::*;

pub mod lexer;

lalrpop_mod!(
    #[allow(clippy::all, clippy::nursery, unused_qualifications, dead_code)]
    grammar,
    "/surface/parser/grammar.rs"
);

#[salsa::tracked(return_ref)]
pub fn parse_module(db: &dyn crate::Db, source: SourceFile) -> Module {
    let text = source.text(db);
    let (tokens, errors) = lexer::lex(&text);

    for error in errors {
        ParseErrors::push(db, ParseError::from(error))
    }

    match grammar::ModuleParser::new().parse(db, tokens) {
        Ok(module) => module,
        Err(error) => {
            ParseErrors::push(db, ParseError::from(error));
            Module::new(db, vec![])
        }
    }
}
