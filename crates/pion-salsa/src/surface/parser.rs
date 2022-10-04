#![allow(clippy::use_self)]

use lalrpop_util::lalrpop_mod;

use super::errors::ParseError;
use super::syntax::*;
use crate::ir::input_file::InputFile;
use crate::ir::span::Span;

pub mod lexer;

lalrpop_mod!(
    #[allow(clippy::all, clippy::nursery, unused_qualifications, dead_code)]
    grammar,
    "/surface/parser/grammar.rs"
);

#[salsa::tracked(return_ref)]
pub fn parse_input_file(
    db: &dyn crate::Db,
    input_file: InputFile,
) -> (Module<Span>, Vec<ParseError>) {
    let contents = input_file.contents(db);
    let (tokens, errors) = lexer::lex(contents);
    let mut errors: Vec<ParseError> = errors
        .iter()
        .map(|error| ParseError::from(*error))
        .collect();

    let module = match grammar::ModuleParser::new().parse(tokens) {
        Ok(module) => module,
        Err(error) => {
            errors.push(error.into());
            Module { items: vec![] }
        }
    };

    (module, errors)
}
