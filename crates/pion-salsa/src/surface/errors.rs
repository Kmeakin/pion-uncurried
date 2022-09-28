use text_size::{TextRange, TextSize};

use super::parser::lexer;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LexError {
    TooLong(usize),
    UnknownChar(TextRange),
    UnclosedBlockComment(TextRange),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    Lexer(LexError),
    InvalidToken(TextRange),
    UnrecognizedEOF(TextRange, Vec<String>),
    UnrecognizedToken(TextRange, &'static str, Vec<String>),
    ExtraToken(TextRange, &'static str),
}

impl From<LexError> for ParseError {
    fn from(v: LexError) -> Self { Self::Lexer(v) }
}

type LalrpopError<'src> = lalrpop_util::ParseError<TextSize, lexer::Token<'src>, LexError>;
type LalrpopRecovery<'src> = lalrpop_util::ErrorRecovery<TextSize, lexer::Token<'src>, LexError>;

impl<'src> From<LalrpopError<'src>> for ParseError {
    fn from(other: LalrpopError) -> Self {
        match other {
            LalrpopError::InvalidToken { location } => {
                Self::InvalidToken(TextRange::new(location, location))
            }
            LalrpopError::UnrecognizedEOF { location, expected } => {
                Self::UnrecognizedEOF(TextRange::new(location, location), expected)
            }
            LalrpopError::UnrecognizedToken {
                token: (start, token, end),
                expected,
            } => Self::UnrecognizedToken(TextRange::new(start, end), token.description(), expected),
            LalrpopError::ExtraToken {
                token: (start, token, end),
            } => Self::ExtraToken(TextRange::new(start, end), token.description()),
            LalrpopError::User { error } => Self::Lexer(error),
        }
    }
}

impl<'src> From<LalrpopRecovery<'src>> for ParseError {
    fn from(other: LalrpopRecovery<'src>) -> Self { Self::from(other.error) }
}
