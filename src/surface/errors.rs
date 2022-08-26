use codespan_reporting::diagnostic::{Diagnostic, Label};
use text_size::{TextRange, TextSize};

use super::lexer;
use crate::FileId;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct TooLong(pub usize);

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub enum LexError {
    UnknownChar(TextRange),
}

impl LexError {
    pub const fn range(&self) -> TextRange {
        match self {
            Self::UnknownChar(range) => *range,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ParseError {
    TooLong(TooLong),
    Lexer(LexError),
    InvalidToken(TextRange),
    UnrecognizedEOF(TextRange, Vec<String>),
    UnrecognizedToken(TextRange, &'static str, Vec<String>),
    ExtraToken(TextRange, &'static str),
}

impl ParseError {
    pub fn range(&self) -> TextRange {
        match self {
            Self::TooLong(_) => TextRange::new(TextSize::from(u32::MAX), TextSize::from(u32::MAX)),
            Self::Lexer(error, ..) => error.range(),
            Self::InvalidToken(range, ..)
            | Self::UnrecognizedEOF(range, _)
            | Self::UnrecognizedToken(range, ..)
            | Self::ExtraToken(range, _) => *range,
        }
    }
}

type LalrpopError<'src> = lalrpop_util::ParseError<TextSize, lexer::Token<'src>, LexError>;
type LalrpopRecovery<'src> = lalrpop_util::ErrorRecovery<TextSize, lexer::Token<'src>, LexError>;

impl From<TooLong> for ParseError {
    fn from(other: TooLong) -> Self { Self::TooLong(other) }
}

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

impl ParseError {
    pub fn to_diagnostic(&self, file: FileId) -> Diagnostic<FileId> {
        match self {
            ParseError::TooLong(TooLong(len)) => Diagnostic::error()
                .with_message(format!("Parse error: input is too long"))
                .with_notes(vec![format!(
                    "Help: input must be less than {} bytes long, but it is {} bytes",
                    u32::MAX,
                    len
                )]),
            ParseError::Lexer(error) => match error {
                LexError::UnknownChar(range) => Diagnostic::error()
                    .with_message("Parse error: unknown character")
                    .with_labels(vec![Label::primary(file, *range)]),
            },
            ParseError::InvalidToken(range) => Diagnostic::error()
                .with_message("Parse error: invalid token")
                .with_labels(vec![Label::primary(file, *range)]),
            ParseError::UnrecognizedEOF(range, expected) => Diagnostic::error()
                .with_message("Parse error: unexpected end of input")
                .with_labels(vec![Label::primary(file, *range)])
                .with_notes(vec![format!(
                    "Help: expected one of {}",
                    expected.join(", ")
                )]),
            ParseError::UnrecognizedToken(range, unexpected, expected) => Diagnostic::error()
                .with_message(format!("Parse error: unexpected token (`{unexpected}`)"))
                .with_labels(vec![Label::primary(file, *range)])
                .with_notes(vec![format!(
                    "Help: got {}, expected one of {}",
                    unexpected,
                    expected.join(", ")
                )]),
            ParseError::ExtraToken(range, unexpected) => Diagnostic::error()
                .with_message(format!("parse error: unexpected token (`{unexpected}`)"))
                .with_labels(vec![Label::primary(file, *range)]),
        }
    }
}
