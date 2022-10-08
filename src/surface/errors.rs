use text_size::{TextRange, TextSize};

use super::parser::lexer;
use crate::diagnostic::Diagnostic;
use crate::file::File;
use crate::span::{IntoFileSpan, Span};

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

impl ParseError {
    pub fn to_diagnostic(self, file: File) -> Diagnostic {
        match self {
            Self::Lexer(error) => match error {
                LexError::TooLong(len) => crate::error!(
                    Span::default().into_file_span(file),
                    "Parse error: input is too long"
                )
                .with_primary_label(format!(
                    "Help: input must be less than {} bytes long, but it is {} bytes",
                    u32::MAX,
                    len
                )),
                LexError::UnknownChar(span) => {
                    crate::error!(span.into_file_span(file), "Parse error: unknown character")
                }
                LexError::UnclosedBlockComment(span) => {
                    crate::error!(span.into_file_span(file), "Unclosed block comment")
                }
            },
            Self::InvalidToken(span) => {
                crate::error!(span.into_file_span(file), "Parse error: invalid token")
            }
            Self::UnrecognizedEOF(span, expected) => crate::error!(
                span.into_file_span(file),
                "Parse error: unexpected end of input"
            )
            .with_primary_label(format!("Help: expected on of {}", expected.join(", "))),
            Self::UnrecognizedToken(span, unexpected, expected) => {
                crate::error!(span.into_file_span(file), "Parse error: unexpected token")
                    .with_primary_label(format!(
                        "Help: got {}, expected one of {}",
                        unexpected,
                        expected.join(", ")
                    ))
            }
            Self::ExtraToken(span, unexpected) => {
                crate::error!(span.into_file_span(file), "Parse error: unexpected token")
                    .with_primary_label(format!("Help: got {}", unexpected,))
            }
        }
        .finish()
    }
}
