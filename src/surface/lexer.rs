use logos::{Lexer, Logos};
use text_size::{TextRange, TextSize};

use super::errors::{LexError, TooLong};

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[derive(Logos)]
#[rustfmt::skip]
pub enum Token<'src> {
    #[error]                                        Error,
    #[regex(r"\s+",      logos::skip)]              Whitespace,
    #[regex(r"//[^\n]*", logos::skip)]              LineComment,
    #[token(r"/*",       block_comment)]            BlockComment,

    #[token("_")]                                   Underscore,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*")]            Name(&'src str),

    #[token("(")]                                   LParen,
    #[token(")")]                                   RParen,
    #[token("{")]                                   LCurly,
    #[token("}")]                                   RCurly,

    #[token("->")] #[token("→")]                    SingleArrow,
    #[token("=>")] #[token("⇒")]                   DoubleArrow,
    #[token("=")]                                   Eq,
    #[token(":")]                                   Colon,
    #[token(";")]                                   Semicolon,
    #[token(",")]                                   Comma,

    #[token("false")]                               KwFalse,
    #[token("fn")]                                  KwFn,
    #[token("in")]                                  KwIn,
    #[token("let")]                                 KwLet,
    #[token("match")]                               KwMatch,
    #[token("true")]                                KwTrue,
}

fn block_comment<'src>(lexer: &mut Lexer<'src, Token<'src>>) -> logos::Skip {
    const OPEN: &str = "/*";
    const CLOSE: &str = "*/";

    let mut depth: u32 = 1;
    while let Some(c) = lexer.remainder().chars().next() {
        if lexer.remainder().starts_with(OPEN) {
            lexer.bump(OPEN.len());
            depth += 1;
        } else if lexer.remainder().starts_with(CLOSE) {
            lexer.bump(CLOSE.len());
            depth -= 1;
            if depth == 0 {
                break;
            }
        } else {
            lexer.bump(c.len_utf8());
        }
    }

    if depth > 0 {
        todo!("report error")
    }
    logos::Skip
}

pub fn lex(
    src: &str,
) -> Result<impl Iterator<Item = Result<(TextSize, Token, TextSize), LexError>>, TooLong> {
    if u32::try_from(src.len()).is_err() {
        Err(TooLong(src.len()))
    } else {
        Ok(Lexer::new(src).spanned().map(|(token, range)| {
            let start = range.start.try_into().unwrap();
            let end = range.end.try_into().unwrap();
            match token {
                Token::Error => Err(LexError::UnknownChar(TextRange::new(start, end))),
                token => Ok((start, token, end)),
            }
        }))
    }
}

impl<'src> Token<'src> {
    pub const fn description(&self) -> &'static str {
        match self {
            Self::Error => "error",
            Self::Whitespace => "whitespace",
            Self::LineComment => "line comment",
            Self::BlockComment => "block comment",
            Self::Underscore => "`_`",
            Self::Name(_) => "name",
            Self::LParen => "`(`",
            Self::RParen => "`)`",
            Self::LCurly => "`{`",
            Self::RCurly => "`}`",
            Self::SingleArrow => "`->`",
            Self::DoubleArrow => "`=>`",
            Self::Eq => "`=`",
            Self::Colon => "`:`",
            Self::Semicolon => "`;`",
            Self::Comma => "`,`",
            Self::KwFalse => "`false`",
            Self::KwFn => "`fn`",
            Self::KwIn => "`in`",
            Self::KwLet => "`let`",
            Self::KwMatch => "`match`",
            Self::KwTrue => "`true`",
        }
    }
}
