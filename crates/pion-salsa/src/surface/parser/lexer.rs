use std::convert::TryInto;

use logos::{Lexer, Logos};
use text_size::{TextRange, TextSize};

use crate::surface::errors::LexError;

#[derive(Debug, Copy, Clone, PartialEq, Eq)]
#[derive(Logos)]
#[logos(extras = Vec<LexError>)]
#[rustfmt::skip]
pub enum Token<'src> {
    #[error]                                        Error,
    #[regex(r"\s+",      logos::skip)]              Whitespace,
    #[regex(r"//[^\n]*", logos::skip)]              LineComment,
    #[token(r"/*",       block_comment)]            BlockComment,

    #[token("_")]                                   Underscore,
    #[regex(r"[a-zA-Z_][a-zA-Z0-9_-]*")]            Name(&'src str),
    #[regex(r"\?[a-zA-Z_][a-zA-Z0-9_-]*")]          Hole(&'src str),

    #[token("(")]                                   LParen,
    #[token(")")]                                   RParen,
    #[token("{")]                                   LCurly,
    #[token("}")]                                   RCurly,

    #[token("->")] #[token("→")]                    SingleArrow,
    #[token("=>")] #[token("⇒")]                    DoubleArrow,
    #[token("=")]                                   Eq,
    #[token(":")]                                   Colon,
    #[token(";")]                                   Semicolon,
    #[token(",")]                                   Comma,

    #[token("enum")]                                KwEnum,
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

    let span = lexer.span();
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
        let start = span.start.try_into().unwrap();
        let end = span.start.try_into().unwrap();
        lexer
            .extras
            .push(LexError::UnclosedBlockComment(TextRange::new(start, end)));
    }
    logos::Skip
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
            Self::Hole(_) => "hole",
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
            Self::KwEnum => "`enum`",
            Self::KwFalse => "`false`",
            Self::KwFn => "`fn`",
            Self::KwIn => "`in`",
            Self::KwLet => "`let`",
            Self::KwMatch => "`match`",
            Self::KwTrue => "`true`",
        }
    }
}

pub fn lex(src: &str) -> (Vec<(TextSize, Token, TextSize)>, Vec<LexError>) {
    if u32::try_from(src.len()).is_err() {
        (Vec::new(), vec![LexError::TooLong(src.len())])
    } else {
        let mut lexer = Lexer::new(src);
        let mut tokens = Vec::new();
        while let Some(token) = lexer.next() {
            let range = lexer.span();
            let start = range.start.try_into().unwrap();
            let end = range.end.try_into().unwrap();
            match token {
                Token::Error => lexer
                    .extras
                    .push(LexError::UnknownChar(TextRange::new(start, end))),
                token => tokens.push((start, token, end)),
            }
        }
        (tokens, lexer.extras)
    }
}
