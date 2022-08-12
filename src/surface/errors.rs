use text_size::TextRange;

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
