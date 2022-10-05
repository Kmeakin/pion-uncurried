use super::input_file::InputFile;

pub type Span = text_size::TextRange;
pub type Offset = text_size::TextSize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileSpan {
    pub file: InputFile,
    pub span: Span,
}
