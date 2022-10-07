use super::input_file::InputFile;

pub type Span = text_size::TextRange;
pub type Offset = text_size::TextSize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileSpan {
    pub file: InputFile,
    pub span: Span,
}

impl ariadne::Span for FileSpan {
    type SourceId = InputFile;
    fn source(&self) -> &Self::SourceId { &self.file }
    fn start(&self) -> usize { self.span.start().into() }
    fn end(&self) -> usize { self.span.start().into() }
}
