use crate::file::File;

pub type Span = text_size::TextRange;
pub type Offset = text_size::TextSize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileSpan {
    pub file: File,
    pub span: Span,
}

impl ariadne::Span for FileSpan {
    type SourceId = File;
    fn source(&self) -> &Self::SourceId { &self.file }
    fn start(&self) -> usize { self.span.start().into() }
    fn end(&self) -> usize { self.span.start().into() }
}

pub trait IntoFileSpan {
    fn into_file_span(self, default_file: File) -> FileSpan;
}

impl IntoFileSpan for FileSpan {
    fn into_file_span(self, _default_file: File) -> FileSpan { self }
}

impl IntoFileSpan for Span {
    fn into_file_span(self, default_file: File) -> FileSpan {
        FileSpan {
            file: default_file,
            span: self,
        }
    }
}
