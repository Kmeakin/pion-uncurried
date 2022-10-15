use crate::file::File;

pub type Span = text_size::TextRange;
pub type Offset = text_size::TextSize;

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub struct FileSpan {
    pub file: File,
    pub span: Span,
}

impl PartialOrd for FileSpan {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> { Some(self.cmp(other)) }
}

impl Ord for FileSpan {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        let file = self.file.cmp(&other.file);
        let start = self.span.start().cmp(&other.span.start());
        let end = self.span.end().cmp(&other.span.end());
        file.then(start).then(end)
    }
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
