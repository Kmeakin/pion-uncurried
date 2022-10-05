use super::input_file::InputFile;
use super::span::{FileSpan, Span};

/// Used as the "error" value for a `Result` to indicate that an error was
/// detected and reported to the user (i.e., pushed onto the [`Diagnostics`]
/// accumulator).
#[derive(Debug)]
pub struct ErrorReported;

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Diagnostic {
    pub severity: Severity,
    pub span: FileSpan,
    pub message: String,
    pub labels: Vec<Label>,
    pub children: Vec<Diagnostic>,
}

#[derive(Copy, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Debug)]
pub enum Severity {
    Help,
    Note,
    Warning,
    Error,
}

#[derive(Clone, PartialEq, Eq, Hash, Debug)]
pub struct Label {
    pub span: FileSpan,
    pub message: String,
}

#[salsa::accumulator]
pub struct Diagnostics(Diagnostic);

/// Convenience macro for avoiding `format!`
#[macro_export]
macro_rules! diag {
    ($severity:expr, $span:expr, $($message:tt)*) => {
        $crate::ir::diagnostic::Diagnostic::builder($severity, $span, format!($($message)*))
    }
}

/// Convenience macro for avoiding `format!`
#[macro_export]
macro_rules! error {
    ($span:expr, $($message:tt)*) => {
        $crate::ir::diagnostic::Diagnostic::builder($crate::ir::diagnostic::Severity::Error, $span, format!($($message)*))
    }
}

/// Convenience macro for avoiding `format!`
#[macro_export]
macro_rules! warning {
    ($span:expr, $($message:tt)*) => {
        $crate::ir::diagnostic::Diagnostic::builder($crate::ir::diagnostic::Severity::Warning, $span, format!($($message)*))
    }
}

/// Convenience macro for avoiding `format!`
#[macro_export]
macro_rules! note {
    ($span:expr, $($message:tt)*) => {
        $crate::ir::diagnostic::Diagnostic::builder($crate::ir::diagnostic::Severity::Note, $span, format!($($message)*))
    }
}

/// Convenience macro for avoiding `format!`
#[macro_export]
macro_rules! help {
    ($span:expr, $($message:tt)*) => {
        $crate::ir::diagnostic::Diagnostic::builder($crate::ir::diagnostic::Severity::Help, $span, format!($($message)*))
    }
}

impl Diagnostic {
    /// Create a new diagnostic builder with the given "main message" at the
    /// given span.
    pub fn builder(severity: Severity, span: FileSpan, message: String) -> DiagnosticBuilder {
        DiagnosticBuilder::new(severity, span, message)
    }

    /// Emit the diagnostic to the [`Diagnostics`] accumulator.
    /// You can fetch the diagnostics produced by a query (and its
    /// dependencies) by invoking `query::accumulated::<Diagnostics>(..)`.
    pub fn emit(self, db: &dyn crate::Db) -> ErrorReported {
        Diagnostics::push(db, self);
        ErrorReported
    }
}

impl Label {
    pub fn span(&self) -> FileSpan { self.span }

    pub fn message(&self) -> &String { &self.message }
}

pub struct DiagnosticBuilder {
    severity: Severity,
    span: FileSpan,
    message: String,

    /// All labels added by user so far (primary or secondary).
    labels: Vec<Label>,
    children: Vec<Diagnostic>,

    /// Initially true. Indicates if we should add a default primary
    /// label ("here") when the diagnostic is emitted. Set to false
    /// if user adds an explicit primary label or calls
    /// [`Self::skip_primary_label`].
    add_primary_label: bool,
}

impl DiagnosticBuilder {
    fn new(severity: Severity, span: FileSpan, message: impl ToString) -> Self {
        Self {
            severity,
            span,
            message: message.to_string(),
            labels: vec![],
            children: vec![],
            add_primary_label: true,
        }
    }

    /// Replaces the "primary label", which is always placed on the source
    /// of the diagnostic. The default primary label, if nothing else is given,
    /// is just "here".
    #[must_use = "you have not emitted the diagnostic (call `.emit()`)"]
    pub fn with_primary_label(mut self, message: impl ToString) -> Self {
        self.labels.push(Label {
            span: self.span,
            message: message.to_string(),
        });
        self.skip_primary_label()
    }

    /// Avoids adding any primary label at all.
    #[must_use = "you have not emitted the diagnostic (call `.emit()`)"]
    pub fn skip_primary_label(mut self) -> Self {
        self.add_primary_label = false;
        self
    }

    /// Add a "secondary" label to this diagnostic; secondary labels
    /// give auxiliary information and can be located at any span.
    /// (if you supply a [`Span`] and not a [`FileSpan`], the [`Span`]
    /// is assumed to be in the same file as the primary location).
    #[must_use = "you have not emitted the diagnostic (call `.emit()`)"]
    pub fn with_secondary_label(mut self, span: impl IntoFileSpan, message: impl ToString) -> Self {
        let span = span.into_file_span(self.span.file);
        self.labels.push(Label {
            span,
            message: message.to_string(),
        });
        self
    }

    /// Add a child diagnostic. Our severity is raised to at least
    /// the child's level.
    #[must_use = "you have not emitted the diagnostic (call `.emit()`)"]
    pub fn with_child(mut self, diagnostic: Diagnostic) -> Self {
        // Raise our severity to the child's level. Note sure if this
        // is important, it just seems weird to have a "note" with
        // an "error" child.
        self.severity = self.severity.max(diagnostic.severity);

        self.children.push(diagnostic);
        self
    }

    /// Return the completed diagnostic.
    pub fn finish(mut self) -> Diagnostic {
        if self.add_primary_label {
            self = self.with_primary_label("here");
        }

        Diagnostic {
            severity: self.severity,
            span: self.span,
            message: self.message,
            labels: self.labels,
            children: self.children,
        }
    }

    /// Finish and emit the diagnostic.
    pub fn emit(self, db: &dyn crate::Db) -> ErrorReported { self.finish().emit(db) }
}

pub trait IntoFileSpan {
    fn into_file_span(self, default_file: InputFile) -> FileSpan;
}

impl IntoFileSpan for FileSpan {
    fn into_file_span(self, _default_file: InputFile) -> FileSpan { self }
}

impl IntoFileSpan for Span {
    fn into_file_span(self, default_file: InputFile) -> FileSpan {
        FileSpan {
            file: default_file,
            span: self,
        }
    }
}
