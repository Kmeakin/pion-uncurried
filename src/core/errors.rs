use codespan_reporting::diagnostic::{Diagnostic, Label};
use text_size::TextRange;

use super::unify::{RenameError, SpineError, UnifyError};
use super::MetaSource;
use crate::{FileId, RcStr};

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElabError {
    UnboundName {
        file: FileId,
        range: TextRange,
        name: RcStr,
    },
    CallNonFun {
        file: FileId,
        range: TextRange,
        fun_type: RcStr,
    },
    ArityMismatch {
        file: FileId,
        range: TextRange,
        fun_type: RcStr,
        expected_arity: usize,
        actual_arity: usize,
    },
    TypeMismatch {
        file: FileId,
        range: TextRange,
        expected_type: RcStr,
        actual_type: RcStr,
        error: UnifyError,
    },
    UnsolvedMeta {
        source: MetaSource,
    },
}

impl ElabError {
    pub fn to_diagnostic(&self) -> Diagnostic<FileId> {
        match self {
            Self::UnboundName { file, range, name } => Diagnostic::error()
                .with_message(format!("Unbound name `{name}`"))
                .with_labels(vec![Label::primary(*file, *range)]),
            Self::CallNonFun {
                file,
                range,
                fun_type,
            } => Diagnostic::error()
                .with_message("Called non-function expression")
                .with_labels(vec![Label::primary(*file, *range)])
                .with_notes(vec![format!(
                    "Help: type of this expression is `{fun_type}`"
                )]),
            Self::ArityMismatch {
                file,
                range,
                fun_type,
                expected_arity,
                actual_arity,
            } => Diagnostic::error()
                .with_message(format!(
                    "Function expects {expected_arity} arguments but {actual_arity} arguments \
                     were supplied"
                ))
                .with_labels(vec![Label::primary(*file, *range)])
                .with_notes(vec![format!(
                    "Help: type of this expression is `{fun_type}`"
                )]),
            Self::TypeMismatch {
                file,
                range,
                expected_type,
                actual_type,
                error,
            } => match error {
                UnifyError::Mismatch => Diagnostic::error()
                    .with_message("type mismatch")
                    .with_labels(vec![Label::primary(*file, *range)])
                    .with_notes(vec![format!(
                        "\
Help: expected `{expected_type}`
      got      `{actual_type}`"
                    )]),
                UnifyError::Spine(error) => match error {
                    SpineError::NonLinearSpine(_) => Diagnostic::error()
                        .with_message(
                            "Unification error: variable appeared more than once in problem spine",
                        )
                        .with_labels(vec![Label::primary(*file, *range)]),
                    SpineError::NonRigidSpine => Diagnostic::error()
                        .with_message("Unification error: meta variable found in problem spine")
                        .with_labels(vec![Label::primary(*file, *range)]),
                },
                UnifyError::Rename(error) => match error {
                    RenameError::EscapingLocalVar(_) => Diagnostic::error()
                        .with_message("Unification error: local variable escapes solution")
                        .with_labels(vec![Label::primary(*file, *range)]),
                    RenameError::InfiniteSolution => Diagnostic::error()
                        .with_message("Unification error: attempted to construct infinite solution")
                        .with_labels(vec![Label::primary(*file, *range)]),
                },
            },
            Self::UnsolvedMeta { source } => {
                let (file, range, name) = match source {
                    MetaSource::PlaceholderExpr(file, range) => {
                        (file, range, "placeholder expression")
                    }
                    MetaSource::PatType(file, range) => (file, range, "type of pattern"),
                    MetaSource::PlaceholderType(..) | MetaSource::Error => unreachable!(),
                };
                Diagnostic::error()
                    .with_message(format!("Unable to infer {name}"))
                    .with_labels(vec![Label::primary(*file, *range)])
            }
        }
    }
}
