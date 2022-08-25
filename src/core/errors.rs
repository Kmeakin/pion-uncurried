use text_size::TextRange;

use super::unify::UnifyError;
use super::MetaSource;
use crate::RcStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElabError {
    UnboundName {
        range: TextRange,
        name: RcStr,
    },
    CallNonFun {
        range: TextRange,
        fun_type: RcStr,
    },
    ArityMismatch {
        range: TextRange,
        fun_type: RcStr,
        expected_arity: usize,
        actual_arity: usize,
    },
    TypeMismatch {
        range: TextRange,
        expected_type: RcStr,
        actual_type: RcStr,
        error: UnifyError,
    },
    UnsolvedMeta {
        source: MetaSource,
    },
}
