use std::rc::Rc;

use text_size::TextRange;

use super::unify::UnifyError;
use super::{MetaSource, Value};
use crate::RcStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ElabError {
    UnboundName {
        range: TextRange,
        name: RcStr,
    },
    CallNonFun {
        range: TextRange,
        fun_type: Rc<Value>,
    },
    ArityMismatch {
        range: TextRange,
        fun_type: Rc<Value>,
        expected_arity: usize,
        actual_arity: usize,
    },
    TypeMismatch {
        range: TextRange,
        expected_type: Rc<Value>,
        actual_type: Rc<Value>,
        error: UnifyError,
    },
    UnsolvedMeta {
        source: MetaSource,
    },
}
