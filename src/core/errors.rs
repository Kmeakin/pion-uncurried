use std::rc::Rc;

use text_size::TextRange;

use super::Value;
use crate::RcStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
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
    },
}
