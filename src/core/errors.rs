use text_size::TextRange;

use crate::RcStr;

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Error {
    UnboundName { range: TextRange, name: RcStr },
}
