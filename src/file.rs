use crate::symbol::Symbol;

#[salsa::input]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct File {
    pub name: Symbol,

    #[return_ref]
    pub contents: String,
}
