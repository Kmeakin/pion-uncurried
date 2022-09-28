use super::symbol::Symbol;

#[salsa::input]
#[derive(Copy, Clone, PartialEq, Eq, Hash)]
pub struct InputFile {
    pub name: Symbol,

    #[return_ref]
    pub contents: String,
}
