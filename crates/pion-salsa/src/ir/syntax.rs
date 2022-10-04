use super::input_file::InputFile;
use super::span::Span;
use super::symbol::Symbol;
use crate::surface::syntax as surface;

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Let(LetDef),
    Enum(EnumDef),
}

#[salsa::tracked]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LetDef {
    pub name: Symbol,

    pub file: InputFile,

    #[return_ref]
    pub surface: surface::LetDef<Span>,
}

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: Symbol,

    pub file: InputFile,

    #[return_ref]
    pub surface: surface::EnumDef<Span>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: Symbol,
}
