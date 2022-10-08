use crate::file::File;
use crate::span::Span;
use crate::surface::syntax as surface;
use crate::symbol::Symbol;

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub file: File,
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Let(LetDef),
    Enum(EnumDef),
    Variant(EnumVariant),
}

#[salsa::tracked]
#[derive(Debug, Copy, Clone, PartialEq, Eq)]
pub struct LetDef {
    pub name: Symbol,

    pub file: File,

    #[return_ref]
    pub surface: surface::LetDef<Span>,
}

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: Symbol,

    pub file: File,

    #[return_ref]
    pub surface: surface::EnumDef<Span>,
}

#[salsa::tracked]
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: Symbol,

    pub file: File,

    pub parent: EnumDef,

    #[return_ref]
    pub surface: surface::EnumVariant<Span>,
}
