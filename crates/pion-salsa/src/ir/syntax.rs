#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Module {
    pub items: Vec<Item>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Item {
    Let(LetDef),
    Enum(EnumDef),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LetDef {
    pub name: String,

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumDef {
    pub name: String,

}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EnumVariant {
    pub name: String,
}