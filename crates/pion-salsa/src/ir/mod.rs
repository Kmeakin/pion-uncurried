use self::input_file::InputFile;
use self::syntax::*;
use crate::ir::symbol::Symbol;
use crate::surface;
use crate::surface::parser::parse_input_file;

pub mod diagnostic;
pub mod input_file;
pub mod span;
pub mod symbol;
pub mod syntax;

#[salsa::tracked]
pub fn lower_file(db: &dyn crate::Db, file: InputFile) -> Module {
    let module = parse_input_file(db, file);
    let items = module
        .items
        .iter()
        .map(|item| match item {
            surface::syntax::Item::Let(surface) => {
                let name = Symbol::new(db, surface.name.clone());
                Item::Let(LetDef::new(db, name, file, surface.clone()))
            }
            surface::syntax::Item::Enum(surface) => {
                let name = Symbol::new(db, surface.name.clone());
                Item::Enum(EnumDef::new(db, name, file, surface.clone()))
            }
        })
        .collect();
    Module::new(db, items)
}

#[salsa::tracked]
pub fn lookup_item(db: &dyn crate::Db, file: InputFile, name: Symbol) -> Option<Item> {
    let module = lower_file(db, file);
    let items = module.items(db);
    for item in items {
        match item {
            crate::ir::Item::Enum(_) => todo!(),
            crate::ir::Item::Let(def) if def.name(db) == name => return Some(item),
            _ => {}
        }
    }
    None
}
