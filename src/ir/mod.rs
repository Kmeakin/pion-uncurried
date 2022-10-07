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
    let mut items = Vec::with_capacity(module.items.len());
    for item in &module.items {
        match item {
            surface::syntax::Item::Let(surface) => {
                let name = Symbol::new(db, surface.name.clone());
                items.push(Item::Let(LetDef::new(db, name, file, surface.clone())));
            }
            surface::syntax::Item::Enum(surface) => {
                let name = Symbol::new(db, surface.name.clone());
                let parent = EnumDef::new(db, name, file, surface.clone());
                items.push(Item::Enum(parent));
                for surface in &surface.variants {
                    let name = Symbol::new(db, surface.name.clone());
                    items.push(Item::Variant(EnumVariant::new(
                        db,
                        name,
                        file,
                        parent,
                        surface.clone(),
                    )));
                }
            }
        }
    }

    Module::new(db, file, items)
}

#[salsa::tracked]
pub fn lookup_item(db: &dyn crate::Db, file: InputFile, name: Symbol) -> Option<Item> {
    let module = lower_file(db, file);
    let items = module.items(db);
    for item in items {
        let n = match item {
            crate::ir::Item::Enum(def) => def.name(db),
            crate::ir::Item::Let(def) => def.name(db),
            crate::ir::Item::Variant(def) => def.name(db),
        };
        if n == name {
            return Some(item);
        }
    }
    None
}
