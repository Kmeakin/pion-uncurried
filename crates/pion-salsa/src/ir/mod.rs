use self::input_file::InputFile;
use self::syntax::*;
use crate::ir::symbol::Symbol;
use crate::surface;
use crate::surface::parser::parse_input_file;

pub mod input_file;
pub mod span;
pub mod symbol;
pub mod syntax;

#[salsa::tracked]
pub fn lower_file(db: &dyn crate::Db, input_file: InputFile) -> Module {
    let (module, errors) = parse_input_file(db, input_file);
    let items = module
        .items
        .iter()
        .map(|item| match item {
            surface::syntax::Item::Let(surface) => {
                let name = Symbol::new(db, surface.name.clone());
                Item::Let(LetDef::new(db, name, input_file, surface.clone()))
            }
            surface::syntax::Item::Enum(surface) => {
                let name = Symbol::new(db, surface.name.clone());
                Item::Enum(EnumDef::new(db, name, input_file, surface.clone()))
            }
        })
        .collect();
    assert_eq!(errors, &[]);
    Module::new(db, items)
}
