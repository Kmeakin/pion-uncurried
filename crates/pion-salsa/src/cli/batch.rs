use crate::ir::input_file::InputFile;
use crate::ir::symbol::Symbol;

pub fn elab(path: String) {
    let db = crate::Database::default();
    let contents = std::fs::read_to_string(&path).unwrap();
    let name = Symbol::new(&db, path);
    let input_file = InputFile::new(&db, name, contents);

    let module = crate::ir::lower_file(&db, input_file);

    for item in module.items(&db) {
        match item {
            crate::ir::syntax::Item::Enum(enum_def) => todo!(),
            crate::ir::syntax::Item::Let(let_def) => {
                let let_def = crate::core::elab::elab_let_def(&db, let_def);
            }
        }
    }
}
