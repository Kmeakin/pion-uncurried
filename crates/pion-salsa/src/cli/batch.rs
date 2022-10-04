use crate::ir::input_file::InputFile;
use crate::ir::symbol::Symbol;
use crate::surface::pretty::PrettyCtx;

pub fn elab(path: String) {
    let db = crate::Database::default();
    let contents = std::fs::read_to_string(&path).unwrap();
    let name = Symbol::new(&db, path);
    let input_file = InputFile::new(&db, name, contents);

    let module_ir = crate::ir::lower_file(&db, input_file);
    let module_core = crate::core::elab::elab_module(&db, module_ir);
    let module_surface = crate::core::unelab::unelab_module(&db, &module_core);

    let pretty_ctx = PrettyCtx::new();
    let module_pretty = pretty_ctx.pretty_module(&module_surface);
    println!("{}", module_pretty.into_doc().pretty(80));
}
