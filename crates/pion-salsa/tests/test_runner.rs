use std::path::Path;

use walkdir::WalkDir;

fn main() {
    let args = libtest_mimic::Arguments::from_args();
    let tests = find_source_files("tests/elab/ok").map(elab_ok).collect();
    libtest_mimic::run(&args, tests).exit();
}

/// Recursively walk over test files under a file path.
fn find_source_files(root: impl AsRef<Path>) -> impl Iterator<Item = String> {
    WalkDir::new(root)
        .into_iter()
        .filter_map(|entry| entry.ok())
        .filter(|entry| entry.file_type().is_file())
        .filter(|entry| matches!(entry.path().extension(), Some(ext) if ext == "pion"))
        .map(|entry| entry.into_path().to_str().unwrap().to_owned())
}

fn elab_ok(path: String) -> libtest_mimic::Trial {
    libtest_mimic::Trial::test(path.clone(), move || {
        let db = pion_salsa::Database::default();
        let name = pion_salsa::ir::symbol::Symbol::new(&db, path.to_owned());
        let contents = std::fs::read_to_string(&path).unwrap();
        let input_file = pion_salsa::ir::input_file::InputFile::new(&db, name, contents);

        let module_ir = pion_salsa::ir::lower_file(&db, input_file);
        let module_core = pion_salsa::core::elab::elab_module(&db, module_ir);
        let module_surface = pion_salsa::core::unelab::unelab_module(&db, &module_core);

        let pretty_ctx = pion_salsa::surface::pretty::PrettyCtx::new();
        let module_pretty = pretty_ctx.pretty_module(&module_surface);

        let output = format!("{}", module_pretty.into_doc().pretty(80));
        let root = env!("CARGO_MANIFEST_DIR");
        let expect = expect_test::expect_file![format!("{root}/{path}.expected")];
        expect.assert_eq(&output);
        Ok(())
    })
}
