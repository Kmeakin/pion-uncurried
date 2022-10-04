use std::path::Path;

use crate::ir::input_file::InputFile;
use crate::ir::symbol::Symbol;

#[datatest::files("test-data/elab/ok", {input_path in r"^(.*).pion", expected_path = "${1}.expected"})]
fn test_ok(input_path: &Path, expected_path: &Path) {
    let input_path = input_path.to_str().unwrap();
    let db = crate::Database::default();
    let name = Symbol::new(&db, input_path.to_owned());
    let contents = std::fs::read_to_string(input_path).unwrap();
    let input_file = InputFile::new(&db, name, contents);

    let module_ir = crate::ir::lower_file(&db, input_file);
    let module_core = crate::core::elab::elab_module(&db, module_ir);
    let module_surface = crate::core::unelab::unelab_module(&db, &module_core);

    let pretty_ctx = crate::surface::pretty::PrettyCtx::new();
    let module_pretty = pretty_ctx.pretty_module(&module_surface);

    let output = format!("{}", module_pretty.into_doc().pretty(80));
    let expect = expect_test::expect_file![format!(
        "{}/{}",
        env!("CARGO_MANIFEST_DIR"),
        expected_path.display()
    )];
    expect.assert_eq(&output)
}
