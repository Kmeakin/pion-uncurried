use std::io::Write;
use std::path::Path;

use pion_salsa::ir::diagnostic::Diagnostics;
use walkdir::WalkDir;

fn main() {
    let args = libtest_mimic::Arguments::from_args();
    let tests = find_source_files("tests/elab").map(elab_ok).collect();
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
        let file = pion_salsa::ir::input_file::InputFile::new(&db, name, contents);

        let module_ir = pion_salsa::ir::lower_file(&db, file);
        let module_core = pion_salsa::core::elab::elab_module(&db, module_ir);
        let module_surface = pion_salsa::core::unelab::unelab_module(&db, &module_core);

        let pretty_ctx = pion_salsa::surface::pretty::PrettyCtx::new();
        let module_pretty = pretty_ctx.pretty_module(&module_surface);

        let output = format!("{}\n", module_pretty.into_doc().pretty(80));

        let lower_diagnostics = pion_salsa::ir::lower_file::accumulated::<Diagnostics>(&db, file);
        let elab_diagnostics =
            pion_salsa::core::elab::elab_module::accumulated::<Diagnostics>(&db, module_ir);

        let mut output: Vec<u8> = output.into();
        if !lower_diagnostics.is_empty() || !elab_diagnostics.is_empty() {
            writeln!(output, "\n====DIAGNOSTICS====").unwrap();
        }

        for diag in lower_diagnostics.iter().chain(elab_diagnostics.iter()) {
            let mut builder = ariadne::Report::build(
                ariadne::ReportKind::Error,
                diag.file_span.file,
                diag.file_span.span.start().into(),
            )
            .with_message(&diag.message)
            .with_config(ariadne::Config::default().with_color(false));

            for label in &diag.labels {
                builder = builder
                    .with_label(ariadne::Label::new(label.span).with_message(&label.message));
            }

            let report = builder.finish();
            report
                .write(
                    pion_salsa::ir::diagnostic::SourceCache::new(&db),
                    &mut output,
                )
                .expect("Cannot print diagnostic");
        }
        let output = String::from_utf8(output).unwrap();

        let root = env!("CARGO_MANIFEST_DIR");
        let expect = expect_test::expect_file![format!("{root}/{path}.expected")];
        expect.assert_eq(&output);
        Ok(())
    })
}
