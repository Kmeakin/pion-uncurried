use crate::diagnostic::Diagnostics;
use crate::file::File;
use crate::surface::pretty::PrettyCtx;
use crate::symbol::Symbol;

pub fn elab(path: String) {
    let db = crate::Database::default();
    let contents = std::fs::read_to_string(&path).unwrap();
    let name = Symbol::new(&db, path);
    let file = File::new(&db, name, contents);

    let module_ir = crate::ir::lower_file(&db, file);
    let module_core = crate::core::elab::elab_module(&db, module_ir);
    let module_surface = crate::core::unelab::unelab_module(&db, &module_core);

    let pretty_ctx = PrettyCtx::new();
    let module_pretty = pretty_ctx.pretty_module(&module_surface);
    println!("{}", module_pretty.into_doc().pretty(80));

    let lower_diagnostics = crate::ir::lower_file::accumulated::<Diagnostics>(&db, file);
    let elab_diagnostics =
        crate::core::elab::elab_module::accumulated::<Diagnostics>(&db, module_ir);

    for diag in lower_diagnostics.iter().chain(elab_diagnostics.iter()) {
        let mut builder = ariadne::Report::build(
            ariadne::ReportKind::Error,
            diag.file_span.file,
            diag.file_span.span.start().into(),
        )
        .with_message(&diag.message)
        .with_config(ariadne::Config::default().with_color(true));

        for label in &diag.labels {
            builder =
                builder.with_label(ariadne::Label::new(label.span).with_message(&label.message));
        }

        let report = builder.finish();
        report
            .eprint(crate::diagnostic::SourceCache::new(&db))
            .expect("Cannot print diagnostic");
    }
}
