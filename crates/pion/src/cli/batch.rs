use codespan_reporting::term::termcolor::ColorChoice;

use crate::core::elab::ElabCtx;
use crate::surface;

pub fn elab(path: String) {
    let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(ColorChoice::Auto);
    let mut files = codespan_reporting::files::SimpleFiles::new();
    let config = codespan_reporting::term::Config::default();

    let src = match std::fs::read_to_string(&path) {
        Ok(contents) => contents,
        Err(err) => {
            eprintln!("Cannot read `{path}`: {err}");
            return;
        }
    };

    let (module, errors) = surface::Module::parse(&src);
    let file = files.add(path, src);

    if !errors.is_empty() {
        for error in errors {
            let diag = error.to_diagnostic(file);
            codespan_reporting::term::emit(&mut writer, &config, &files, &diag)
                .expect("Could not emit diagnostic");
        }
        return;
    }

    let mut elab_ctx = ElabCtx::new(file);
    let module_core = elab_ctx.elab_module(&module);

    let errors: Vec<_> = elab_ctx.drain_errors().collect();
    if !errors.is_empty() {
        for error in errors {
            let diag = error.to_diagnostic();
            codespan_reporting::term::emit(&mut writer, &config, &files, &diag)
                .expect("Could not emit diagnostic");
        }
        return;
    }

    let module_surface = elab_ctx.unelab_ctx().unelab_module(&module_core);
    let pretty_ctx = elab_ctx.pretty_ctx();
    let module_doc = pretty_ctx.pretty_module(&module_surface).into_doc();
    println!("{}", module_doc.pretty(80));
}
