use codespan_reporting::term::termcolor::ColorChoice;
use pion::core::elab::ElabCtx;
use pion::surface;
use rustyline::error::ReadlineError;
use rustyline::Editor;

const HISTORY_FILE: &str = "pion_history";

fn main() {
    let mut editor = Editor::<()>::new().expect("Could not create editor");
    let mut writer = codespan_reporting::term::termcolor::StandardStream::stderr(ColorChoice::Auto);
    let mut files = codespan_reporting::files::SimpleFiles::new();
    let config = codespan_reporting::term::Config::default();

    loop {
        match editor.load_history(HISTORY_FILE) {
            Ok(()) => {}
            Err(err) => {
                eprintln!("Unable to read history file: {err}");
            }
        }
        let line = match editor.readline(">> ") {
            Ok(line) => line,
            Err(ReadlineError::Interrupted) => continue,
            Err(ReadlineError::Eof) => break,
            Err(err) => {
                eprintln!("Unable to read line: {err}");
                continue;
            }
        };
        editor.add_history_entry(&line);
        match editor.save_history(HISTORY_FILE) {
            Ok(()) => {}
            Err(err) => {
                eprintln!("Unable to save history file: {err}");
            }
        }

        let (expr, errors) = surface::Expr::parse(&line);
        let file = files.add("<repl>", line);

        if !errors.is_empty() {
            for error in errors {
                let diag = error.to_diagnostic(file);
                codespan_reporting::term::emit(&mut writer, &config, &files, &diag)
                    .expect("Could not emit diagnostic");
            }
            continue;
        }

        let mut elab_ctx = ElabCtx::new(file);
        let (expr_core, expr_type) = elab_ctx.synth_expr(&expr);

        let errors: Vec<_> = elab_ctx.drain_errors().collect();
        if !errors.is_empty() {
            for error in errors {
                let diag = error.to_diagnostic();
                codespan_reporting::term::emit(&mut writer, &config, &files, &diag)
                    .expect("Could not emit diagnostic");
            }
            continue;
        }

        let type_core = elab_ctx.quote_ctx().quote_value(&expr_type);
        let expr_surface = elab_ctx.unelab_ctx().unelab_expr(&expr_core);
        let type_surface = elab_ctx.unelab_ctx().unelab_expr(&type_core);
        let pretty_ctx = elab_ctx.pretty_ctx();
        let expr_doc = pretty_ctx.pretty_expr(&expr_surface).into_doc();
        let type_doc = pretty_ctx.pretty_expr(&type_surface).into_doc();
        println!("core: {}", expr_doc.pretty(80));
        println!("type: {}", type_doc.pretty(80));

        let expr_value = elab_ctx.eval_ctx().eval_expr(&expr_core);
        let value_core = elab_ctx.quote_ctx().quote_value(&expr_value);
        let value_surface = elab_ctx.unelab_ctx().unelab_expr(&value_core);
        let pretty_ctx = elab_ctx.pretty_ctx();
        let value_doc = pretty_ctx.pretty_expr(&value_surface).into_doc();
        println!("value: {}", value_doc.pretty(80));
    }
}
