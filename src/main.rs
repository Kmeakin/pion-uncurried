use pion::core::elab::ElabCtx;
use pion::surface;
use rustyline::error::ReadlineError;
use rustyline::Editor;

const HISTORY_FILE: &str = "pion_history";

fn main() {
    let mut editor = Editor::<()>::new().expect("Could not create editor");

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
        if !errors.is_empty() {
            panic!("parse errors encountered: {errors:?}")
        }

        let mut elab_ctx = ElabCtx::new();
        let (expr_core, expr_type) = elab_ctx.synth_expr(&expr);
        dbg!(&expr_core);
        dbg!(&expr_type);

        let expr_value = elab_ctx.eval_ctx().eval_expr(&expr_core);
        dbg!(&expr_value);

        let quoted_core = elab_ctx.quote_ctx().quote_value(&expr_value);
        dbg!(quoted_core);
    }
}
