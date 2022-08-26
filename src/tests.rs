use std::io::Write;
use std::path::Path;

use codespan_reporting::term::termcolor::NoColor;

use crate::core::elab::ElabCtx;
use crate::surface;

#[datatest::files("test-data/elab/", {input_path in r"^(.*).pion", expected_path = "${1}.expected"})]
fn test_elab(input_path: &Path, expected_path: &Path) {
    let input_path = input_path.to_str().unwrap();

    let mut files = codespan_reporting::files::SimpleFiles::new();
    let config = codespan_reporting::term::Config::default();

    let src = match std::fs::read_to_string(&input_path) {
        Ok(contents) => contents,
        Err(err) => panic!("Cannot read `{input_path}`: {err}"),
    };

    let mut output = Vec::with_capacity(src.len());

    let (module, parse_errors) = surface::Module::parse(&src);
    let file = files.add(input_path, src);

    let mut elab_ctx = ElabCtx::new(file);
    let module_core = elab_ctx.elab_module(&module);

    let elab_errors: Vec<_> = elab_ctx.drain_errors().collect();

    let module_surface = elab_ctx.unelab_ctx().unelab_module(&module_core);
    let pretty_ctx = elab_ctx.pretty_ctx();
    let module_doc = pretty_ctx.pretty_module(&module_surface).into_doc();
    writeln!(output, "{}", module_doc.pretty(80)).expect("Could not write module");

    let mut output = NoColor::new(output);
    for error in parse_errors {
        let diag = error.to_diagnostic(file);
        codespan_reporting::term::emit(&mut output, &config, &files, &diag)
            .expect("Could not emit diagnostic");
    }

    for error in elab_errors {
        let diag = error.to_diagnostic();
        codespan_reporting::term::emit(&mut output, &config, &files, &diag)
            .expect("Could not emit diagnostic");
    }

    let output = String::from_utf8(output.into_inner()).unwrap();
    let expect = expect_test::expect_file![format!(
        "{}/{}",
        env!("CARGO_MANIFEST_DIR"),
        expected_path.display()
    )];
    expect.assert_eq(&output)
}
