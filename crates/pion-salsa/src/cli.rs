use clap::{Parser, Subcommand};

mod batch;

#[derive(Parser)]
#[clap(author, version, about)]
struct Args {
    #[clap(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    Repl,
    Elab { path: String },
}

pub fn cli() {
    let args = Args::parse();
    match args.command {
        Command::Repl => todo!(),
        Command::Elab { path } => batch::elab(path),
    }
}
