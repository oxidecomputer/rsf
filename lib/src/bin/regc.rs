use camino::Utf8PathBuf;
use clap::Parser;
use rsf::{model::ModelModules, parser::parse};

#[derive(Parser, Debug)]
struct Cli {
    /// Filename of the spec
    spec: Utf8PathBuf,
}

fn main() {
    let args = Cli::parse();
    let ast = match parse(&args.spec) {
        Ok(ast) => ast,
        Err(e) => {
            eprintln!("parse error: {e}");
            std::process::exit(1);
        }
    };

    let _resolved = match ModelModules::resolve(&ast, String::default()) {
        Ok(resolved) => resolved,
        Err(e) => {
            eprintln!("resolve error: {e}");
            std::process::exit(1);
        }
    };
}
