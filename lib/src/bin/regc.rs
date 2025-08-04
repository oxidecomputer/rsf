use std::sync::Arc;

use camino::Utf8PathBuf;
use clap::Parser;
use colored::Colorize;
use rsf::{
    ast::Identifier,
    model::{ModelModules, Register, Visitor},
    parser::parse,
};

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

    let resolved = match ModelModules::resolve(&ast, String::default()) {
        Ok(resolved) => resolved,
        Err(e) => {
            eprintln!("resolve error: {e}");
            std::process::exit(1);
        }
    };

    let _modules = match resolved.get_modules() {
        Ok(modules) => modules,
        Err(e) => {
            eprintln!("failed to get resolved modules: {e}");
            std::process::exit(1);
        }
    };

    show_register_map(&resolved)
}

struct RegisterMapVisitor {}

impl Visitor for RegisterMapVisitor {
    fn register_component(
        &mut self,
        id: &Identifier,
        path: &[Identifier],
        _reg: Arc<Register>,
        _array_index: Option<u128>,
        addr: u128,
    ) {
        let name = if path.is_empty() {
            id.name.cyan().to_string()
        } else {
            format!(
                "{}{}{}",
                path.iter()
                    .map(|x| x.name.blue().to_string())
                    .collect::<Vec<_>>()
                    .join(":".dimmed().to_string().as_str()),
                ":".dimmed(),
                id.name.cyan(),
            )
        };
        let addr = format!("{:08x}", addr);
        println!("{}{}{} {}", "0x".green(), addr.green(), ":".dimmed(), name);
    }
}

fn show_register_map(m: &ModelModules) {
    let mut visitor = RegisterMapVisitor {};
    m.root.accept(&mut visitor);
}
