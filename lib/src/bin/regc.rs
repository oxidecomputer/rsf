use camino::Utf8PathBuf;
use clap::{Parser, Subcommand};
use colored::Colorize;
use rsf::{
    ast::Identifier,
    model::{ModelModules, Register, Visitor},
    parser::parse,
};
use std::num::ParseIntError;
use std::sync::Arc;

#[derive(Parser, Debug)]
struct Cli {
    /// Filename of the spec
    spec: Utf8PathBuf,

    /// Command to execute
    #[command(subcommand)]
    command: Commands,
}

#[derive(Subcommand, Debug)]
pub enum Commands {
    /// Print an address map of the register space
    AddrMap(AddrMap),

    /// Print information about a register
    RegInfo(RegInfo),
}

#[derive(Parser, Debug)]
pub struct AddrMap {}

#[derive(Parser, Debug)]
pub struct RegInfo {
    /// Name of the register to show
    #[clap(long, conflicts_with = "address")]
    name: Option<String>,

    /// Address of the register to show. Hex or decimal values accepted.
    #[clap(long, conflicts_with = "name", value_parser = parse_hex_or_dec)]
    address: Option<u128>,
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
    match args.command {
        Commands::AddrMap(_) => {
            show_register_map(&resolved);
        }
        Commands::RegInfo(cmd) => {
            if let Some(name) = cmd.name {
                let mut v = RegInfoVisitor {
                    matcher: RegInfoMatch::Name(name),
                };
                resolved.root.accept(&mut v);
            }
            if let Some(addr) = cmd.address {
                let mut v = RegInfoVisitor {
                    matcher: RegInfoMatch::Addr(addr),
                };
                resolved.root.accept(&mut v);
            };
        }
    };
}

fn show_register_map(m: &ModelModules) {
    let mut visitor = RegMapVisitor {};
    m.root.accept(&mut visitor);
}

struct RegMapVisitor {}
impl Visitor for RegMapVisitor {
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
                    .join(".".dimmed().to_string().as_str()),
                ".".dimmed(),
                id.name.cyan(),
            )
        };
        let addr = format!("{addr:08x}");
        println!("{}{}{} {}", "0x".green(), addr.green(), ":".dimmed(), name);
    }
}

enum RegInfoMatch {
    Name(String),
    Addr(u128),
}

struct RegInfoVisitor {
    matcher: RegInfoMatch,
}

impl Visitor for RegInfoVisitor {
    fn register_component(
        &mut self,
        id: &Identifier,
        path: &[Identifier],
        reg: Arc<Register>,
        _array_index: Option<u128>,
        addr: u128,
    ) {
        let fullpath = if path.is_empty() {
            id.name.clone()
        } else {
            format!(
                "{}.{}",
                path.iter()
                    .map(|x| x.name.as_str())
                    .collect::<Vec<_>>()
                    .join("."),
                id.name,
            )
        };
        match &self.matcher {
            RegInfoMatch::Name(name) => {
                let re = regex::Regex::new(name).expect("valid regex");
                if re.is_match(fullpath.as_str()) {
                    println!(
                        "{}{} {}",
                        format!("0x{addr:x}").green(),
                        ":".dimmed(),
                        fullpath
                    );
                    println!("{reg}");
                }
            }
            RegInfoMatch::Addr(a) => {
                if *a == addr {
                    println!(
                        "{}{} {}",
                        format!("0x{addr:x}").green(),
                        ":".dimmed(),
                        fullpath
                    );
                    println!("{reg}");
                }
            }
        }
    }
}

fn parse_hex_or_dec(s: &str) -> Result<u128, ParseIntError> {
    if let Some(hex_str) = s.strip_prefix("0x").or_else(|| s.strip_prefix("0X"))
    {
        u128::from_str_radix(hex_str, 16)
    } else {
        s.parse::<u128>()
    }
}
