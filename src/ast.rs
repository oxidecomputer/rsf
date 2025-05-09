use std::{collections::BTreeMap, fmt::Display};

pub use crate::common::{Enum, Identifier, Number};
use crate::common::{Span, Typename};
use std::fmt::Write;

pub type FieldType = crate::common::FieldType<QualifiedType>;
pub type Block = crate::common::Block<QualifiedType>;
pub type BlockElement = crate::common::BlockElement<QualifiedType>;
pub type Component = crate::common::Component<QualifiedType>;
pub type Register = crate::common::Register<FieldType>;
pub type Field = crate::common::Field<FieldType>;

#[derive(Debug, Clone)]
pub struct Use {
    pub module: Identifier,
}

impl Emit for Use {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        writeln!(f, "use {};", self.module.name)
    }
}

impl From<Vec<&str>> for QualifiedType {
    fn from(value: Vec<&str>) -> Self {
        Self {
            path: value.into_iter().map(Identifier::new).collect(),
            span: Span::Any,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct QualifiedType {
    pub path: Vec<Identifier>,
    pub span: Span,
}

impl Display for QualifiedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let path = self
            .path
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        write!(f, "{}", path)
    }
}

impl Typename for QualifiedType {
    fn typename(&self) -> String {
        self.to_string()
    }
}

#[derive(Debug, Default, Clone)]
pub struct Ast {
    pub use_statements: Vec<Use>,
    pub enums: Vec<Enum>,
    pub registers: Vec<Register>,
    pub blocks: Vec<Block>,
}

pub trait Emit {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result;
    fn to_code(&self) -> String {
        let mut output = String::default();
        self.emit(&mut output).unwrap();
        output
    }
}

impl Emit for Register {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        for x in &self.doc {
            writeln!(f, "///{}", x)?;
        }
        writeln!(f, "register<{}> {} {{", self.width.to_code(), self.id.name)?;
        for x in &self.fields {
            x.emit(f)?;
        }
        writeln!(f, "}}")?;
        writeln!(f)
    }
}

impl Emit for Field {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        for x in &self.doc {
            writeln!(f, "    ///{}", x)?;
        }
        write!(
            f,
            "    {}: {} {}",
            self.id.name,
            self.mode.to_code(),
            self.typ.to_code()
        )?;
        writeln!(f, " @ {},", self.offset.to_code())
    }
}

impl Emit for FieldType {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "bool"),
            Self::Bitfield { width } => write!(f, "b{}", width.value),
            Self::User { id } => id.emit(f),
        }
    }
}

impl Emit for QualifiedType {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        let path = self
            .path
            .iter()
            .map(|id| id.name.as_str())
            .collect::<Vec<_>>()
            .join("::");
        write!(f, "{}", path)
    }
}

impl Emit for Block {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        for x in &self.doc {
            writeln!(f, "///{}", x)?;
        }
        writeln!(f, "block {} {{", self.id.name)?;
        for x in &self.elements {
            x.emit(f)?;
        }
        writeln!(f, "}}")?;
        writeln!(f)
    }
}

impl Emit for BlockElement {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        for x in &self.doc {
            writeln!(f, "    ///{}", x)?;
        }
        self.component.emit(f)?;
        write!(f, " @ {},", self.offset.to_code())?;
        writeln!(f)
    }
}

impl Emit for Component {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        match self {
            Self::Single { id, typ } => {
                write!(f, "    {}: {}", id.name, typ.to_code())?;
            }
            Self::Array {
                id,
                typ,
                length,
                spacing,
            } => {
                write!(
                    f,
                    "    {}: {}[{}",
                    id.name,
                    typ.to_code(),
                    length.to_code(),
                )?;
                write!(f, "; {}]", spacing.to_code())?;
            }
        }
        Ok(())
    }
}

impl Emit for Ast {
    fn emit(&self, f: &mut impl Write) -> std::fmt::Result {
        for x in &self.use_statements {
            x.emit(f)?;
        }
        writeln!(f)?;

        for x in &self.enums {
            x.emit(f)?;
        }

        writeln!(f)?;
        for x in &self.registers {
            x.emit(f)?;
        }

        for x in &self.blocks {
            x.emit(f)?;
        }
        Ok(())
    }
}

/// A set of ASTs indexed by module name
pub struct AstModules {
    pub root: Ast,
    pub used: BTreeMap<String, AstModules>,
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse;
    use expectorate::assert_contents;

    #[test]
    fn ast_roundtrip() {
        let modules = match parse("examples/nic.rsf".into()) {
            Ok(ast) => ast,
            Err(ref e) => {
                panic!("parsing failed: {}", e);
            }
        };

        let code = modules.root.to_code();

        println!("{code}");
        // make sure we end with exactly one newline for comparison
        let code = code.trim().to_owned() + "\n";

        assert_contents("examples/nic.rsf", &code);
    }
}
