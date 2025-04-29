use std::{collections::BTreeMap, fmt::Display};

pub use crate::common::{Enum, Identifier, Number};
use crate::common::{Span, Typename};

pub type Type = crate::common::Type<QualifiedType>;
pub type Block = crate::common::Block<QualifiedType>;
pub type BlockElement = crate::common::BlockElement<QualifiedType>;
pub type Component = crate::common::Component<QualifiedType>;
pub type Register = crate::common::Register<Type>;
pub type Field = crate::common::Field<Type>;

#[derive(Debug, Clone)]
pub struct Use {
    pub module: Identifier,
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

/// A set of ASTs indexed by module name
pub struct AstModules {
    pub root: Ast,
    pub used: BTreeMap<String, AstModules>,
}
