use crate::common::Span;
pub use crate::common::{Enum, Identifier, Number};

pub type Type = crate::common::Type<QualifiedType>;
pub type Block = crate::common::Block<QualifiedType>;
pub type BlockElement = crate::common::BlockElement<QualifiedType>;
pub type Component = crate::common::Component<QualifiedType>;
pub type Register = crate::common::Register<Type>;
pub type Field = crate::common::Field<Type>;

#[derive(Debug)]
pub struct Use {
    pub module: Identifier,
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct ModulePath {
    pub path: Vec<Identifier>,
    pub span: Span,
}

impl From<Vec<&str>> for ModulePath {
    fn from(value: Vec<&str>) -> Self {
        Self {
            path: value.into_iter().map(Identifier::new).collect(),
            span: Span::Any,
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct QualifiedType {
    pub path: ModulePath,
    pub span: Span,
}

impl From<ModulePath> for QualifiedType {
    fn from(value: ModulePath) -> Self {
        Self {
            path: value,
            span: Span::Any,
        }
    }
}

#[derive(Debug, Default)]
pub struct Ast {
    pub use_statements: Vec<Use>,
    pub enums: Vec<Enum>,
    pub registers: Vec<Register>,
    pub blocks: Vec<Block>,
}
