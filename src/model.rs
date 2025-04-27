use crate::ast::Enum;
pub use crate::common::Component;
use std::sync::Arc;

pub type Register = crate::common::Register<Type>;
pub type BlockElement = crate::common::BlockElement<Component<ComponentType>>;
pub type Block = crate::common::Block<BlockElement>;
pub type Type = crate::common::Type<ComponentType>;

#[derive(Debug)]
pub enum ComponentType {
    Register(Arc<Register>),
    Block(Arc<Block>),
}

#[derive(Debug)]
pub struct Model {
    pub enums: Vec<Enum>,
    pub registers: Vec<Register>,
    pub blocks: Vec<Block>,
}
