pub use crate::common::Enum;

use crate::ast::{self, AstModules, Identifier};
use anyhow::{Result, anyhow};
use std::{
    collections::{BTreeMap, VecDeque},
    sync::Arc,
};

pub type Type = crate::common::Type<QualifiedType>;
pub type Block = crate::common::Block<QualifiedType>;
pub type BlockElement = crate::common::BlockElement<QualifiedType>;
pub type Component = crate::common::Component<QualifiedType>;
pub type Register = crate::common::Register<Type>;
pub type Field = crate::common::Field<Type>;

#[derive(Debug, Clone)]
pub enum ComponentType {
    Register(Arc<Register>),
    Enum(Arc<Enum>),
    Block(Arc<Block>),
    Bjalbo,
}

#[derive(Debug, Default, Clone)]
pub struct Model {
    pub enums: Vec<Arc<Enum>>,
    pub registers: Vec<Arc<Register>>,
    pub blocks: Vec<Arc<Block>>,
}

#[derive(Debug, Default, Clone)]
pub struct ModelModules {
    pub root: Model,
    pub used: BTreeMap<String, ModelModules>,
}

#[derive(Debug, Clone)]
pub struct QualifiedType {
    pub module_path: Vec<Model>,
    pub typ: ComponentType,
}

impl ModelModules {
    pub fn resolve(m: &AstModules) -> Result<Self> {
        let mut mm = Self::default();

        // Depth-first construction to build out the leaves of the tree first
        // so references can be resolved.
        for (module_name, module_tree) in &m.used {
            mm.used
                .insert(module_name.clone(), Self::resolve(module_tree)?);
        }

        for e in &m.root.enums {
            mm.root.enums.push(Arc::new(e.clone()));
        }
        for r in &m.root.registers {
            let mut fields = Vec::default();
            for f in &r.fields {
                fields.push(Field {
                    id: f.id.clone(),
                    mode: f.mode.clone(),
                    typ: mm.resolve_type(&f.typ)?,
                });
            }
            mm.root.registers.push(Arc::new(Register {
                id: r.id.clone(),
                width: r.width.clone(),
                fields,
            }));
        }
        for b in &m.root.blocks {
            let mut elements = Vec::default();
            for e in &b.elements {
                elements.push(BlockElement {
                    component: match &e.component {
                        crate::ast::Component::Single { id, typ } => {
                            Component::Single {
                                id: id.clone(),
                                typ: mm.resolve_path(&typ.path)?,
                            }
                        }
                        crate::ast::Component::Array {
                            id,
                            typ,
                            length,
                            spacing,
                        } => Component::Array {
                            id: id.clone(),
                            typ: mm.resolve_path(&typ.path)?,
                            length: length.clone(),
                            spacing: spacing.clone(),
                        },
                    },
                    offset: e.offset.clone(),
                })
            }
            mm.root.blocks.push(Arc::new(Block {
                id: b.id.clone(),
                elements,
            }));
        }

        Ok(mm)
    }

    pub fn resolve_type(&self, typ: &ast::Type) -> Result<Type> {
        let result = match typ {
            ast::Type::Bool => Type::Bool,
            ast::Type::Bitfield { width } => Type::Bitfield {
                width: width.clone(),
            },
            ast::Type::Ellipsis => Type::Ellipsis,
            ast::Type::Component { id } => Type::Component {
                id: self.resolve_path(&id.path)?,
            },
        };
        Ok(result)
    }

    pub fn resolve_path(&self, path: &[Identifier]) -> Result<QualifiedType> {
        let Some((typ, path)) = path.split_last() else {
            return Err(anyhow!("could not resolve empty path"));
        };
        self.resolve_path_rec(typ, path.iter().collect())
    }

    pub fn resolve_path_rec(
        &self,
        typ: &Identifier,
        mut path: VecDeque<&Identifier>,
    ) -> Result<QualifiedType> {
        match path.pop_front() {
            Some(module) => {
                let next = self.used.get(&module.name).ok_or_else(|| {
                    // TODO make sure we show the whole path that is not found
                    // in the error.
                    anyhow!("module {} not found", module.name)
                })?;
                let tp = next.resolve_path_rec(typ, path)?;
                let mut module_path = vec![self.root.clone()];
                module_path.extend_from_slice(&tp.module_path);
                Ok(QualifiedType {
                    module_path,
                    typ: tp.typ,
                })
            }
            None => {
                if let Some(r) =
                    self.root.registers.iter().find(|x| x.id.name == typ.name)
                {
                    return Ok(QualifiedType {
                        module_path: Vec::default(),
                        typ: ComponentType::Register(r.clone()),
                    });
                }
                if let Some(e) =
                    self.root.enums.iter().find(|x| x.id.name == typ.name)
                {
                    return Ok(QualifiedType {
                        module_path: Vec::default(),
                        typ: ComponentType::Enum(e.clone()),
                    });
                }
                if let Some(b) =
                    self.root.blocks.iter().find(|x| x.id.name == typ.name)
                {
                    return Ok(QualifiedType {
                        module_path: Vec::default(),
                        typ: ComponentType::Block(b.clone()),
                    });
                }

                // TODO make sure we show the whole path that is not found
                // in the error.
                Err(anyhow!("type {} not found {:#?}", typ.name, self.root))
            }
        }
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse;

    #[test]
    fn nic_example_resolve() {
        let ast = match parse("examples/nic.rsf".into()) {
            Ok(ast) => ast,
            Err(ref e) => {
                panic!("parsing failed: {}", e);
            }
        };

        let resolve = ModelModules::resolve(&ast).expect("resolve ast");
        println!("{resolve:#?}");
    }
}
