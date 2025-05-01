pub use crate::common::Enum;

use crate::{
    ast::{self, AstModules, Identifier},
    common::Typename,
};
use anyhow::{Result, anyhow};
use colored::*;
use std::{
    collections::{BTreeMap, VecDeque},
    fmt::Display,
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
}

impl Typename for ComponentType {
    fn typename(&self) -> String {
        match self {
            Self::Register(x) => x.id.name.clone(),
            Self::Enum(x) => x.id.name.clone(),
            Self::Block(x) => x.id.name.clone(),
        }
    }
}

#[derive(Debug, Clone)]
pub struct Model {
    pub id: String,
    pub enums: Vec<Arc<Enum>>,
    pub registers: Vec<Arc<Register>>,
    pub blocks: Vec<Arc<Block>>,
}

#[derive(Debug, Clone)]
pub struct ModelModules {
    pub root: Model,
    pub used: BTreeMap<String, ModelModules>,
}

#[derive(Debug, Clone)]
pub struct QualifiedType {
    pub module_path: Vec<Model>,
    pub typ: ComponentType,
}

impl Display for ModelModules {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.root)?;

        for submodule in self.used.values() {
            write!(f, "{}", submodule)?;
        }

        Ok(())
    }
}

impl Display for Model {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let name = if self.id.is_empty() {
            "root"
        } else {
            self.id.as_str()
        };
        writeln!(f, "{}", name.magenta())?;
        writeln!(f, "{}", "=".repeat(name.len()).dimmed())?;
        for x in &self.enums {
            writeln!(f, "{}", x)?;
        }
        for x in &self.registers {
            writeln!(f, "{}", x)?;
        }
        for x in &self.blocks {
            writeln!(f, "{}", x)?;
        }
        Ok(())
    }
}

impl Display for Register {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{} {}{}{}{}",
            "register".blue(),
            self.id.name.cyan(),
            "<".dimmed(),
            self.width.value.to_string().yellow(),
            ">".dimmed(),
        )?;
        for x in &self.fields {
            writeln!(f, "  {}", x)?;
        }
        Ok(())
    }
}

impl Display for Field {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}{} {} {}",
            self.id.name,
            ":".dimmed(),
            self.mode.to_string().blue(),
            self.typ,
        )?;
        if let Some(offset) = &self.offset {
            write!(
                f,
                " {} {}",
                "@".dimmed(),
                offset.value.to_string().yellow()
            )?;
        }
        Ok(())
    }
}

impl Display for Enum {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(
            f,
            "{} {}{}{}{}",
            "enum".blue(),
            self.id.name.cyan(),
            "<".dimmed(),
            self.width.value.to_string().yellow(),
            ">".dimmed(),
        )?;
        for x in &self.alternatives {
            writeln!(f, "  {}", x)?;
        }
        Ok(())
    }
}

impl Display for Block {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{} {}", "block".blue(), self.id.name.cyan())?;
        for x in &self.elements {
            writeln!(f, "  {}", x)?;
        }
        Ok(())
    }
}

impl Display for BlockElement {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.component)?;
        if let Some(offset) = &self.offset {
            write!(
                f,
                " {} {}",
                "@".dimmed(),
                offset.value.to_string().yellow()
            )?;
        }
        Ok(())
    }
}

impl Display for Component {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Single { id, typ } => {
                write!(f, "{}: {}", id.name, typ.typename())?;
            }
            Self::Array {
                id,
                typ,
                length,
                spacing,
            } => {
                write!(
                    f,
                    "{}: {}{}{}",
                    id.name,
                    typ.typename(),
                    "[".dimmed(),
                    length.value.to_string().yellow(),
                )?;
                if let Some(spacing) = &spacing {
                    write!(
                        f,
                        "{} {}{}",
                        ";".dimmed(),
                        spacing.value.to_string().yellow(),
                        "]".dimmed(),
                    )?;
                } else {
                    write!(f, "{}", "]".dimmed())?;
                }
            }
        }
        Ok(())
    }
}

impl Display for QualifiedType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        let path = self
            .module_path
            .iter()
            .map(|model| model.id.as_str())
            .collect::<Vec<_>>()
            .join("::");
        write!(f, "{}::{}", path, self.typ)
    }
}

impl Typename for QualifiedType {
    fn typename(&self) -> String {
        let sep = "::".dimmed().to_string();
        let path = self
            .module_path
            .iter()
            .map(|model| model.id.as_str().magenta().to_string())
            .collect::<Vec<_>>()
            .join(sep.as_str());
        format!("{}{}{}", path, sep, self.typ.typename().cyan())
    }
}

impl Display for ComponentType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(x) => write!(f, "register {}", x),
            Self::Enum(x) => write!(f, "enum {}", x),
            Self::Block(x) => write!(f, "block {}", x),
        }
    }
}

impl ModelModules {
    pub fn resolve(m: &AstModules, id: String) -> Result<Self> {
        let mut mm = Self {
            used: BTreeMap::default(),
            root: Model {
                id,
                enums: vec![],
                registers: vec![],
                blocks: vec![],
            },
        };

        // Depth-first construction to build out the leaves of the tree first
        // so references can be resolved.
        for (module_name, module_tree) in &m.used {
            let next = Self::resolve(module_tree, module_name.clone())?;
            mm.used.insert(module_name.clone(), next);
        }

        for e in &m.root.enums {
            mm.root.enums.push(Arc::new(e.clone()));
        }
        for r in &m.root.registers {
            let mut fields = Vec::default();
            for f in &r.fields {
                fields.push(Field {
                    doc: f.doc.clone(),
                    id: f.id.clone(),
                    mode: f.mode.clone(),
                    typ: mm.resolve_type(&f.typ)?,
                    offset: f.offset.clone(),
                });
            }
            mm.root.registers.push(Arc::new(Register {
                doc: r.doc.clone(),
                id: r.id.clone(),
                width: r.width.clone(),
                fields,
            }));
        }
        for b in &m.root.blocks {
            let mut elements = Vec::default();
            for e in &b.elements {
                elements.push(BlockElement {
                    doc: e.doc.clone(),
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
                doc: b.doc.clone(),
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
                        module_path: vec![self.root.clone()],
                        typ: ComponentType::Register(r.clone()),
                    });
                }
                if let Some(e) =
                    self.root.enums.iter().find(|x| x.id.name == typ.name)
                {
                    return Ok(QualifiedType {
                        module_path: vec![self.root.clone()],
                        typ: ComponentType::Enum(e.clone()),
                    });
                }
                if let Some(b) =
                    self.root.blocks.iter().find(|x| x.id.name == typ.name)
                {
                    return Ok(QualifiedType {
                        module_path: vec![self.root.clone()],
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
    use crate::{common::FieldMode, parser::parse};

    fn check_enum_field(
        field: &Field,
        name: &str,
        mode: FieldMode,
        type_path: &[&str],
        typename: &str,
    ) {
        assert_eq!(name, field.id.name);
        let Type::Component { id } = &field.typ else {
            panic!("expected component")
        };

        assert_eq!(mode, field.mode);

        for (i, p) in type_path.iter().enumerate() {
            assert_eq!(&id.module_path[i].id, p);
        }
        let ComponentType::Enum(e) = &id.typ else {
            panic!("expected enum")
        };
        assert_eq!(e.id.name, typename);
    }

    fn check_ellipsis_field(field: &Field, name: &str, mode: FieldMode) {
        assert_eq!(name, field.id.name);
        assert_eq!(mode, field.mode);
        assert!(matches!(field.typ, Type::Ellipsis));
    }

    fn check_bool_field(field: &Field, name: &str, mode: FieldMode) {
        assert_eq!(name, field.id.name);
        assert_eq!(mode, field.mode);
        assert!(matches!(field.typ, Type::Bool));
    }

    fn check_register_block_single_component(
        block_element: &BlockElement,
        name: &str,
        type_path: &[&str],
        typename: &str,
        offset: Option<u128>,
    ) {
        let Component::Single { id, typ } = &block_element.component else {
            panic!("expected single component");
        };
        assert_eq!(id.name, name);
        for (i, p) in type_path.iter().enumerate() {
            assert_eq!(&typ.module_path[i].id, p);
        }
        let ComponentType::Register(r) = &typ.typ else {
            panic!("expected register component");
        };
        assert_eq!(typename, r.id.name);
        assert_eq!(offset, block_element.offset.as_ref().map(|x| x.value))
    }

    fn check_block_block_array_component(
        block_element: &BlockElement,
        name: &str,
        type_path: &[&str],
        typename: &str,
        count: u128,
        stride: Option<u128>,
        offset: Option<u128>,
    ) {
        let Component::Array {
            id,
            typ,
            length,
            spacing,
        } = &block_element.component
        else {
            panic!("expected single component");
        };
        assert_eq!(id.name, name);
        for (i, p) in type_path.iter().enumerate() {
            assert_eq!(&typ.module_path[i].id, p);
        }
        let ComponentType::Block(b) = &typ.typ else {
            panic!("expected block component");
        };
        assert_eq!(typename, b.id.name);
        assert_eq!(offset, block_element.offset.as_ref().map(|x| x.value));
        assert_eq!(count, length.value);
        assert_eq!(stride, spacing.as_ref().map(|x| x.value));
    }

    #[test]
    fn nic_example_resolve() {
        let ast = match parse("examples/nic.rsf".into()) {
            Ok(ast) => ast,
            Err(ref e) => {
                panic!("parsing failed: {}", e);
            }
        };

        let resolved =
            ModelModules::resolve(&ast, String::from("")).expect("resolve ast");
        println!("{resolved}");

        // check PhyConfig
        assert_eq!(resolved.root.registers[0].id.name, "PhyConfig");
        assert_eq!(resolved.root.registers[0].width.value, 32);
        check_enum_field(
            &resolved.root.registers[0].fields[0],
            "speed",
            FieldMode::ReadWrite,
            // first path element is the "root"
            &["", "ethernet"],
            "DataRate",
        );
        check_enum_field(
            &resolved.root.registers[0].fields[1],
            "reach",
            FieldMode::ReadWrite,
            &["", "ethernet"],
            "Reach",
        );
        check_enum_field(
            &resolved.root.registers[0].fields[2],
            "lanes",
            FieldMode::ReadWrite,
            &[""],
            "Lanes",
        );
        check_enum_field(
            &resolved.root.registers[0].fields[3],
            "fec",
            FieldMode::ReadWrite,
            &["", "ethernet"],
            "Fec",
        );
        check_enum_field(
            &resolved.root.registers[0].fields[4],
            "modulation",
            FieldMode::ReadWrite,
            &["", "cei"],
            "Modulation",
        );
        check_ellipsis_field(
            &resolved.root.registers[0].fields[5],
            "_",
            FieldMode::Reserved,
        );

        // check PhyStatus
        assert_eq!(resolved.root.registers[1].id.name, "PhyStatus");
        assert_eq!(resolved.root.registers[1].width.value, 32);
        check_bool_field(
            &resolved.root.registers[1].fields[0],
            "carrier",
            FieldMode::ReadOnly,
        );
        check_bool_field(
            &resolved.root.registers[1].fields[1],
            "signal_error",
            FieldMode::ReadOnly,
        );
        check_bool_field(
            &resolved.root.registers[1].fields[2],
            "data_valid",
            FieldMode::ReadOnly,
        );
        check_ellipsis_field(
            &resolved.root.registers[1].fields[3],
            "_",
            FieldMode::Reserved,
        );

        // check Phy
        assert_eq!(&resolved.root.blocks[0].id.name, "Phy");
        check_register_block_single_component(
            &resolved.root.blocks[0].elements[0],
            "config",
            &[],
            "PhyConfig",
            Some(0x200),
        );
        check_register_block_single_component(
            &resolved.root.blocks[0].elements[1],
            "status",
            &[],
            "PhyStatus",
            Some(0x400),
        );

        // check Nic
        assert_eq!(&resolved.root.blocks[1].id.name, "Nic");
        check_block_block_array_component(
            &resolved.root.blocks[1].elements[0],
            "phys",
            &[],
            "Phy",
            4,
            Some(0x1000),
            Some(0x6000),
        );
    }
}
