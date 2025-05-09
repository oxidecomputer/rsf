pub use crate::common::Enum;

use crate::{
    ast::{self, AstModules, Identifier, Number},
    common::Typename,
};
use anyhow::{Result, anyhow};
use colored::*;
use std::{
    collections::{BTreeMap, VecDeque},
    fmt::Display,
    sync::Arc,
};

//pub type Type = crate::common::Type<QualifiedType>;
pub type Block = crate::common::Block<QualifiedComponentType>;
pub type BlockElement = crate::common::BlockElement<QualifiedComponentType>;
pub type Component = crate::common::Component<QualifiedComponentType>;
pub type FieldType = crate::common::FieldType<QualifiedFieldType>;
pub type Register = crate::common::Register<FieldType>;
pub type Field = crate::common::Field<FieldType>;

impl FieldType {
    pub fn width(&self) -> u128 {
        match self {
            Self::Bool => 1,
            Self::Bitfield { width } => width.value,
            Self::User { id } => {
                let FieldUserType::Enum(e) = &id.typ;
                e.width.value
            }
            Self::Ellipsis => todo!(),
        }
    }
}

#[derive(Debug, Clone)]
pub enum ComponentUserType {
    Register(Arc<Register>),
    Block(Arc<Block>),
}

#[derive(Debug, Clone)]
pub enum FieldUserType {
    Enum(Arc<Enum>),
}

impl Display for FieldUserType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Enum(e) => write!(f, "Enum({e})"),
        }
    }
}

impl Typename for FieldUserType {
    fn typename(&self) -> String {
        match self {
            Self::Enum(e) => e.id.name.clone(),
        }
    }
}

impl Typename for ComponentUserType {
    fn typename(&self) -> String {
        match self {
            Self::Register(x) => x.id.name.clone(),
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
pub struct QualifiedComponentType {
    pub module_path: Vec<Model>,
    pub typ: ComponentUserType,
}

#[derive(Debug, Clone)]
pub struct QualifiedFieldType {
    pub module_path: Vec<Model>,
    pub typ: FieldUserType,
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

#[derive(Debug)]
pub struct RegisterLookupResult {
    pub definition: Arc<Register>,
    pub address: u128,
}

#[derive(Debug)]
pub struct RegisterLookup {
    pub path: VecDeque<String>,
    pub result: Option<RegisterLookupResult>,
}

impl From<&str> for RegisterLookup {
    fn from(value: &str) -> Self {
        let path = value.split("/");
        Self {
            path: path.into_iter().map(|x| x.to_owned()).collect(),
            result: None,
        }
    }
}

impl Visitor for RegisterLookup {
    fn block_component(
        &mut self,
        id: &Identifier,
        _path: &[Identifier],
        _block: Arc<Block>,
        array_index: Option<u128>,
        _addr: u128,
    ) -> bool {
        let Some(first) = self.path.front() else {
            return false;
        };

        if &id.name == first {
            if let Some(array_index) = array_index {
                let Some(first) = self.path.get(1) else {
                    return false;
                };
                if &array_index.to_string() == first {
                    self.path.pop_front();
                    self.path.pop_front();
                    return true;
                } else {
                    return false;
                }
            }
            self.path.pop_front();
            true
        } else {
            false
        }
    }

    fn register_component(
        &mut self,
        id: &Identifier,
        _path: &[Identifier],
        reg: Arc<Register>,
        array_index: Option<u128>,
        addr: u128,
    ) {
        if self.path.len() > 1 {
            return;
        }
        let Some(first) = self.path.front() else {
            return;
        };
        if &id.name == first {
            if let Some(array_index) = array_index {
                let Some(first) = self.path.get(1) else {
                    return;
                };
                if &array_index.to_string() == first {
                    self.path.pop_front();
                    self.path.pop_front();
                    return;
                } else {
                    return;
                }
            }
            self.path.pop_front();
            self.result = Some(RegisterLookupResult {
                definition: reg.clone(),
                address: addr,
            })
        }
    }
}

impl Model {
    /// Fetch the register at the provided path. The path is expected to
    /// be in the form block_1/block_2/register for an arbitrary number
    /// of blocks. Array type block components are indexed with square
    /// brackets, e.g. block_1[5]/block_2/register.
    pub fn get_register(&self, _path: &str) -> Result<Option<Arc<Register>>> {
        todo!()
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
        write!(
            f,
            " {} {}",
            "@".dimmed(),
            self.offset.value.to_string().yellow()
        )?;
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
        write!(
            f,
            " {} {}",
            "@".dimmed(),
            self.offset.value.to_string().yellow()
        )
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
                write!(
                    f,
                    "{} {}{}",
                    ";".dimmed(),
                    spacing.value.to_string().yellow(),
                    "]".dimmed(),
                )?;
            }
        }
        Ok(())
    }
}

impl Display for QualifiedComponentType {
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

impl Typename for QualifiedComponentType {
    fn typename(&self) -> String {
        let path = self.module_path.iter().filter(|x| !x.id.is_empty());
        if self.module_path.len() == 1 {
            format!("{}", self.typ.typename().cyan())
        } else {
            let sep = "::".dimmed().to_string();
            let path = path
                .map(|model| model.id.as_str().magenta().to_string())
                .collect::<Vec<_>>()
                .join(sep.as_str());
            format!("{}{}{}", path, sep, self.typ.typename().cyan())
        }
    }
}

// TODO exact same as QualifiedComponentType
impl Display for QualifiedFieldType {
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

// TODO exact same as QualifiedComponentType
impl Typename for QualifiedFieldType {
    fn typename(&self) -> String {
        let path = self.module_path.iter().filter(|x| !x.id.is_empty());
        if self.module_path.len() == 1 {
            format!("{}", self.typ.typename().cyan())
        } else {
            let sep = "::".dimmed().to_string();
            let path = path
                .map(|model| model.id.as_str().magenta().to_string())
                .collect::<Vec<_>>()
                .join(sep.as_str());
            format!("{}{}{}", path, sep, self.typ.typename().cyan())
        }
    }
}

impl Display for ComponentUserType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Register(x) => write!(f, "register {}", x),
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
                    typ: mm.resolve_field_type(&f.typ)?,
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
                                typ: mm.resolve_component_type(&typ.path)?,
                            }
                        }
                        crate::ast::Component::Array {
                            id,
                            typ,
                            length,
                            spacing,
                        } => Component::Array {
                            id: id.clone(),
                            typ: mm.resolve_component_type(&typ.path)?,
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

        // TODO only intended for root and this is a recursive function
        //mm.check()?;

        Ok(mm)
    }

    pub fn resolve_field_type(
        &self,
        typ: &ast::FieldType,
    ) -> Result<FieldType> {
        let result = match typ {
            ast::FieldType::Bool => FieldType::Bool,
            ast::FieldType::Bitfield { width } => FieldType::Bitfield {
                width: width.clone(),
            },
            ast::FieldType::Ellipsis => FieldType::Ellipsis,
            ast::FieldType::User { id } => FieldType::User {
                id: self.resolve_field_path(&id.path)?,
            },
        };
        Ok(result)
    }

    pub fn resolve_field_path(
        &self,
        path: &[Identifier],
    ) -> Result<QualifiedFieldType> {
        let Some((typ, path)) = path.split_last() else {
            return Err(anyhow!("could not resolve empty path"));
        };
        self.resolve_field_path_rec(typ, path.iter().collect())
    }

    pub fn resolve_component_type(
        &self,
        path: &[Identifier],
    ) -> Result<QualifiedComponentType> {
        let Some((typ, path)) = path.split_last() else {
            return Err(anyhow!("could not resolve empty path"));
        };
        self.resolve_component_path_rec(typ, path.iter().collect())
    }

    pub fn resolve_field_path_rec(
        &self,
        typ: &Identifier,
        mut path: VecDeque<&Identifier>,
    ) -> Result<QualifiedFieldType> {
        match path.pop_front() {
            Some(module) => {
                let next = self.used.get(&module.name).ok_or_else(|| {
                    // TODO make sure we show the whole path that is not found
                    // in the error.
                    anyhow!("module {} not found", module.name)
                })?;
                let tp = next.resolve_field_path_rec(typ, path)?;
                let mut module_path = vec![self.root.clone()];
                module_path.extend_from_slice(&tp.module_path);
                Ok(QualifiedFieldType {
                    module_path,
                    typ: tp.typ,
                })
            }
            None => {
                if let Some(e) =
                    self.root.enums.iter().find(|x| x.id.name == typ.name)
                {
                    return Ok(QualifiedFieldType {
                        module_path: vec![self.root.clone()],
                        typ: FieldUserType::Enum(e.clone()),
                    });
                }

                // TODO make sure we show the whole path that is not found
                // in the error.
                Err(anyhow!("type {} not found {:#?}", typ.name, self.root))
            }
        }
    }

    pub fn resolve_component_path_rec(
        &self,
        typ: &Identifier,
        mut path: VecDeque<&Identifier>,
    ) -> Result<QualifiedComponentType> {
        match path.pop_front() {
            Some(module) => {
                let next = self.used.get(&module.name).ok_or_else(|| {
                    // TODO make sure we show the whole path that is not found
                    // in the error.
                    anyhow!("module {} not found", module.name)
                })?;
                let tp = next.resolve_component_path_rec(typ, path)?;
                let mut module_path = vec![self.root.clone()];
                module_path.extend_from_slice(&tp.module_path);
                Ok(QualifiedComponentType {
                    module_path,
                    typ: tp.typ,
                })
            }
            None => {
                if let Some(r) =
                    self.root.registers.iter().find(|x| x.id.name == typ.name)
                {
                    return Ok(QualifiedComponentType {
                        module_path: vec![self.root.clone()],
                        typ: ComponentUserType::Register(r.clone()),
                    });
                }
                if let Some(b) =
                    self.root.blocks.iter().find(|x| x.id.name == typ.name)
                {
                    return Ok(QualifiedComponentType {
                        module_path: vec![self.root.clone()],
                        typ: ComponentUserType::Block(b.clone()),
                    });
                }

                // TODO make sure we show the whole path that is not found
                // in the error.
                Err(anyhow!("type {} not found {:#?}", typ.name, self.root))
            }
        }
    }

    #[allow(dead_code)]
    fn check(&self) -> Result<()> {
        self.check_main_block()?;
        Ok(())
    }

    fn check_main_block(&self) -> Result<()> {
        self.root
            .blocks
            .iter()
            .find(|x| x.id.name == "Main")
            .ok_or(anyhow!("main block not found"))?;
        Ok(())
    }
}

impl Model {
    pub fn accept<V: Visitor>(&self, v: &mut V) {
        for e in &self.enums {
            v.enumeration(e.clone());
        }
        for r in &self.registers {
            v.register(r.clone());
        }
        for b in &self.blocks {
            v.block(b.clone(), 0);
            if b.id.name == "Main" {
                b.accept(v, 0, Vec::default());
            }
        }
    }
}

impl Block {
    pub fn accept<V: Visitor>(
        &self,
        v: &mut V,
        addr: u128,
        path: Vec<Identifier>,
    ) {
        for e in &self.elements {
            let addr = addr + e.offset.value;
            v.block_element(e, addr);
            v.component(&e.component, addr);
            e.component.accept(v, addr, path.clone());
        }
    }
}

impl Component {
    pub fn accept<V: Visitor>(
        &self,
        v: &mut V,
        addr: u128,
        mut path: Vec<Identifier>,
    ) {
        match self {
            Self::Single { id, typ } => {
                v.single_component(id, typ, addr);
                match &typ.typ {
                    ComponentUserType::Register(r) => {
                        v.register_component(id, &path, r.clone(), None, addr);
                    }
                    ComponentUserType::Block(b) => {
                        if v.block_component(id, &path, b.clone(), None, addr) {
                            path.push(id.clone());
                            b.accept(v, addr, path);
                        }
                    }
                }
            }
            Self::Array {
                id,
                typ,
                length,
                spacing,
            } => {
                for i in 0..length.value {
                    let addr = addr + i * spacing.value;
                    v.array_component(id, typ, length, spacing, addr);
                    match &typ.typ {
                        ComponentUserType::Register(r) => {
                            v.register_component(
                                id,
                                &path,
                                r.clone(),
                                Some(i),
                                addr,
                            );
                        }
                        ComponentUserType::Block(b) => {
                            let mut path = path.clone();
                            path.push(id.clone());
                            path.push(Identifier::new(&i.to_string()));
                            if v.block_component(
                                id,
                                &path,
                                b.clone(),
                                Some(i),
                                addr,
                            ) {
                                b.accept(v, addr, path);
                            }
                        }
                    }
                }
            }
        }
    }
}

impl Register {
    pub fn accept<V: Visitor>(&self, v: &mut V) {
        for f in &self.fields {
            v.field(f);
        }
    }
}

pub trait Visitor {
    fn register(&mut self, _reg: Arc<Register>) {}
    fn field(&mut self, _field: &Field) {}
    fn block(&mut self, _block: Arc<Block>, _addr: u128) {}
    fn enumeration(&mut self, _enum: Arc<Enum>) {}
    fn block_element(&mut self, _element: &BlockElement, _addr: u128) {}
    fn component(&mut self, _component: &Component, _addr: u128) {}
    fn single_component(
        &mut self,
        _id: &Identifier,
        _typ: &QualifiedComponentType,
        _addr: u128,
    ) {
    }
    fn array_component(
        &mut self,
        _id: &Identifier,
        _typ: &QualifiedComponentType,
        _length: &Number,
        _spacing: &Number,
        _addr: u128,
    ) {
    }
    fn register_component(
        &mut self,
        _id: &Identifier,
        _path: &[Identifier],
        _reg: Arc<Register>,
        _array_index: Option<u128>,
        _addr: u128,
    ) {
    }
    /// Visit a block component. Return value indicates whether to recurse
    /// into the block.
    fn block_component(
        &mut self,
        _id: &Identifier,
        _path: &[Identifier],
        _block: Arc<Block>,
        _array_index: Option<u128>,
        _addr: u128,
    ) -> bool {
        true
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
        let FieldType::User { id } = &field.typ else {
            panic!("expected component")
        };

        assert_eq!(mode, field.mode);

        for (i, p) in type_path.iter().enumerate() {
            assert_eq!(&id.module_path[i].id, p);
        }

        let FieldUserType::Enum(e) = &id.typ;
        assert_eq!(e.id.name, typename);
    }

    fn check_bool_field(field: &Field, name: &str, mode: FieldMode) {
        assert_eq!(name, field.id.name);
        assert_eq!(mode, field.mode);
        assert!(matches!(field.typ, FieldType::Bool));
    }

    fn check_register_block_single_component(
        block_element: &BlockElement,
        name: &str,
        type_path: &[&str],
        typename: &str,
        offset: u128,
    ) {
        let Component::Single { id, typ } = &block_element.component else {
            panic!("expected single component");
        };
        assert_eq!(id.name, name);
        for (i, p) in type_path.iter().enumerate() {
            assert_eq!(&typ.module_path[i].id, p);
        }
        let ComponentUserType::Register(r) = &typ.typ else {
            panic!("expected register component");
        };
        assert_eq!(typename, r.id.name);
        assert_eq!(offset, block_element.offset.value)
    }

    fn check_block_block_array_component(
        block_element: &BlockElement,
        name: &str,
        type_path: &[&str],
        typename: &str,
        count: u128,
        stride: u128,
        offset: u128,
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
        let ComponentUserType::Block(b) = &typ.typ else {
            panic!("expected block component");
        };
        assert_eq!(typename, b.id.name);
        assert_eq!(offset, block_element.offset.value);
        assert_eq!(count, length.value);
        assert_eq!(stride, spacing.value);
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

        // check Phy
        assert_eq!(&resolved.root.blocks[0].id.name, "Phy");
        check_register_block_single_component(
            &resolved.root.blocks[0].elements[0],
            "config",
            &[],
            "PhyConfig",
            0x200,
        );
        check_register_block_single_component(
            &resolved.root.blocks[0].elements[1],
            "status",
            &[],
            "PhyStatus",
            0x400,
        );

        // check Main
        assert_eq!(&resolved.root.blocks[1].id.name, "Main");
        check_block_block_array_component(
            &resolved.root.blocks[1].elements[0],
            "phys",
            &[],
            "Phy",
            4,
            0x1000,
            0x6000,
        );
    }

    #[test]
    fn register_lookup_visitor() {
        let ast = match parse("examples/nic.rsf".into()) {
            Ok(ast) => ast,
            Err(ref e) => {
                panic!("parsing failed: {}", e);
            }
        };

        let resolved =
            ModelModules::resolve(&ast, String::from("")).expect("resolve ast");

        let mut lookup = RegisterLookup::from("phys/1/status");
        resolved.root.accept(&mut lookup);

        let Some(result) = &lookup.result else {
            panic!("lookup for phys/1/status returned no result");
        };

        assert_eq!(result.address, 0x7400);

        println!("{lookup:#?}");
    }
}
