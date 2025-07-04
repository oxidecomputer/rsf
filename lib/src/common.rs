use colored::*;
use std::{fmt::Display, ops::Range};

use crate::ast::Emit;

#[derive(Debug, Clone, PartialEq)]
pub struct Enum {
    pub doc: Vec<String>,
    pub id: Identifier,
    pub width: Number,
    pub alternatives: Vec<Alternative>,
}

impl Emit for Enum {
    fn emit(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        for x in &self.doc {
            writeln!(f, "///{x}")?;
        }
        writeln!(f, "enum<{}> {} {{", self.width.to_code(), self.id.name)?;
        for x in &self.alternatives {
            x.emit(f)?;
        }
        writeln!(f, "}}")?;
        Ok(())
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Alternative {
    pub doc: Vec<String>,
    pub id: Identifier,
    pub value: Number,
}

impl Emit for Alternative {
    fn emit(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        for x in &self.doc {
            writeln!(f, "    ///{x}")?;
        }
        writeln!(f, "    {} = {},", self.id.name, self.value.to_code())
    }
}

impl Display for Alternative {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{} {} {}",
            self.id.name,
            "=".dimmed(),
            self.value.value.to_string().yellow()
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum FieldMode {
    ReadOnly,
    WriteOnly,
    ReadWrite,
    Reserved,
}

impl Emit for FieldMode {
    fn emit(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self {
            Self::ReadOnly => write!(f, "ro"),
            Self::WriteOnly => write!(f, "wo"),
            Self::ReadWrite => write!(f, "rw"),
            Self::Reserved => write!(f, "reserved"),
        }
    }
}

impl Display for FieldMode {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::ReadOnly => write!(f, "ro"),
            Self::WriteOnly => write!(f, "wo"),
            Self::ReadWrite => write!(f, "rw"),
            Self::Reserved => write!(f, "reserved"),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Field<T> {
    pub doc: Vec<String>,
    pub id: Identifier,
    pub mode: FieldMode,
    pub typ: T,
    pub offset: Number,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Register<T> {
    pub doc: Vec<String>,
    pub id: Identifier,
    pub width: Number,
    pub reset_value: Option<Number>,
    pub sram: bool,
    pub fields: Vec<Field<T>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Block<T> {
    pub doc: Vec<String>,
    pub id: Identifier,
    pub sram: bool,
    pub elements: Vec<BlockElement<T>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct BlockElement<T> {
    pub doc: Vec<String>,
    pub component: Component<T>,
    pub offset: Number,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Component<T> {
    Single {
        id: Identifier,
        typ: T,
    },
    Array {
        id: Identifier,
        typ: T,
        length: Number,
        spacing: Number,
    },
}

pub trait Typename {
    fn typename(&self) -> String;
}

#[derive(Debug, PartialEq, Clone)]
pub enum FieldType<T: Display + Typename> {
    Bool,
    Bitfield { width: Number },
    User { id: T },
}

impl<T: Display + Typename> Display for FieldType<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "{}", "bool".cyan()),
            Self::Bitfield { width } => {
                write!(f, "{}{}", "b".cyan(), width.value.to_string().cyan())
            }
            Self::User { id } => write!(f, "{}", id.typename()),
        }
    }
}

#[derive(Default, Debug, Clone, PartialEq)]
pub struct Identifier {
    pub name: String,
    pub span: Span,
}

impl Identifier {
    pub fn new(name: &str) -> Self {
        Identifier {
            name: name.to_owned(),
            span: Span::Any,
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Number {
    pub format: NumberFormat,
    pub value: u128,
    pub span: Span,
}

#[derive(Debug, PartialEq, Clone, Copy)]
pub enum NumberFormat {
    Binary { digits: usize },
    Hex { digits: usize },
    Decimal { digits: usize },
}

impl NumberFormat {
    pub fn binary() -> Self {
        Self::Binary { digits: 0 }
    }

    pub fn hex() -> Self {
        Self::Hex { digits: 0 }
    }

    pub fn decimal() -> Self {
        Self::Decimal { digits: 0 }
    }
}

impl Number {
    pub fn new(value: u128, format: NumberFormat) -> Self {
        Self {
            value,
            format,
            span: Span::default(),
        }
    }
}

impl Emit for Number {
    fn emit(&self, f: &mut impl std::fmt::Write) -> std::fmt::Result {
        match self.format {
            NumberFormat::Binary { digits } => {
                write!(f, "0b{:0width$b}", self.value, width = digits)
            }
            NumberFormat::Hex { digits } => {
                write!(f, "0x{:0width$x}", self.value, width = digits)
            }
            NumberFormat::Decimal { digits } => {
                write!(f, "{:0width$}", self.value, width = digits)
            }
        }
    }
}

#[derive(Debug, Clone, Default)]
pub enum Span {
    #[default]
    Any,
    Range(Range<usize>),
}

impl PartialEq for Span {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            (Self::Any, _) => true,
            (_, Self::Any) => true,
            (Self::Range(a), Self::Range(b)) => a == b,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span::Range(value)
    }
}
