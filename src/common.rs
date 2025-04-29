use colored::*;
use std::{fmt::Display, ops::Range};

#[derive(Debug, Clone)]
pub struct Enum {
    pub id: Identifier,
    pub width: Number,
    pub alternatives: Vec<Alternative>,
}

#[derive(Debug, Clone)]
pub struct Alternative {
    pub id: Identifier,
    pub value: Number,
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

#[derive(Debug, Clone)]
pub struct Field<T> {
    pub id: Identifier,
    pub mode: FieldMode,
    pub typ: T,
}

#[derive(Debug, Clone)]
pub struct Register<T> {
    pub id: Identifier,
    pub width: Number,
    pub fields: Vec<Field<T>>,
}

#[derive(Debug, Clone)]
pub struct Block<T> {
    pub id: Identifier,
    pub elements: Vec<BlockElement<T>>,
}

#[derive(Debug, Clone)]
pub struct BlockElement<T> {
    pub component: Component<T>,
    pub offset: Option<Number>,
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
        spacing: Option<Number>,
    },
}

pub trait Typename {
    fn typename(&self) -> String;
}

#[derive(Debug, PartialEq, Clone)]
pub enum Type<T: Display + Typename> {
    Bool,
    Bitfield { width: Number },
    Component { id: T },
    Ellipsis,
}

impl<T: Display + Typename> Display for Type<T> {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Bool => write!(f, "{}", "bool".cyan()),
            Self::Bitfield { width } => {
                write!(f, "{}{}", "b".cyan(), width.value.to_string().cyan())
            }
            Self::Component { id } => write!(f, "{}", id.typename()),
            Self::Ellipsis => write!(f, "{}", "...".cyan()),
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
    pub value: u128,
    pub span: Span,
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
