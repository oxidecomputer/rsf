use std::ops::Range;

#[derive(Debug)]
pub struct Enum {
    pub id: Identifier,
    pub width: Number,
    pub alternatives: Vec<Alternative>,
}

#[derive(Debug)]
pub struct Alternative {
    pub id: Identifier,
    pub value: Number,
}

#[derive(Debug, PartialEq)]
pub enum FieldMode {
    ReadOnly,
    WriteOnly,
    ReadWrite,
    Reserved,
}

#[derive(Debug)]
pub struct Field<T> {
    pub id: Identifier,
    pub mode: FieldMode,
    pub typ: T,
}

#[derive(Debug)]
pub struct Register<T> {
    pub id: Identifier,
    pub width: Number,
    pub fields: Vec<Field<T>>,
}

#[derive(Debug)]
pub struct Block<T> {
    pub id: Identifier,
    pub elements: Vec<BlockElement<T>>,
}

#[derive(Debug)]
pub struct BlockElement<T> {
    pub component: Component<T>,
    pub offset: Option<Number>,
}

#[derive(Debug, PartialEq)]
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

#[derive(Debug, PartialEq)]
pub enum Type<T> {
    Bool,
    Bitfield { width: Number },
    Component { id: T },
    Ellipsis,
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

#[derive(Debug, PartialEq)]
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
            (a, b) => a == b,
        }
    }
}

impl From<Range<usize>> for Span {
    fn from(value: Range<usize>) -> Self {
        Span::Range(value)
    }
}
