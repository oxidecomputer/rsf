//! RSF text format parser

use std::collections::{BTreeMap, BTreeSet};

use crate::{
    ast::{
        Ast, AstModules, Block, BlockElement, Component, Enum, Field,
        FieldType, Identifier, Number, QualifiedType, Register, Use,
    },
    common::{Alternative, FieldMode, NumberFormat},
};
use anyhow::{Result, anyhow};
use camino::{Utf8Path, Utf8PathBuf};
use winnow::{
    LocatingSlice, ModalResult, Parser,
    ascii::{
        alpha1, alphanumeric1, digit1, hex_digit1, line_ending, multispace0,
        multispace1, newline, till_line_ending,
    },
    combinator::{
        alt, cut_err, delimited, fail, not, repeat, separated, trace,
    },
    error::{ContextError, ErrMode, StrContext, StrContextValue},
};

pub type Input<'i> = LocatingSlice<&'i str>;

#[derive(Debug)]
pub enum Top {
    Comment,
    Use(Use),
    Enum(Enum),
    Register(Register),
    Block(Block),
}

/// Parse the file at the specified path, and recursively parse any used
/// modules.
pub fn parse(filepath: &Utf8Path) -> Result<AstModules> {
    parse_rec(filepath, BTreeSet::default())
}

pub fn parse_rec(
    filepath: &Utf8Path,
    mut seen: BTreeSet<Utf8PathBuf>,
) -> Result<AstModules> {
    if !seen.insert(filepath.to_owned()) {
        let path = seen
            .iter()
            .map(|x| x.file_stem().unwrap_or("?"))
            .collect::<Vec<_>>()
            .join("->");
        let looped = filepath.file_stem().unwrap_or("?");
        return Err(anyhow!(
            "use loop detected along use path {path}->{looped}",
        ));
    }
    let text = std::fs::read_to_string(filepath)?;
    let input = Input::new(text.as_str());
    let ast = parse_ast.parse(input).map_err(|e| anyhow!("{e}"))?;
    let modules_path = filepath.parent().ok_or_else(|| {
        anyhow!("failed to determine modules path from {filepath}")
    })?;
    let mut result = AstModules {
        root: ast.clone(),
        used: BTreeMap::default(),
    };

    for used in &ast.use_statements {
        let mut path = Utf8PathBuf::from(modules_path);
        path.push(format!("{}.rsf", used.module.name));
        let sub = parse_rec(&path, seen.clone())
            .map_err(|e| anyhow!("{}: {e}", used.module.name))?;

        result.used.insert(used.module.name.clone(), sub);
    }
    Ok(result)
}

/// Parse an individual module file into an AST.
pub fn parse_ast(input: &mut Input) -> ModalResult<Ast> {
    let mut ast = Ast::default();
    let top = parse_top(input)?;
    for t in top.into_iter() {
        match t {
            Top::Comment => {}
            Top::Use(x) => ast.use_statements.push(x),
            Top::Enum(x) => ast.enums.push(x),
            Top::Register(x) => ast.registers.push(x),
            Top::Block(x) => ast.blocks.push(x),
        }
    }
    Ok(ast)
}

macro_rules! tr {
    ($name:ident) => {
        trace(stringify!($name), $name)
    };
}

pub fn parse_top(input: &mut Input) -> ModalResult<Vec<Top>> {
    let result = cut_err(repeat(
        1..,
        alt((
            tr!(parse_use_top),
            tr!(parse_enum_top),
            tr!(parse_reg_top),
            tr!(parse_block_top),
            tr!(multi_disc_parser_top),
            fail.context(StrContext::Label("top level element"))
                .context(StrContext::Expected(StrContextValue::Description(
                    "use statement",
                )))
                .context(StrContext::Expected(StrContextValue::Description(
                    "register/block with a documentation comment",
                ))),
        )),
    ))
    .parse_next(input)?;
    Ok(result)
}

pub fn parse_use_top(input: &mut Input) -> ModalResult<Top> {
    Ok(Top::Use(parse_use.parse_next(input)?))
}

pub fn parse_use(input: &mut Input) -> ModalResult<Use> {
    token("use").parse_next(input)?;
    let module = identifier_parser.parse_next(input)?;
    token(";").parse_next(input)?;
    Ok(Use { module })
}

pub fn parse_enum_top(input: &mut Input) -> ModalResult<Top> {
    Ok(Top::Enum(parse_enum.parse_next(input)?))
}

pub fn parse_enum(input: &mut Input) -> ModalResult<Enum> {
    let doc = doc_comment_parser.parse_next(input)?;
    token("enum").parse_next(input)?;
    let mut e = cut_err(parse_enum_cut).parse_next(input)?;
    e.doc = doc;
    Ok(e)
}
pub fn parse_enum_cut(input: &mut Input) -> ModalResult<Enum> {
    let width = delimited("<", number_parser, ">").parse_next(input)?;
    let id = identifier_parser.parse_next(input)?;
    token("{").parse_next(input)?;
    let alternatives =
        separated(1.., parse_enum_alt, token(",")).parse_next(input)?;
    // allow trailing comma
    let _ = token(",").parse_next(input);
    token("}").parse_next(input)?;
    Ok(Enum {
        doc: Vec::default(),
        id,
        width,
        alternatives,
    })
}

pub fn parse_enum_alt(input: &mut Input) -> ModalResult<Alternative> {
    let doc = doc_comment_parser.parse_next(input)?;
    let id = identifier_parser.parse_next(input)?;
    token("=").parse_next(input)?;
    let value = number_parser.parse_next(input)?;
    Ok(Alternative { doc, id, value })
}

pub fn parse_reg_top(input: &mut Input) -> ModalResult<Top> {
    Ok(Top::Register(parse_reg.parse_next(input)?))
}

pub fn parse_reg(input: &mut Input) -> ModalResult<Register> {
    let doc = doc_comment_parser.parse_next(input)?;
    let sram = token("sram").parse_next(input).is_ok();
    token("register").parse_next(input)?;
    let mut reg = cut_err(parse_reg_cut).parse_next(input)?;
    reg.doc = doc;
    reg.sram = sram;
    Ok(reg)
}

pub fn parse_reg_cut(input: &mut Input) -> ModalResult<Register> {
    let width = delimited("<", number_parser, ">").parse_next(input)?;
    let id = identifier_parser.parse_next(input)?;
    let reset_value = match token("reset").parse_next(input) {
        Err(_) => None,
        Ok(_) => Some(delimited("<", number_parser, ">").parse_next(input)?),
    };

    token("{").parse_next(input)?;
    let mut fields = Vec::default();
    // TODO there is probably a better way to do this with repeat combinators?
    // It's easy enough to do in a way that gobbles up errors and reports only
    // the failure to parse the delimiter. But to get a good error we require
    // that once "{" is seen everything that follows is either a valid field
    // or a "}" and to report the error from the `parse_field` parser for
    // anything that is not "}". I don't know how to do that with repeat
    // combinators.
    loop {
        if token("}").parse_next(input).is_ok() {
            return Ok(Register {
                doc: Vec::default(),
                id,
                width,
                sram: false,
                fields,
                reset_value,
            });
        }
        fields.push(parse_field.parse_next(input)?);
        // allow trailing comma
        let _ = token(",").parse_next(input);
    }
}

pub fn parse_field(input: &mut Input) -> ModalResult<Field> {
    let doc = doc_comment_parser
        .context(StrContext::Expected(StrContextValue::Description(
            "docstring for field",
        )))
        .parse_next(input)?;
    let id = identifier_parser.parse_next(input)?;
    token(":").parse_next(input)?;
    let mode = parse_field_mode.parse_next(input)?;
    let typ = parse_field_type.parse_next(input)?;
    let offset = component_offset_parser.parse_next(input)?;
    Ok(Field {
        doc,
        id,
        mode,
        typ,
        offset,
    })
}

pub fn parse_field_type(input: &mut Input) -> ModalResult<FieldType> {
    alt((parse_bitfield_type, parse_bool_type, parse_component_type))
        .parse_next(input)
}

pub fn parse_bool_type(input: &mut Input) -> ModalResult<FieldType> {
    token(parse_bool_type_impl).parse_next(input)?;
    Ok(FieldType::Bool)
}

pub fn parse_bitfield_type(input: &mut Input) -> ModalResult<FieldType> {
    Ok(FieldType::Bitfield {
        width: token(parse_bitfield_type_impl).parse_next(input)?,
    })
}

pub fn parse_component_type(input: &mut Input) -> ModalResult<FieldType> {
    Ok(FieldType::User {
        id: token(parse_qualified_type).parse_next(input)?,
    })
}

pub fn parse_bool_type_impl(input: &mut Input) -> ModalResult<()> {
    "bool".parse_next(input)?;
    Ok(())
}

pub fn parse_bitfield_type_impl(input: &mut Input) -> ModalResult<Number> {
    "b".parse_next(input)?;
    number_parser.parse_next(input)
}

pub fn parse_qualified_type(input: &mut Input) -> ModalResult<QualifiedType> {
    let (path, span) = separated(0.., identifier_parser, token("::"))
        .with_span()
        .parse_next(input)?;
    Ok(QualifiedType {
        path,
        span: span.into(),
    })
}

pub fn parse_field_mode(input: &mut Input) -> ModalResult<FieldMode> {
    alt((
        parse_field_mode_ro,
        parse_field_mode_wo,
        parse_field_mode_rw,
        parse_field_mode_reserved,
    ))
    .parse_next(input)
}

pub fn parse_field_mode_ro(input: &mut Input) -> ModalResult<FieldMode> {
    token("ro").parse_next(input)?;
    Ok(FieldMode::ReadOnly)
}

pub fn parse_field_mode_wo(input: &mut Input) -> ModalResult<FieldMode> {
    token("wo").parse_next(input)?;
    Ok(FieldMode::WriteOnly)
}

pub fn parse_field_mode_rw(input: &mut Input) -> ModalResult<FieldMode> {
    token("rw").parse_next(input)?;
    Ok(FieldMode::ReadWrite)
}

pub fn parse_field_mode_reserved(input: &mut Input) -> ModalResult<FieldMode> {
    token("reserved").parse_next(input)?;
    Ok(FieldMode::Reserved)
}

pub fn parse_block_top(input: &mut Input) -> ModalResult<Top> {
    Ok(Top::Block(parse_block.parse_next(input)?))
}

pub fn parse_block(input: &mut Input) -> ModalResult<Block> {
    let doc = doc_comment_parser.parse_next(input)?;
    let sram = token("sram").parse_next(input).is_ok();
    token("block").parse_next(input)?;
    let mut blk = cut_err(parse_block_cut).parse_next(input)?;
    blk.doc = doc;
    blk.sram = sram;
    Ok(blk)
}

pub fn parse_block_cut(input: &mut Input) -> ModalResult<Block> {
    let id = identifier_parser.parse_next(input)?;

    token("{").parse_next(input)?;
    let elements =
        separated(0.., block_element_parser, token(",")).parse_next(input)?;
    // allow trailing comma
    let _ = token(",").parse_next(input);
    token("}").parse_next(input)?;
    Ok(Block {
        doc: Vec::default(),
        id,
        sram: false,
        elements,
    })
}

pub fn block_element_parser(input: &mut Input) -> ModalResult<BlockElement> {
    let doc = doc_comment_parser.parse_next(input)?;
    let component = component_parser.parse_next(input)?;
    let offset = component_offset_parser.parse_next(input)?;
    Ok(BlockElement {
        doc,
        component,
        offset,
    })
}

pub fn component_parser(input: &mut Input) -> ModalResult<Component> {
    let id = identifier_parser.parse_next(input)?;
    token(":").parse_next(input)?;
    let typ = parse_qualified_type.parse_next(input)?;
    Ok(match component_array_parser.parse_next(input) {
        Ok((length, spacing)) => Component::Array {
            id,
            typ,
            length,
            spacing,
        },
        Err(_) => Component::Single { id, typ },
    })
}

pub fn component_array_parser(
    input: &mut Input,
) -> ModalResult<(Number, Number)> {
    token("[").parse_next(input)?;
    let length = number_parser.parse_next(input)?;
    token(";").parse_next(input)?;
    let spacing = number_parser.parse_next(input)?;
    token("]").parse_next(input)?;
    Ok((length, spacing))
}

pub fn component_offset_parser(input: &mut Input) -> ModalResult<Number> {
    token("@").parse_next(input)?;
    number_parser.parse_next(input)
}

/// Tokens have arbitrary amounts of space on either side.
pub fn token<'s, Output, ParseNext>(
    mut parser: ParseNext,
) -> impl Parser<Input<'s>, Output, ErrMode<ContextError>>
where
    ParseNext: Parser<Input<'s>, Output, ErrMode<ContextError>>,
{
    move |input: &mut Input<'s>| {
        multi_disc_parser.parse_next(input)?;
        let result = parser.parse_next(input)?;
        multi_disc_parser.parse_next(input)?;
        Ok(result)
    }
}

pub fn multi_disc_parser_top(input: &mut Input) -> ModalResult<Top> {
    multi_disc_parser1.parse_next(input)?;
    Ok(Top::Comment)
}

pub fn multi_disc_parser1(input: &mut Input) -> ModalResult<()> {
    repeat(1.., disc_parser).parse_next(input)
}

pub fn multi_disc_parser(input: &mut Input) -> ModalResult<()> {
    repeat(0.., disc_parser).parse_next(input)
}

pub fn disc_parser(input: &mut Input) -> ModalResult<()> {
    alt((line_comment_disc_parser, space_disc_parser)).parse_next(input)?;
    Ok(())
}

pub fn line_comment_disc_parser(input: &mut Input) -> ModalResult<()> {
    let _ = line_comment_parser.parse_next(input)?;
    Ok(())
}

pub fn space_disc_parser(input: &mut Input) -> ModalResult<()> {
    let _ = multispace1.parse_next(input)?;
    Ok(())
}

pub fn doc_comment_parser(input: &mut Input) -> ModalResult<Vec<String>> {
    let lines: Vec<String> =
        separated(1.., doc_comment_line_parser, newline).parse_next(input)?;
    Ok(lines)
}

pub fn doc_comment_line_parser(input: &mut Input) -> ModalResult<String> {
    let _ = multispace0.parse_next(input)?;
    let _ = "///".parse_next(input)?;
    let s = till_line_ending.parse_next(input)?;
    Ok(s.to_owned())
}

/// Parse c-style a line comment.
pub fn line_comment_parser(input: &mut Input) -> ModalResult<Top> {
    let _ = multispace0.parse_next(input)?;
    let _ = "//".parse_next(input)?;
    not("/").parse_next(input)?;
    let _ = till_line_ending.parse_next(input)?;
    let _ = line_ending.parse_next(input)?;
    Ok(Top::Comment)
}

pub fn line_comments_parser(input: &mut Input) -> ModalResult<Vec<Top>> {
    repeat(0.., token(line_comment_parser)).parse_next(input)
}

/// Parse an identifier.
pub fn identifier_parser<'s>(input: &mut Input<'s>) -> ModalResult<Identifier> {
    trace("identifier_parser", move |input: &mut Input<'s>| {
        let (ident, span) = token((alt((alpha1, "_")), alphanumunder0))
            .with_span()
            .parse_next(input)?;
        Ok(Identifier {
            name: format!("{}{}", ident.0, ident.1),
            span: span.into(),
        })
    })
    .parse_next(input)
}

/// Parse a series of alphanumeric chracters or underscore.
pub fn alphanumunder0(input: &mut Input) -> ModalResult<String> {
    let result = repeat(0.., alt((alphanumeric1, "_"))).parse_next(input)?;
    Ok(result)
}

pub fn number_parser(input: &mut Input) -> ModalResult<Number> {
    let ((value, format), span) =
        number_parser_impl.with_span().parse_next(input)?;
    Ok(Number {
        value,
        format,
        span: span.into(),
    })
}

pub fn number_parser_impl(
    input: &mut Input,
) -> ModalResult<(u128, NumberFormat)> {
    if token("0x").parse_next(input).is_ok() {
        let s = hex_digit1.parse_next(input)?;
        let n = u128::from_str_radix(s, 16).unwrap();
        Ok((n, NumberFormat::Hex { digits: s.len() }))
    } else if token("0b").parse_next(input).is_ok() {
        let s: String = repeat(1.., alt(("0", "1"))).parse_next(input)?;
        let n = u128::from_str_radix(&s, 2).unwrap();
        Ok((n, NumberFormat::Binary { digits: s.len() }))
    } else {
        let s = digit1.parse_next(input)?;
        let n: u128 = s.parse().unwrap();
        Ok((n, NumberFormat::Decimal { digits: s.len() }))
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::common::Span;

    #[test]
    fn nic_example_parse() {
        let text = std::fs::read_to_string("examples/nic.rsf").unwrap();
        let s = Input::new(text.as_str());
        let ast = match parse_ast.parse(s) {
            Ok(ast) => ast,
            Err(ref e) => {
                panic!("parsing failed: {e}");
            }
        };
        assert_eq!(ast.use_statements.len(), 3);
        assert_eq!(ast.use_statements[0].module.name, "ethernet");
        assert_eq!(ast.use_statements[1].module.name, "cei");
        assert_eq!(ast.use_statements[2].module.name, "version");

        assert_eq!(ast.enums.len(), 1);
        assert_eq!(ast.enums[0].id.name, "Lanes");
        assert_eq!(ast.enums[0].width.value, 2);
        assert_eq!(ast.enums[0].alternatives.len(), 5);
        assert_eq!(ast.enums[0].alternatives[0].id.name, "Single");
        assert_eq!(ast.enums[0].alternatives[1].id.name, "L2");
        assert_eq!(ast.enums[0].alternatives[2].id.name, "L4");
        assert_eq!(ast.enums[0].alternatives[3].id.name, "F2");
        assert_eq!(ast.enums[0].alternatives[4].id.name, "F4");

        assert_eq!(ast.registers.len(), 5);
        assert_eq!(ast.registers[0].id.name, "PhyConfig");
        assert_eq!(ast.registers[0].width.value, 32);
        assert_eq!(ast.registers[0].fields.len(), 5);
        assert_eq!(ast.registers[0].fields[0].id.name, "speed");
        assert_eq!(ast.registers[0].fields[0].mode, FieldMode::ReadWrite);
        assert_eq!(
            ast.registers[0].fields[0].typ,
            FieldType::User {
                id: QualifiedType::from(vec!["ethernet", "DataRate"])
            }
        );
        assert_eq!(ast.registers[0].fields[1].id.name, "reach");
        assert_eq!(ast.registers[0].fields[1].mode, FieldMode::ReadWrite);
        assert_eq!(
            ast.registers[0].fields[1].typ,
            FieldType::User {
                id: QualifiedType::from(vec!["ethernet", "Reach"])
            }
        );
        assert_eq!(ast.registers[0].fields[2].id.name, "lanes");
        assert_eq!(ast.registers[0].fields[2].mode, FieldMode::ReadWrite);
        assert_eq!(
            ast.registers[0].fields[2].typ,
            FieldType::User {
                id: QualifiedType::from(vec!["Lanes"])
            }
        );
        assert_eq!(ast.registers[0].fields[3].id.name, "fec");
        assert_eq!(ast.registers[0].fields[3].mode, FieldMode::ReadWrite);
        assert_eq!(
            ast.registers[0].fields[3].typ,
            FieldType::User {
                id: QualifiedType::from(vec!["ethernet", "Fec"])
            }
        );
        assert_eq!(ast.registers[0].fields[4].id.name, "modulation");
        assert_eq!(ast.registers[0].fields[4].mode, FieldMode::ReadWrite);
        assert_eq!(
            ast.registers[0].fields[4].typ,
            FieldType::User {
                id: QualifiedType::from(vec!["cei", "Modulation"])
            }
        );
        assert_eq!(ast.registers[1].id.name, "PhyStatus");
        assert_eq!(ast.registers[1].width.value, 32);
        assert_eq!(ast.registers[1].fields.len(), 3);
        assert_eq!(ast.registers[1].fields[0].id.name, "carrier");
        assert_eq!(ast.registers[1].fields[0].mode, FieldMode::ReadOnly);
        assert_eq!(ast.registers[1].fields[0].typ, FieldType::Bool);
        assert_eq!(ast.registers[1].fields[1].id.name, "signal_error");
        assert_eq!(ast.registers[1].fields[1].mode, FieldMode::ReadOnly);
        assert_eq!(ast.registers[1].fields[1].typ, FieldType::Bool);
        assert_eq!(ast.registers[1].fields[2].id.name, "data_valid");
        assert_eq!(ast.registers[1].fields[2].mode, FieldMode::ReadOnly);
        assert_eq!(ast.registers[1].fields[2].typ, FieldType::Bool);

        assert_eq!(ast.blocks[0].id.name, "Main");
        assert_eq!(ast.blocks[0].elements.len(), 3);
        assert_eq!(
            ast.blocks[0].elements[0].component,
            Component::Array {
                id: Identifier::new("phys"),
                typ: QualifiedType::from(vec!["Phy"]),
                length: Number {
                    value: 4,
                    format: NumberFormat::Decimal { digits: 1 },
                    span: Span::Any
                },
                spacing: Number {
                    value: 0x1000,
                    format: NumberFormat::Hex { digits: 4 },
                    span: Span::Any
                },
            }
        );
        assert_eq!(
            ast.blocks[0].elements[0].offset,
            Number {
                value: 0x6000,
                format: NumberFormat::Hex { digits: 4 },
                span: Span::Any,
            },
        );

        assert_eq!(ast.blocks.len(), 3);
        assert_eq!(ast.blocks[1].id.name, "Phy");
        assert_eq!(ast.blocks[1].elements.len(), 4);
        assert_eq!(
            ast.blocks[1].elements[0].component,
            Component::Single {
                id: Identifier::new("config"),
                typ: QualifiedType::from(vec!["PhyConfig"])
            }
        );
        assert_eq!(
            ast.blocks[1].elements[0].offset,
            Number {
                value: 0x200,
                format: NumberFormat::Hex { digits: 3 },
                span: Span::Any,
            },
        );
        assert_eq!(
            ast.blocks[1].elements[1].component,
            Component::Single {
                id: Identifier::new("status"),
                typ: QualifiedType::from(vec!["PhyStatus"])
            }
        );
        assert_eq!(
            ast.blocks[1].elements[1].offset,
            Number {
                value: 0x400,
                format: NumberFormat::Hex { digits: 3 },
                span: Span::Any,
            },
        );

        println!("{ast:#?}")
    }
}
