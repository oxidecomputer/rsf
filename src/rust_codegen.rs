//! Rust code generation

use crate::common::{FieldMode, NumberFormat, Typename};
use crate::model::{Block, Component, FieldType, FieldUserType, Register};
use crate::model::{ModelModules, Visitor};
use anyhow::{Result, anyhow};
use camino::Utf8Path;
use camino_tempfile::NamedUtf8TempFile;
use convert_case::{Case, Casing};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::BTreeMap;
use std::io::Write;
use std::str::FromStr;
use std::sync::Arc;

#[derive(Default)]
struct CodegenVisitor {
    addr_type: TokenStream,
    value_type: TokenStream,
    prelude: TokenStream,
    register_definitions: TokenStream,
    enum_definitions: TokenStream,
    block_definitions: TokenStream,
    block_methods: BTreeMap<String, TokenStream>,
    current_block: String,
}

impl CodegenVisitor {
    fn tokens(self) -> TokenStream {
        let mut tokens = self.prelude;
        tokens.extend(self.register_definitions);
        tokens.extend(self.enum_definitions);
        tokens.extend(self.block_definitions);
        for (name, block_tokens) in self.block_methods {
            let name = match name.as_str() {
                "Client" => format_ident!("Client"),
                _ => {
                    format_ident!("{}Instance", name.to_case(Case::Pascal))
                }
            };

            tokens.extend(quote! {
                impl #name {
                    #block_tokens
                }
            })
        }
        tokens
    }
}

impl Visitor for CodegenVisitor {
    fn register(&mut self, reg: Arc<Register>) {
        let name = format_ident!("{}", reg.id.name.to_case(Case::Pascal));
        let width = proc_macro2::Literal::u128_unsuffixed(reg.width.value);
        let mut fields = TokenStream::default();
        for f in &reg.fields {
            let doc = f.doc.join("\n");

            let getter = format_ident!("get_{}", f.id.name);
            let setter = format_ident!("set_{}", f.id.name);

            let offset = proc_macro2::Literal::u128_unsuffixed(f.offset.value);

            match &f.typ {
                FieldType::Bool => {
                    let width = proc_macro2::Literal::u128_unsuffixed(1);
                    if f.mode == FieldMode::ReadOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #getter(&self) -> bool {
                                self.0.get_field::<#width, #offset>().unwrap().to_bool()
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #setter(&mut self, data__: bool) {
                                self.0.set_field::<#width, #offset>(BitSet::<#width>::from_bool(data__)).unwrap();
                            }
                        })
                    }
                }
                FieldType::Bitfield { width } => {
                    let width =
                        proc_macro2::Literal::u128_unsuffixed(width.value);
                    if f.mode == FieldMode::ReadOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #getter(&self) -> BitSet<#width> {
                                self.0.get_field::<#width, #offset>().unwrap()
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #setter(&mut self, data__: BitSet<#width>) {
                                self.0.set_field::<#width, #offset>(data__).unwrap();
                            }
                        })
                    }
                }
                FieldType::User { id } => {
                    let FieldUserType::Enum(e) = &id.typ;
                    let width =
                        proc_macro2::Literal::u128_unsuffixed(e.width.value);
                    let typename = if id.module_path.is_empty() {
                        let ident = format_ident!(
                            "{}",
                            e.id.name.to_case(Case::Pascal)
                        );
                        quote! { #ident }
                    } else {
                        let mut parts = id
                            .module_path
                            .iter()
                            .filter(|x| !x.id.is_empty())
                            .map(|x| {
                                format_ident!("{}", x.id.to_case(Case::Snake))
                            })
                            .collect::<Vec<_>>();
                        parts.push(format_ident!(
                            "{}",
                            e.id.name.to_case(Case::Pascal)
                        ));
                        quote! { #(#parts)::* }
                    };

                    if f.mode == FieldMode::ReadOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #getter(&self) -> #typename {
                                self.0.get_field::<#width, #offset>().unwrap().try_into().unwrap()
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #setter(&mut self, data__: #typename) {
                                self.0.set_field::<#width, #offset>(data__.into()).unwrap();
                            }
                        })
                    }
                }
            }
        }
        let addr_type = &self.addr_type;
        let value_type = &self.value_type;

        let instance_name =
            format_ident!("{}Instance", reg.id.name.to_case(Case::Pascal));

        let doc = reg.doc.join("\n");
        let instance_doc = format!("Instance of a [`{}`]", reg.id.name);

        self.register_definitions.extend(quote! {

            #[derive(Default, Debug)]
            #[doc = #doc]
            pub struct #name(BitSet<#width>);

            impl #name {
                #fields
                pub fn reset(&mut self) {
                    self.0 = BitSet::<#width>::ZERO;
                }
            }

            impl From<#value_type> for #name {
                fn from(value: #value_type) -> Self {
                    //TODO should be fallible
                    Self(BitSet::<#width>::from_int(value).unwrap())
                }
            }

            impl From<#name> for #value_type {
                fn from(value: #name) -> Self {
                    //TODO it's not obvious without looking here that value_type
                    // should be an integer of some kind
                    value.0.to_int()
                }
            }

            #[doc = #instance_doc]
            pub struct #instance_name {
                pub addr: #addr_type,
            }

            impl rust_rpi::RegisterInstance<#name, #addr_type, #value_type> for #instance_name {
                fn read(
                    &self,
                    platform: &impl rust_rpi::Platform<#addr_type, #value_type>,
                ) -> Result<#name> {
                    platform.read(self.addr)
                }
                fn write(
                    &self,
                    platform: &impl rust_rpi::Platform<#addr_type, #value_type>,
                    value: #name,
                ) -> Result<()> {
                    platform.write(self.addr, value)
                }
                fn try_update<F: FnOnce(&mut #name) -> Result<()>>(
                    &self,
                    platform: &impl rust_rpi::Platform<#addr_type, #value_type>,
                    f: F,
                ) -> Result<()> {
                    let mut value = self.read(platform)?;
                    f(&mut value)?;
                    self.write(platform, value)
                }
                fn update<F: FnOnce(&mut #name)>(
                    &self,
                    platform: &impl rust_rpi::Platform<#addr_type, #value_type>,
                    f: F,
                ) -> Result<()> {
                    let mut value = self.read(platform)?;
                    f(&mut value);
                    self.write(platform, value)
                }
            }

        })
    }

    fn enumeration(&mut self, e: Arc<crate::ast::Enum>) {
        let name = format_ident!("{}", e.id.name.to_case(Case::Pascal));
        let repr = match e.width.value {
            x if x < 8 => quote! { u8 },
            x if x < 16 => quote! { u16 },
            x if x < 32 => quote! { u32 },
            x if x < 64 => quote! { u64 },
            x if x < 128 => quote! { u128 },
            _ => panic!("enums cannot be more than 128 bits wide"),
        };
        let doc = e.doc.join("\n");

        let mut alts = TokenStream::default();
        for a in &e.alternatives {
            let doc = a.doc.join("\n");
            let alt_name = format_ident!("{}", a.id.name.to_case(Case::Pascal));
            let alt_value = match &a.value.format {
                NumberFormat::Binary { digits } => {
                    proc_macro2::Literal::from_str(&format!(
                        "0b{:0width$b}",
                        a.value.value,
                        width = digits,
                    ))
                }
                NumberFormat::Hex { digits } => proc_macro2::Literal::from_str(
                    &format!("0x{:0width$x}", a.value.value, width = digits,),
                ),
                NumberFormat::Decimal { digits } => {
                    proc_macro2::Literal::from_str(&format!(
                        "{:0width$}",
                        a.value.value,
                        width = digits,
                    ))
                }
            }
            .unwrap();
            alts.extend(quote! {
                #[doc = #doc]
                #alt_name = #alt_value,
            });
        }

        let width = proc_macro2::Literal::u128_unsuffixed(e.width.value);

        self.enum_definitions.extend(quote! {
            #[doc = #doc]
            #[derive(num_enum::TryFromPrimitive, PartialEq, Debug)]
            #[repr(#repr)]
            pub enum #name {
                #alts
            }

            impl From<#name> for BitSet<#width> {
                fn from(value: #name) -> BitSet<#width> {
                    BitSet::<#width>::try_from(value as #repr).unwrap()
                }
            }

            impl TryFrom<BitSet<#width>> for #name {
                type Error = anyhow::Error;

                fn try_from(value: BitSet<#width>) -> Result<Self> {
                    Ok(Self::try_from(value.to_int())?)
                }
            }
        });
    }

    fn block(&mut self, block: Arc<Block>, _addr: u128) {
        let (name, block_name) = match block.id.name.as_str() {
            "Main" => ("Client", format_ident!("Client")),
            other => (
                other,
                format_ident!("{}Instance", other.to_case(Case::Pascal)),
            ),
        };

        self.current_block = name.to_owned();

        let addr_type = &self.addr_type;
        let doc = block.doc.join("\n");

        self.block_definitions.extend(quote! {
            #[doc = #doc]
            #[derive(Default, Debug)]
            pub struct #block_name{
                pub addr: #addr_type
            }
        });

        for element in &block.elements {
            let doc = element.doc.join("\n");
            let mut tokens = self
                .block_methods
                .get(&self.current_block)
                .cloned()
                .unwrap_or_default();

            let offset = proc_macro2::Literal::from_str(&format!(
                "0x{:x}",
                element.offset.value
            ))
            .unwrap();

            match &element.component {
                Component::Single { id, typ } => {
                    let method_name =
                        format_ident!("{}", id.name.to_case(Case::Snake));
                    let type_name =
                        typename_to_qualified_ident(typ, "Instance");
                    tokens.extend(quote! {
                        #[doc = #doc]
                        pub fn #method_name(&self) -> #type_name {
                            #type_name {
                                addr: self.addr + #offset,
                            }
                        }
                    });
                    self.block_methods
                        .insert(self.current_block.clone(), tokens);
                }
                Component::Array {
                    id,
                    typ,
                    length: _,
                    spacing,
                } => {
                    let method_name =
                        format_ident!("{}", id.name.to_case(Case::Snake));
                    let type_name =
                        typename_to_qualified_ident(typ, "Instance");
                    let spacing = proc_macro2::Literal::from_str(&format!(
                        "0x{:x}",
                        spacing.value
                    ))
                    .unwrap();
                    tokens.extend(quote! {
                        #[doc = #doc]
                        pub fn #method_name(&self, index: #addr_type) -> #type_name {
                            #type_name {
                                addr: self.addr + #offset + (index * #spacing)
                            }
                        }
                    });
                    self.block_methods
                        .insert(self.current_block.clone(), tokens);
                }
            }
        }
    }

    fn block_component(
        &mut self,
        _id: &crate::ast::Identifier,
        _path: &[crate::ast::Identifier],
        block: Arc<Block>,
        _array_index: Option<u128>,
        _addr: u128,
    ) -> bool {
        let name = match block.id.name.as_str() {
            "Main" => "Client",
            other => other,
        };
        self.current_block = name.to_owned();
        true
    }
}

/// The address type to be used for register access in generated code.
pub enum AddrType {
    U8,
    U16,
    U32,
    U64,
    U128,
}

impl From<AddrType> for TokenStream {
    fn from(value: AddrType) -> Self {
        match value {
            AddrType::U8 => quote! { u8 },
            AddrType::U16 => quote! { u16 },
            AddrType::U32 => quote! { u32 },
            AddrType::U64 => quote! { u64 },
            AddrType::U128 => quote! { u128 },
        }
    }
}

/// The value type to be used for register access in generated code.
pub enum ValueType {
    U8,
    U16,
    U32,
    U64,
    U128,
}

impl From<ValueType> for TokenStream {
    fn from(value: ValueType) -> Self {
        match value {
            ValueType::U8 => quote! { u8 },
            ValueType::U16 => quote! { u16 },
            ValueType::U32 => quote! { u32 },
            ValueType::U64 => quote! { u64 },
            ValueType::U128 => quote! { u128 },
        }
    }
}

pub fn codegen(
    file: &Utf8Path,
    addr_type: AddrType,
    value_type: ValueType,
) -> Result<String> {
    codegen_internal(file, addr_type, value_type, false)
}

pub fn codegen_internal(
    file: &Utf8Path,
    addr_type: AddrType,
    value_type: ValueType,
    local_dev: bool,
) -> Result<String> {
    let ast = crate::parser::parse(file)?;
    let resolved = ModelModules::resolve(&ast, String::default())?;
    generate_rpi(&resolved, addr_type.into(), value_type.into(), local_dev)
}

pub fn generate_rpi(
    model: &ModelModules,
    addr_type: TokenStream,
    value_type: TokenStream,
    local_dev: bool,
) -> Result<String> {
    let tokens = generate_rpi_rec(model, addr_type, value_type, local_dev)?;

    let file: syn::File = syn::parse2(tokens.clone()).map_err(|e| {
        let generated = tokens
            .to_string()
            .replace(";", ";\n")
            .replace("{", "{\n")
            .replace("}", "}\n");

        let tmp = NamedUtf8TempFile::new().unwrap();
        let (mut file, path) = tmp.keep().unwrap();
        file.write_all(generated.as_bytes()).unwrap();
        anyhow!(
            "token parsing failed: {e:#?}, rust source file written to {}.
            Try running rustfmt over that file to see whare the issues are.",
            path,
        )
    })?;
    let code = prettyplease::unparse(&file);
    Ok(code)
}

pub fn generate_rpi_rec(
    model: &ModelModules,
    addr_type: TokenStream,
    value_type: TokenStream,
    local_dev: bool,
) -> Result<TokenStream> {
    let mut cgv = CodegenVisitor {
        addr_type: addr_type.clone(),
        value_type: value_type.clone(),
        prelude: use_statements(local_dev),
        ..Default::default()
    };
    model.root.accept(&mut cgv);
    let mut tokens = cgv.tokens();

    for (name, module) in &model.used {
        let model_tokens = generate_rpi_rec(
            module,
            addr_type.clone(),
            value_type.clone(),
            local_dev,
        )?;
        let modname = format_ident!("{}", name.to_case(Case::Snake));
        tokens.extend(quote! {
            pub mod #modname {
                #model_tokens
            }
        })
    }

    Ok(tokens)
}

fn use_statements(local_dev: bool) -> TokenStream {
    let mut tokens = quote! {
        use bitset::BitSet;
        use anyhow::Result;
    };
    if !local_dev {
        tokens.extend(quote! {
            use rsf::rust_rpi;
        });
    }
    tokens
}

fn typename_to_qualified_ident(
    tn: &impl Typename,
    suffix: &str,
) -> TokenStream {
    let typename = tn.typename();
    let parts = typename.split("::").collect::<Vec<_>>();

    let Some((typename, path)) = parts.split_last() else {
        panic!("empty split!? from: {typename}");
    };

    let typ = format_ident!("{}{}", typename.to_case(Case::Pascal), suffix);

    // We only care about the last module in the path. While a module path can
    // be arbitrarily deep depending on RSF source organization, the actual
    // module definitions are not nested, it's just a single flat module
    // definition space defined by a collection of RSF files.
    let Some(last) = path.last() else {
        return quote! { #typ };
    };

    let last = format_ident!("{}", last.to_case(Case::Snake));

    let _parts = path
        .iter()
        .map(|x| format_ident!("{}", x.to_case(Case::Snake)))
        .collect::<Vec<_>>();

    quote! { #last::#typ }
}

#[cfg(test)]
mod test {
    use super::*;
    use expectorate::assert_contents;

    // Test code generation in terms of expected syntax.
    #[test]
    fn test_codegen() {
        let output = match codegen_internal(
            "examples/nic.rsf".into(),
            AddrType::U32,
            ValueType::U32,
            true,
        ) {
            Ok(out) => out,
            Err(ref e) => {
                panic!("codegen failed: {e}");
            }
        };
        assert_contents("test_data/nic_rpi.rs", &output);
    }

    // Kersplat generated code right here!
    #[allow(dead_code)]
    #[cfg(feature = "test_generated")]
    mod generated {
        use crate::rust_rpi;
        include!("../test_data/nic_rpi.rs");
    }

    // Run a test program against generated code
    #[cfg(feature = "test_generated")]
    #[test]
    fn run_generated_code() {
        use crate::rust_rpi::{DummyPlatform, RegisterInstance};
        use generated::*;

        // Initialize the underlying platform. For testing this is just a
        // dummy platform.
        let platform = DummyPlatform::default();

        // Create the RPI client
        let rpi = Client::default();

        // Poke at some config
        rpi.phys(1)
            .config()
            .update(&platform, |c: &mut PhyConfig| {
                c.set_speed(ethernet::DataRate::G50);
            })
            .unwrap();

        // Read some config, status
        let config = rpi.phys(1).config().read(&platform).unwrap();
        assert_eq!(config.get_speed(), ethernet::DataRate::G50);

        let status = rpi.phys(2).status().read(&platform).unwrap();
        assert!(!status.get_carrier());
    }
}
