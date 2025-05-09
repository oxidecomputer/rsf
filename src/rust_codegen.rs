//! Rust code generation

use crate::common::{FieldMode, NumberFormat, Typename};
use crate::model::{Block, Component, FieldType, FieldUserType, Register};
use crate::model::{ModelModules, Visitor};
use anyhow::Result;
use convert_case::{Case, Casing};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::collections::BTreeMap;
use std::str::FromStr;
use std::sync::Arc;

#[derive(Default)]
struct CodegenVisitor {
    addr_type: TokenStream,
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
                            pub fn #getter(&self) -> bool {
                                self.0.get_field::<#width, #offset>().unwrap().to_bool()
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
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
                            pub fn #getter(&self) -> BitSet<#width> {
                                self.0.get_field::<#width, #offset>().unwrap()
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
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
                            pub fn #getter(&self) -> #typename {
                                self.0.get_field::<#width, #offset>().unwrap().try_into().unwrap()
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            pub fn #setter(&mut self, data__: #typename) {
                                self.0.set_field::<#width, #offset>(data__.into()).unwrap();
                            }
                        })
                    }
                }
            }
        }
        let addr_type = &self.addr_type;

        let instance_name =
            format_ident!("{}Instance", reg.id.name.to_case(Case::Pascal));

        self.register_definitions.extend(quote! {
            #[derive(Default, Debug)]
            pub struct #name(BitSet<#width>);
            impl #name {
                #fields
            }
            pub struct #instance_name {
                addr: #addr_type,
            }

            impl rust_rpi::RegisterInstance<#name, #addr_type> for #instance_name {
                fn read(
                    &self,
                    platform: &impl rust_rpi::Platform<#addr_type>,
                ) -> Result<#name> {
                    platform.read(self.addr)
                }
                fn write(
                    &self,
                    platform: &impl rust_rpi::Platform<#addr_type>,
                    value: #name,
                ) -> Result<()> {
                    platform.write(self.addr, value)
                }
                fn update(
                    &self,
                    platform: &impl rust_rpi::Platform<#addr_type>,
                    f: fn(&mut #name) -> Result<()>
                ) -> Result<()> {
                    let mut value = self.read(platform)?;
                    f(&mut value)?;
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

        let mut alts = TokenStream::default();
        for a in &e.alternatives {
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
                #alt_name = #alt_value,
            });
        }

        let width = proc_macro2::Literal::u128_unsuffixed(e.width.value);

        self.enum_definitions.extend(quote! {
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

        self.block_definitions.extend(quote! {
            #[derive(Default, Debug)]
            pub struct #block_name{ addr: #addr_type }
        });

        for element in &block.elements {
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
                    let type_name = format_ident!(
                        "{}Instance",
                        typ.typename().to_case(Case::Pascal)
                    );
                    tokens.extend(quote! {
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
                    let type_name = format_ident!(
                        "{}Instance",
                        typ.typename().to_case(Case::Pascal)
                    );
                    let spacing = proc_macro2::Literal::from_str(&format!(
                        "0x{:x}",
                        spacing.value
                    ))
                    .unwrap();
                    tokens.extend(quote! {
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

pub fn generate_rpi(
    model: &ModelModules,
    addr_type: TokenStream,
) -> Result<String> {
    let tokens = generate_rpi_rec(model, addr_type)?;

    let file: syn::File = syn::parse2(tokens)?;
    let code = prettyplease::unparse(&file);
    Ok(code)
}

pub fn generate_rpi_rec(
    model: &ModelModules,
    addr_type: TokenStream,
) -> Result<TokenStream> {
    let mut cgv = CodegenVisitor {
        addr_type: addr_type.clone(),
        prelude: use_statements(),
        ..Default::default()
    };
    model.root.accept(&mut cgv);
    let mut tokens = cgv.tokens();

    for (name, module) in &model.used {
        let model_tokens = generate_rpi_rec(module, addr_type.clone())?;
        let modname = format_ident!("{}", name.to_case(Case::Snake));
        tokens.extend(quote! {
            pub mod #modname {
                #model_tokens
            }
        })
    }

    Ok(tokens)
}

pub fn use_statements() -> TokenStream {
    quote! {
        use bitset::BitSet;
        use anyhow::Result;
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::parser::parse;
    use expectorate::assert_contents;

    // Test code generation in terms of expected syntax.
    #[test]
    fn test_codegen() {
        let ast = match parse("examples/nic.rsf".into()) {
            Ok(ast) => ast,
            Err(ref e) => {
                panic!("parsing failed: {e}");
            }
        };

        let resolved =
            ModelModules::resolve(&ast, String::from("")).expect("resolve ast");

        let output =
            generate_rpi(&resolved, quote! { u32 }).expect("generate nic rpi");
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
                Ok(c.set_speed(ethernet::DataRate::G50))
            })
            .unwrap();

        // Read some config, status
        let config = rpi.phys(1).config().read(&platform).unwrap();
        assert_eq!(config.get_speed(), ethernet::DataRate::G50);

        let status = rpi.phys(2).status().read(&platform).unwrap();
        assert_eq!(status.get_carrier(), false);
    }
}
