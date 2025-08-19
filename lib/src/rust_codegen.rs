//! Rust code generation

use crate::ast::Number;
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
    addr_type: AddrType,
    value_type: ValueType,
    prelude: TokenStream,
    register_definitions: TokenStream,
    enum_definitions: TokenStream,
    block_definitions: TokenStream,
    block_methods: BTreeMap<String, TokenStream>,
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

    // Todo: the SRAM interface needs to be fleshed out as we get more
    // experience with both the consumers of the API and the underlying
    // mechanism.  For now, we simply provide enough functionality to
    // locate the data in SRAM and instantiate an instance in memory.
    fn sram(&mut self, reg: Arc<Register>) {
        let name = format_ident!("{}", reg.id.name.to_case(Case::Pascal));
        let width = proc_macro2::Literal::u128_unsuffixed(reg.width.value);

        let instance_name =
            format_ident!("{}Instance", reg.id.name.to_case(Case::Pascal));

        let doc = reg.doc.join("\n");
        let instance_doc = format!("Instance of a [`{}`]", reg.id.name);

        self.register_definitions.extend(quote! {

            #[derive(Debug, Default)]
            #[doc = #doc]
            pub struct #name([u8; #width]);

            #[doc = #instance_doc]
            pub struct #instance_name {
                pub msel_id: u32,
            }
        })
    }
}

impl Visitor for CodegenVisitor {
    fn register(&mut self, reg: Arc<Register>) {
        if reg.sram {
            return self.sram(reg);
        }
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
                                bool::from(self.0.get_field::<#width, #offset>())
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #setter(&mut self, data__: bool) {
                                self.0.set_field::<#width, #offset>(BitSet::<#width>::from(data__));
                            }
                        })
                    }
                }
                FieldType::Bitfield { width } => {
                    if width.value > 64 {
                        continue;
                    }
                    let width =
                        proc_macro2::Literal::u128_unsuffixed(width.value);
                    if f.mode == FieldMode::ReadOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #getter(&self) -> BitSet<#width> {
                                self.0.get_field::<#width, #offset>()
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #setter(&mut self, data__: BitSet<#width>) {
                                self.0.set_field::<#width, #offset>(data__);
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
                            .filter(|x| !x.is_empty())
                            .map(|x| {
                                format_ident!("{}", x.to_case(Case::Snake))
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
                            pub fn #getter(&self) -> Result<#typename, rust_rpi::OutOfRange> {
                                self.0.get_field::<#width, #offset>().try_into()
                            }
                        });
                    }
                    if f.mode == FieldMode::WriteOnly
                        || f.mode == FieldMode::ReadWrite
                    {
                        fields.extend(quote! {
                            #[doc = #doc]
                            pub fn #setter(&mut self, data__: #typename) {
                                self.0.set_field::<#width, #offset>(data__.into());
                            }
                        })
                    }
                }
            }
        }
        let addr_type: TokenStream = self.addr_type.into();
        let value_type: TokenStream = self.value_type.into();

        let instance_name =
            format_ident!("{}Instance", reg.id.name.to_case(Case::Pascal));

        // TODO: real fix is https://github.com/oxidecomputer/rsf/issues/10
        //       but for now prefer compile time panic to runtime panic.
        if let Some(reset_value) = &reg.reset_value
            && reset_value.value >= 1 << reg.width.value
        {
            panic!("reset value overflows register width");
        }

        let doc = reg.doc.join("\n");
        let instance_doc = format!("Instance of a [`{}`]", reg.id.name);
        let reset = match &reg.reset_value {
            None => quote! { self.0 = BitSet::<#width>::ZERO },
            Some(v) => {
                let x = proc_macro2::Literal::u128_unsuffixed(v.value);
                quote! { bitset_macro::bitset!(#width, #x); }
            }
        };

        let conversion = quote! { BitSet::<#width>::from(value) };

        // Condition some methods on width. This allows things like
        // descriptors to be defined that are not managed like regular
        // registers through platform traits, but it is nonetheless useful
        // to have codgen for the data structures. This is particularly valuable
        // for DMA data structures.
        //
        // TODO: maybe there should be a more explicit `dma` qualifier instead.
        // Similar to the `sram` qualifier.
        let to_from_value = if reg.width.value <= u128::from(self.value_type) {
            quote! {
                impl From<#value_type> for #name {
                    fn from(value: #value_type) -> Self {
                        //TODO should be fallible
                        Self(#conversion)
                    }
                }

                impl From<#name> for #value_type {
                    fn from(value: #name) -> Self {
                        //TODO it's not obvious without looking here that value_type
                        // should be an integer of some kind
                        #value_type::from(value.0)
                    }
                }
            }
        } else {
            quote! {}
        };

        let rpi_impl = if reg.width.value <= u128::from(self.value_type) {
            quote! {
                impl rust_rpi::RegisterInstance<#name, #addr_type, #value_type> for #instance_name {
                    fn cons(&self) -> #name {
                        let mut v = #name::default();
                        v.reset();
                        v
                    }

                    fn read<
                        P: rust_rpi::Platform<#addr_type, #value_type>,
                    >(
                        &self,
                        platform: &P,
                    ) -> Result<#name, P::Error> {
                        platform.read(self.addr)
                    }

                    fn write<
                        P: rust_rpi::Platform<#addr_type, #value_type>,
                    >(
                        &self,
                        platform: &P,
                        value: #name,
                    ) -> Result<(), P::Error> {
                        platform.write(self.addr, value)
                    }

                    fn try_update<
                        P: rust_rpi::Platform<#addr_type, #value_type>,
                        F: FnOnce(&mut #name) -> Result<(), P::Error>
                    >(
                        &self,
                        platform: &P,
                        f: F,
                    ) -> Result<(), P::Error> {
                        let mut value = self.read(platform)?;
                        f(&mut value)?;
                        self.write(platform, value)
                    }

                    fn update<
                        P: rust_rpi::Platform<#addr_type, #value_type>,
                        F: FnOnce(&mut #name)
                    >(
                        &self,
                        platform: &P,
                        f: F,
                    ) -> Result<(), P::Error> {
                        let mut value = self.read(platform)?;
                        f(&mut value);
                        self.write(platform, value)
                    }
                    fn try_set<
                        P: rust_rpi::Platform<#addr_type, #value_type>,
                        F: FnOnce(&mut #name) -> Result<(), P::Error>
                    >(
                        &self,
                        platform: &P,
                        f: F,
                    ) -> Result<(), P::Error> {
                        let mut value = #name::default();
                        value.reset();
                        f(&mut value)?;
                        self.write(platform, value)
                    }

                    fn set<
                        P: rust_rpi::Platform<#addr_type, #value_type>,
                        F: FnOnce(&mut #name)
                    >(
                        &self,
                        platform: &P,
                        f: F,
                    ) -> Result<(), P::Error> {
                        let mut value = #name::default();
                        value.reset();
                        f(&mut value);
                        self.write(platform, value)
                    }
                }
            }
        } else {
            quote! {}
        };

        self.register_definitions.extend(quote! {

            #[derive(Default, Debug)]
            #[doc = #doc]
            pub struct #name(BitSet<#width>);

            impl #name {
                #fields
                pub fn value(&self) -> BitSet<#width> {
                    self.0
                }
                pub fn reset(&mut self) {
                    #reset
                }
            }

            #to_from_value

            #[doc = #instance_doc]
            pub struct #instance_name {
                pub addr: #addr_type,
            }

            #rpi_impl
        })
    }

    fn enumeration(&mut self, e: Arc<crate::ast::Enum>) {
        let name = format_ident!("{}", e.id.name.to_case(Case::Pascal));
        let repr = match e.width.value {
            x if x <= 8 => quote! { u8 },
            x if x <= 16 => quote! { u16 },
            x if x <= 32 => quote! { u32 },
            x if x <= 64 => quote! { u64 },
            x if x <= 128 => quote! { u128 },
            _ => panic!("enums cannot be more than 128 bits wide"),
        };
        let doc = e.doc.join("\n");

        let mut alts = TokenStream::default();
        for a in &e.alternatives {
            let doc = a.doc.join("\n");
            let alt_name = format_ident!("{}", a.id.name.to_case(Case::Pascal));
            let alt_value = number_to_token(&a.value);
            alts.extend(quote! {
                #[doc = #doc]
                #alt_name = #alt_value,
            });
        }

        let width = proc_macro2::Literal::u128_unsuffixed(e.width.value);
        let mut alts_conv = Vec::default();
        for alt in &e.alternatives {
            let aname = format_ident!("{}", alt.id.name.to_case(Case::Pascal));
            let value = number_to_token(&alt.value);
            alts_conv.push(quote! {
                #name::#aname => bitset_macro::bitset!(#width, #value)
            });
        }
        let conversion = quote! {
            match value {
                #(#alts_conv,)*
            }
        };

        self.enum_definitions.extend(quote! {
            #[doc = #doc]
            #[derive(num_enum::TryFromPrimitive, PartialEq, Debug)]
            #[repr(#repr)]
            pub enum #name {
                #alts
            }

            impl From<#name> for BitSet<#width> {
                fn from(value: #name) -> BitSet<#width> {
                    #conversion
                }
            }

            impl TryFrom<BitSet<#width>> for #name {
                type Error = rust_rpi::OutOfRange;

                fn try_from(value: BitSet<#width>)
                    -> Result<Self, Self::Error> {
                    Self::try_from(#repr::from(value))
                        .map_err(|_|
                            rust_rpi::OutOfRange::EnumValueOutOfRange
                        )

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

        let current_block = name.to_owned();

        let addr_type: TokenStream = self.addr_type.into();
        let doc = block.doc.join("\n");

        self.block_definitions.extend(quote! {
            #[doc = #doc]
            #[derive(Default, Debug)]
            pub struct #block_name{
                pub addr: #addr_type
            }
        });

        let mut elements = block.elements.clone();
        elements.sort_by(|a, b| a.offset.value.cmp(&b.offset.value));
        for (idx, element) in elements.iter().enumerate() {
            let doc = element.doc.join("\n");
            let mut tokens = self
                .block_methods
                .get(&current_block)
                .cloned()
                .unwrap_or_default();

            // SRAM memory is accessed indirectly and is addressed using a
            // "memory selector ID".  The IDs are assigned in offset order, but
            // can't be calculated directly from the offset.  Thus, we ensure
            // the elements are sorted above, and build the msel_id from the
            // index here.
            let msel_id =
                proc_macro2::Literal::from_str(&format!("{idx}")).unwrap();
            let offset = match element.offset.value {
                0 => TokenStream::new(),
                x => {
                    let v = proc_macro2::Literal::from_str(&format!("0x{x:x}"))
                        .unwrap();
                    quote!( + #v )
                }
            };

            match &element.component {
                Component::Single { id, typ } => {
                    let method_name =
                        format_ident!("{}", id.name.to_case(Case::Snake));
                    let type_name =
                        typename_to_qualified_ident(typ, "Instance");
                    if block.sram {
                        tokens.extend(quote! {
                            #[doc = #doc]
                            pub fn #method_name(&self) -> #type_name {
                                #type_name {
                                    msel_id: #msel_id,
                                }
                            }
                        });
                    } else {
                        tokens.extend(quote! {
                            #[doc = #doc]
                            pub fn #method_name(&self) -> #type_name {
                                #type_name {
                                    addr: self.addr #offset,
                                }
                            }
                        });
                    }
                    self.block_methods.insert(current_block.clone(), tokens);
                }
                Component::Array {
                    id,
                    typ,
                    length,
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
                    let length =
                        proc_macro2::Literal::u128_unsuffixed(length.value);
                    if block.sram {
                        tokens.extend(quote! {
                        #[doc = #doc]
                            pub fn #method_name(&self) -> #type_name {
                                #type_name {
                                    msel_id: #msel_id,
                                }
                            }
                        })
                    } else {
                        tokens.extend(quote! {
                        #[doc = #doc]
                        pub fn #method_name(&self, index: #addr_type)
                            -> Result<#type_name, rust_rpi::OutOfRange> {
                            if index > #length {
                                return Err(rust_rpi::OutOfRange::IndexOutOfRange);
                            }
                            Ok(#type_name {
                                addr: self.addr #offset + (index * #spacing)
                            })
                        }
                    });
                    }
                    self.block_methods.insert(current_block.clone(), tokens);
                }
            }
        }
    }

    fn block_component(
        &mut self,
        _id: &crate::ast::Identifier,
        _path: &[crate::ast::Identifier],
        _block: Arc<Block>,
        _array_index: Option<u128>,
        _addr: u128,
    ) -> bool {
        true
    }
}

/// The address type to be used for register access in generated code.
#[derive(Copy, Clone, Default)]
pub enum AddrType {
    U8,
    U16,
    U32,
    #[default]
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
#[derive(Copy, Clone, Default)]
pub enum ValueType {
    U8,
    U16,
    U32,
    #[default]
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

impl From<ValueType> for u128 {
    fn from(value: ValueType) -> Self {
        match value {
            ValueType::U8 => 8,
            ValueType::U16 => 16,
            ValueType::U32 => 32,
            ValueType::U64 => 64,
            ValueType::U128 => 128,
        }
    }
}

pub fn codegen(
    file: &Utf8Path,
    addr_type: AddrType,
    value_type: ValueType,
) -> Result<String> {
    let ast = crate::parser::parse(file)?;
    let resolved = ModelModules::resolve(&ast, String::default())?;
    let modules = resolved.get_modules()?;
    generate_rpi(&modules, addr_type, value_type)
}

pub fn generate_rpi(
    modules: &BTreeMap<String, Arc<ModelModules>>,
    addr_type: AddrType,
    value_type: ValueType,
) -> Result<String> {
    let mut tokens = TokenStream::new();
    for (name, module) in modules {
        let model_tokens =
            generate_rpi_rec(name.to_string(), module, addr_type, value_type)?;
        tokens.extend(quote! {
            #model_tokens
        });
    }

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
    name: String,
    model: &ModelModules,
    addr_type: AddrType,
    value_type: ValueType,
) -> Result<TokenStream> {
    let mut cgv = CodegenVisitor {
        addr_type,
        value_type,
        prelude: use_statements(),
        ..Default::default()
    };

    model.root.accept(&mut cgv);
    let mut tokens = cgv.tokens();

    // If the name is not empty, then we are generating code for a module.  We
    // need to build the module definition and the "use" instructions for all
    // other modules being imported.
    if !name.is_empty() {
        let mut mod_tokens = TokenStream::new();
        let mut import_tokens = TokenStream::new();

        for name in model.used.keys() {
            let import_name = format_ident!("{}", name.to_case(Case::Snake));
            import_tokens.extend(quote! { use super::#import_name; });
        }
        let modname = format_ident!("{}", name.to_case(Case::Snake));
        mod_tokens.extend(quote! {
            pub mod #modname {
                #import_tokens
                #tokens
            }
        });
        tokens = mod_tokens;
    }

    Ok(tokens)
}

fn use_statements() -> TokenStream {
    let tokens = quote! {
        use bitset::BitSet;
        use rust_rpi;
    };
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

fn number_to_token(n: &Number) -> proc_macro2::Literal {
    match &n.format {
        NumberFormat::Binary { digits } => proc_macro2::Literal::from_str(
            &format!("0b{:0width$b}", n.value, width = digits,),
        ),
        NumberFormat::Hex { digits } => proc_macro2::Literal::from_str(
            &format!("0x{:0width$x}", n.value, width = digits),
        ),
        NumberFormat::Decimal { digits } => proc_macro2::Literal::from_str(
            &format!("{:0width$}", n.value, width = digits,),
        ),
    }
    .unwrap()
}
