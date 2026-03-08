//! Rust code generation

use crate::ast::{Identifier, Number};
use crate::common::{Alternative, Attribute, FieldMode, NumberFormat, Typename};
use crate::model::{Block, Component, Field, FieldType, FieldUserType, Register};
use crate::model::{ModelModules, QualifiedFieldType, Visitor};
use anyhow::{Result, anyhow};
use camino::Utf8Path;
use camino_tempfile::NamedUtf8TempFile;
use convert_case::{Case, Casing};
use proc_macro2::{Punct, Spacing, TokenStream};
use quote::{ToTokens, TokenStreamExt, format_ident, quote};
use std::collections::BTreeMap;
use std::io::Write;
use std::str::FromStr;
use std::sync::Arc;

#[derive(Default)]
struct CodegenVisitor {
    addr_type: AddrType,
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
        let attrs = attrs_accessor(&format_ident!("attrs"), &reg.attrs);
        let instance_doc = format!("Instance of a [`{}`]", reg.id.name);

        self.register_definitions.extend(quote! {

            #[derive(Debug, Default)]
            #[doc = #doc]
            pub struct #name([u8; #width]);

            impl #name {
                #attrs
            }

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
        let mut display_impl = TokenStream::default();
        let mut fields = TokenStream::default();
        for f in &reg.fields {
            let doc = f.doc.join("\n");

            let fname_str = &f.id.name;
            let getter = format_ident!("get_{fname_str}");
            let setter = format_ident!("set_{fname_str}");
            let attrs_getter = format_ident!("{fname_str}_attrs");

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
                        display_impl.extend(quote! {
                            writeln!(f, "{}: {}", #fname_str, self.#getter())?;
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
                        display_impl.extend(quote! {
                            writeln!(
                                f,
                                "{}: {}/0x{:x}/0b{:b}",
                                #fname_str,
                                self.#getter().to_int(),
                                self.#getter().to_int(),
                                self.#getter().to_int(),
                            )?;
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
                        display_impl.extend(quote! {
                            writeln!(f, "{}: {:?}", #fname_str, self.#getter())?;
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
                        });
                    }
                }
            }
            fields.extend(attrs_accessor(&attrs_getter, &f.attrs));
        }
        let addr_type: TokenStream = self.addr_type.into();
        let value_type = width_to_value_type(reg.width.value);

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
        let (to_from_value, rpi_impl) = if let Some(value_type) = &value_type {
            let to_from = quote! {
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
            };
            let rpi = quote! {
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
            };
            (to_from, rpi)
        } else {
            (quote! {}, quote! {})
        };

        let format_param = if display_impl.is_empty() {
            format_ident!("_f")
        } else {
            format_ident!("f")
        };

        let attrs = attrs_accessor(&format_ident!("attrs"), &reg.attrs);
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
                #attrs
            }

            #to_from_value

            #[doc = #instance_doc]
            pub struct #instance_name {
                pub addr: #addr_type,
            }

            #rpi_impl

            impl core::fmt::Display for #name {
                fn fmt(&self, #format_param: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
                    #display_impl
                    Ok(())
                }
            }
        })
    }

    fn enumeration(&mut self, e: Arc<crate::ast::Enum>) {
        let name = format_ident!("{}", e.id.name.to_case(Case::Pascal));
        let repr = width_to_value_type(e.width.value)
            .expect("enums cannot be more than 128 bits wide");
        let doc = e.doc.join("\n");
        let attrs = attrs_accessor(&format_ident!("attrs"), &e.attrs);

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
            impl #name {
                #attrs
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
            pub struct #block_name {
                pub addr: #addr_type
            }
        });

        let mut elements = block.elements.clone();
        elements.sort_by_key(|a| a.offset.value);
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

            let (id_name, typ) = match &element.component {
                Component::Single { id, typ, .. } => (&id.name, typ),
                Component::Array { id, typ, .. } => (&id.name, typ),
            };
            let name = id_name.to_case(Case::Snake);
            let method_name = format_ident!("{name}");
            let attrs_name = format_ident!("{name}_attrs");
            let type_name = typename_to_qualified_ident(typ, "Instance");
            match &element.component {
                Component::Single { .. } => {
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
                }
                Component::Array {
                    length, spacing, ..
                } => {
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
                }
            }
            tokens.extend(attrs_accessor(&attrs_name, &element.attrs));
            self.block_methods.insert(current_block.clone(), tokens);
        }
        let mut tokens = self
            .block_methods
            .get(&current_block)
            .cloned()
            .unwrap_or_default();
        tokens.extend(attrs_accessor(&format_ident!("attrs"), &block.attrs));
        self.block_methods.insert(current_block.clone(), tokens);
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

fn width_to_value_type(width: u128) -> Option<TokenStream> {
    match width {
        x if x <= 8 => Some(quote! { u8 }),
        x if x <= 16 => Some(quote! { u16 }),
        x if x <= 32 => Some(quote! { u32 }),
        x if x <= 64 => Some(quote! { u64 }),
        x if x <= 128 => Some(quote! { u128 }),
        _ => None,
    }
}

pub fn codegen(file: &Utf8Path, addr_type: AddrType) -> Result<String> {
    let ast = crate::parser::parse(file)?;
    let resolved = ModelModules::resolve(&ast, String::default())?;
    let mut tokens = generate_module_tokens(&resolved, addr_type)?;
    tokens.extend(generate_regdb_tokens(&resolved, addr_type));

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

/// Generate tokens for a module and all of its sub-modules by walking the
/// `ModelModules` tree directly. Each used module becomes a nested `pub mod`
/// block, which correctly handles the case where different subtrees contain
/// modules with the same name.
fn generate_module_tokens(
    model: &ModelModules,
    addr_type: AddrType,
) -> Result<TokenStream> {
    let mut cgv = CodegenVisitor {
        addr_type,
        prelude: use_statements(),
        ..Default::default()
    };

    model.root.accept(&mut cgv);
    let mut tokens = cgv.tokens();

    // Recursively generate nested modules for each dependency.
    for (name, sub) in &model.used {
        let sub_tokens = generate_module_tokens(sub, addr_type)?;
        let modname = format_ident!("{}", name.to_case(Case::Snake));
        tokens.extend(quote! {
            pub mod #modname {
                #sub_tokens
            }
        });
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

    // The "Main" block is special-cased to generate a struct named "Client"
    // rather than "MainInstance".
    let typ = if *typename == "Main" {
        format_ident!("Client")
    } else {
        format_ident!("{}{}", typename.to_case(Case::Pascal), suffix)
    };

    // We only care about the last module in the path. Types reference their
    // direct parent module (a child `pub mod` in the generated code).
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

impl ToTokens for Attribute {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        let name = &self.id.name;
        let value = &self.value;
        tokens.extend(quote! {(#name, #value)});
    }
}

struct AttrSlice<'a>(&'a [Attribute]);

impl ToTokens for AttrSlice<'_> {
    fn to_tokens(&self, tokens: &mut TokenStream) {
        for (k, attr) in self.0.iter().enumerate() {
            if k > 0 {
                tokens.append(Punct::new(',', Spacing::Alone));
            }
            attr.to_tokens(tokens);
        }
    }
}

fn attrs_accessor(
    name: &proc_macro2::Ident,
    attrs: &[Attribute],
) -> TokenStream {
    let attrs = AttrSlice(attrs);
    quote! {
        pub fn #name(&self) -> &'static [(&'static str, &'static str)] {
            &[#attrs]
        }
    }
}

// --- Register DB code generation ---

struct RegDbEntry {
    name: String,
    address: u128,
    register: Arc<Register>,
}

struct RegDbVisitor {
    entries: Vec<RegDbEntry>,
}

impl Visitor for RegDbVisitor {
    fn register_component(
        &mut self,
        id: &Identifier,
        path: &[Identifier],
        reg: Arc<Register>,
        _array_index: Option<u128>,
        addr: u128,
    ) {
        let name = if path.is_empty() {
            id.name.clone()
        } else {
            format!(
                "{}.{}",
                path.iter()
                    .map(|x| x.name.as_str())
                    .collect::<Vec<_>>()
                    .join("."),
                id.name,
            )
        };
        self.entries.push(RegDbEntry {
            name,
            address: addr,
            register: reg,
        });
    }
}

fn generate_regdb_tokens(
    model: &ModelModules,
    addr_type: AddrType,
) -> TokenStream {
    let mut visitor = RegDbVisitor { entries: vec![] };
    model.root.accept(&mut visitor);

    let addr_type_ts: TokenStream = addr_type.into();

    let inserts: Vec<TokenStream> = visitor
        .entries
        .iter()
        .map(|entry| {
            let name = &entry.name;
            let addr = proc_macro2::Literal::u128_unsuffixed(entry.address);
            let reg_tokens = register_construct_tokens(&entry.register);
            quote! {
                let _ = db.insert_unique(rsf::db::Entry {
                    name: String::from(#name),
                    address: #addr as #addr_type_ts,
                    register: #reg_tokens,
                });
            }
        })
        .collect();

    quote! {
        pub fn regdb() -> rsf::db::RegisterDb<#addr_type_ts> {
            let mut db = rsf::db::RegisterDb::new();
            #(#inserts)*
            db
        }
    }
}

fn register_construct_tokens(reg: &Register) -> TokenStream {
    let doc = string_vec_tokens(&reg.doc);
    let id = ident_construct_tokens(&reg.id);
    let width = number_construct_tokens(&reg.width);
    let reset_value = match &reg.reset_value {
        None => quote! { None },
        Some(n) => {
            let n = number_construct_tokens(n);
            quote! { Some(#n) }
        }
    };
    let sram = reg.sram;
    let fields: Vec<TokenStream> =
        reg.fields.iter().map(field_construct_tokens).collect();
    let attrs: Vec<TokenStream> =
        reg.attrs.iter().map(attr_construct_tokens).collect();

    quote! {
        rsf::model::Register {
            doc: #doc,
            id: #id,
            width: #width,
            reset_value: #reset_value,
            sram: #sram,
            fields: vec![#(#fields),*],
            attrs: vec![#(#attrs),*],
        }
    }
}

fn field_construct_tokens(f: &Field) -> TokenStream {
    let doc = string_vec_tokens(&f.doc);
    let id = ident_construct_tokens(&f.id);
    let mode = field_mode_construct_tokens(&f.mode);
    let typ = field_type_construct_tokens(&f.typ);
    let offset = number_construct_tokens(&f.offset);
    let attrs: Vec<TokenStream> =
        f.attrs.iter().map(attr_construct_tokens).collect();

    quote! {
        rsf::model::Field {
            doc: #doc,
            id: #id,
            mode: #mode,
            typ: #typ,
            offset: #offset,
            attrs: vec![#(#attrs),*],
        }
    }
}

fn field_type_construct_tokens(typ: &FieldType) -> TokenStream {
    match typ {
        FieldType::Bool => quote! { rsf::model::FieldType::Bool },
        FieldType::Bitfield { width } => {
            let w = number_construct_tokens(width);
            quote! { rsf::model::FieldType::Bitfield { width: #w } }
        }
        FieldType::User { id } => {
            let qft = qualified_field_type_construct_tokens(id);
            quote! { rsf::model::FieldType::User { id: #qft } }
        }
    }
}

fn qualified_field_type_construct_tokens(
    qft: &QualifiedFieldType,
) -> TokenStream {
    let module_path: Vec<TokenStream> = qft
        .module_path
        .iter()
        .map(|s| quote! { String::from(#s) })
        .collect();
    let typ = field_user_type_construct_tokens(&qft.typ);
    quote! {
        rsf::model::QualifiedFieldType {
            module_path: vec![#(#module_path),*],
            typ: #typ,
        }
    }
}

fn field_user_type_construct_tokens(fut: &FieldUserType) -> TokenStream {
    match fut {
        FieldUserType::Enum(e) => {
            let e = enum_construct_tokens(e);
            quote! {
                rsf::model::FieldUserType::Enum(std::sync::Arc::new(#e))
            }
        }
    }
}

fn enum_construct_tokens(e: &crate::common::Enum) -> TokenStream {
    let doc = string_vec_tokens(&e.doc);
    let id = ident_construct_tokens(&e.id);
    let width = number_construct_tokens(&e.width);
    let alts: Vec<TokenStream> =
        e.alternatives.iter().map(alt_construct_tokens).collect();
    let attrs: Vec<TokenStream> =
        e.attrs.iter().map(attr_construct_tokens).collect();

    quote! {
        rsf::common::Enum {
            doc: #doc,
            id: #id,
            width: #width,
            alternatives: vec![#(#alts),*],
            attrs: vec![#(#attrs),*],
        }
    }
}

fn alt_construct_tokens(a: &Alternative) -> TokenStream {
    let doc = string_vec_tokens(&a.doc);
    let id = ident_construct_tokens(&a.id);
    let value = number_construct_tokens(&a.value);

    quote! {
        rsf::common::Alternative {
            doc: #doc,
            id: #id,
            value: #value,
        }
    }
}

fn ident_construct_tokens(id: &Identifier) -> TokenStream {
    let name = &id.name;
    quote! { rsf::common::Identifier::new(#name) }
}

fn number_construct_tokens(n: &Number) -> TokenStream {
    let value = proc_macro2::Literal::u128_unsuffixed(n.value);
    let format = match &n.format {
        NumberFormat::Binary { digits } => {
            let d = *digits;
            quote! { rsf::common::NumberFormat::Binary { digits: #d } }
        }
        NumberFormat::Hex { digits } => {
            let d = *digits;
            quote! { rsf::common::NumberFormat::Hex { digits: #d } }
        }
        NumberFormat::Decimal { digits } => {
            let d = *digits;
            quote! { rsf::common::NumberFormat::Decimal { digits: #d } }
        }
    };
    quote! { rsf::common::Number::new(#value, #format) }
}

fn field_mode_construct_tokens(mode: &FieldMode) -> TokenStream {
    match mode {
        FieldMode::ReadOnly => {
            quote! { rsf::common::FieldMode::ReadOnly }
        }
        FieldMode::WriteOnly => {
            quote! { rsf::common::FieldMode::WriteOnly }
        }
        FieldMode::ReadWrite => {
            quote! { rsf::common::FieldMode::ReadWrite }
        }
        FieldMode::Reserved => {
            quote! { rsf::common::FieldMode::Reserved }
        }
    }
}

fn attr_construct_tokens(attr: &Attribute) -> TokenStream {
    let id = ident_construct_tokens(&attr.id);
    let value = &attr.value;
    quote! {
        rsf::common::Attribute {
            id: #id,
            value: String::from(#value),
        }
    }
}

fn string_vec_tokens(v: &[String]) -> TokenStream {
    let items: Vec<TokenStream> =
        v.iter().map(|s| quote! { String::from(#s) }).collect();
    quote! { vec![#(#items),*] }
}
