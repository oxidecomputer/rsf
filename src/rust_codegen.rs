//! Rust code generation

use crate::model::Register;
use crate::model::{ModelModules, Visitor};
use proc_macro2::TokenStream;
use quote::{format_ident, quote};
use std::sync::Arc;

struct CodegenVisitor {
    tokens: TokenStream,
}

impl Visitor for CodegenVisitor {
    fn register(&mut self, reg: Arc<Register>) {
        let name = format_ident!("{}", reg.id.name);
        let width = proc_macro2::Literal::u128_unsuffixed(reg.width.value);
        let mut fields = TokenStream::default();
        for f in &reg.fields {
            let getter = format_ident!("get_{}", f.id.name);
            let setter = format_ident!("set_{}", f.id.name);
            let width = proc_macro2::Literal::u128_unsuffixed(f.typ.width());
            let offset = proc_macro2::Literal::u128_unsuffixed(f.offset.value);
            fields.extend(quote! {
                pub fn #getter(&self) -> BitSet<#width> {
                    self.0.get_field::<#width, #offset>().unwrap()
                }
                pub fn #setter(&mut self, data__: BitSet<#width>) {
                    self.0.set_field::<#width, #offset>(data__).unwrap();
                }
            })
        }
        self.tokens.extend(quote! {
            struct #name(Bitset<#width>) {
                #fields
            }
        })
    }
}

pub fn generate_rpi(model: &ModelModules) -> TokenStream {
    let mut cgv = CodegenVisitor { tokens: prelude() };
    model.root.accept(&mut cgv);
    cgv.tokens
}

pub fn prelude() -> TokenStream {
    let mut tokens = use_statements();
    tokens.extend(client());
    tokens
}

pub fn use_statements() -> TokenStream {
    quote! {
        use bitset::Bitset;
    }
}

pub fn client() -> TokenStream {
    quote! {
        pub struct Client {}
    }
}
