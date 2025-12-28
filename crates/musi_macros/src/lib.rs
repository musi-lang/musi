use proc_macro::TokenStream;
use quote::quote;
use syn::{Attribute, DeriveInput, Expr, ExprLit, Lit, Meta, parse_macro_input};

#[proc_macro_derive(MusiError, attributes(ms))]
pub fn derive_musi_error(input: TokenStream) -> TokenStream {
    let input = parse_macro_input!(input as DeriveInput);
    let enum_name = &input.ident;

    let variants = match &input.data {
        syn::Data::Enum(data_enum) => &data_enum.variants,
        _ => {
            return syn::Error::new_spanned(input, "MusiError can only be derived for enums")
                .to_compile_error()
                .into();
        }
    };

    let match_arms = variants.iter().filter_map(|variant| {
        let variant_name = &variant.ident;
        let code = extract_ms_code(&variant.attrs)?;
        let error_code_variant = syn::Ident::new(&format!("MS{code}"), variant.ident.span());

        let pattern = match &variant.fields {
            syn::Fields::Unit => quote! { Self::#variant_name },
            syn::Fields::Unnamed(_) => quote! { Self::#variant_name(..) },
            syn::Fields::Named(_) => quote! { Self::#variant_name { .. } },
        };

        Some(quote! {
            #pattern => Some(musi_basic::error::ErrorCode::#error_code_variant),
        })
    });

    let expanded = quote! {
        impl musi_basic::error::IntoMusiError for #enum_name {
            fn code(&self) -> Option<musi_basic::error::ErrorCode> {
                match self {
                    #(#match_arms)*
                    _ => None,
                }
            }
        }
    };

    expanded.into()
}

fn extract_ms_code(attrs: &[Attribute]) -> Option<u16> {
    for attr in attrs {
        if !attr.path().is_ident("ms") {
            continue;
        }

        let Meta::List(meta_list) = &attr.meta else {
            continue;
        };

        let expr: Expr = syn::parse2(meta_list.tokens.clone()).ok()?;
        if let Expr::Lit(ExprLit {
            lit: Lit::Int(lit_int),
            ..
        }) = expr
        {
            return lit_int.base10_parse().ok();
        }
    }
    None
}
