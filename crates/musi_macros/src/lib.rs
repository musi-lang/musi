//! Proc-macro for declaring Musi native modules.
//!
//! # Usage
//!
//! ```ignore
//! #[musi_module]          // specifier = "musi:io"  (derived from mod name)
//! #[musi_module("musi:assert")]  // explicit specifier override
//! pub mod io {
//!     pub fn writeln(s: &str) -> () { println!("{s}"); }
//!     pub fn read_line() -> String  { ... }
//!
//!     // Override the generated Musi signature (everything after `fn name`):
//!     #[musi_src("['T](arr: []'T): Int")]
//!     pub fn array_length(arr: Value) -> i64 { ... }
//! }
//! ```
//!
//! Each `pub fn` in the module body gains:
//! - A private `_native_<fn>` wrapper that marshals `&[Value]` ↔ Rust types.
//! - An entry in `pub const FUNCTIONS: &[(&str, NativeFn)]`.
//! - A line in `pub const MUSI_SOURCE: &str` with the `#[intrinsic(...)]` Musi declaration.
//!
//! Supported Rust parameter types → Musi types:
//! - `&str`    → `String`
//! - `i64`     → `Int`
//! - `f64`     → `Float`
//! - `bool`    → `Bool`
//! - `Value`   → pass-through (requires `#[musi_src]` for non-trivial Musi signatures)
//!
//! Supported Rust return types → Musi types:
//! - `()`           → `Unit`
//! - `i64`          → `Int`
//! - `f64`          → `Float`
//! - `bool`         → `Bool`
//! - `String`       → `String`
//! - `Value`        → pass-through
//! - `Option<i64>`  → `Option[Int]`
//! - `Option<Value>`→ `Option['T]`

extern crate proc_macro;

use proc_macro::TokenStream;
use proc_macro2::TokenStream as TokenStream2;
use quote::{format_ident, quote};
use syn::{
    Attribute, FnArg, Ident, ItemMod, LitStr, Pat, ReturnType, Type,
    parse_macro_input,
    spanned::Spanned,
};

// -- type classification -------------------------------------------------------

enum ParamKind {
    Str,   // &str  → String
    I64,   // i64   → Int
    F64,   // f64   → Float
    Bool,  // bool  → Bool
    Value, // Value → pass-through (requires #[musi_src])
}

enum ReturnKind {
    Unit,
    I64,
    F64,
    Bool,
    OwnedStr,    // String
    Value,       // Value pass-through
    OptionI64,   // Option<i64>
    OptionValue, // Option<Value>
    OptionStr,   // Option<String>
}

impl ParamKind {
    fn musi_type(&self) -> &'static str {
        match self {
            Self::Str   => "String",
            Self::I64   => "Int",
            Self::F64   => "Float",
            Self::Bool  => "Bool",
            Self::Value => "'T",
        }
    }
}

impl ReturnKind {
    fn musi_type(&self) -> &'static str {
        match self {
            Self::Unit        => "Unit",
            Self::I64         => "Int",
            Self::F64         => "Float",
            Self::Bool        => "Bool",
            Self::OwnedStr    => "String",
            Self::Value       => "'T",
            Self::OptionI64   => "Option[Int]",
            Self::OptionValue => "Option['T]",
            Self::OptionStr   => "Option[String]",
        }
    }
}

fn classify_param(ty: &Type) -> Option<ParamKind> {
    match ty {
        Type::Reference(r) => {
            if let Type::Path(p) = r.elem.as_ref()
                && p.path.is_ident("str")
            {
                return Some(ParamKind::Str);
            }
            None
        }
        Type::Path(p) => match p.path.get_ident()?.to_string().as_str() {
            "i64"   => Some(ParamKind::I64),
            "f64"   => Some(ParamKind::F64),
            "bool"  => Some(ParamKind::Bool),
            "Value" => Some(ParamKind::Value),
            _       => None,
        },
        _ => None,
    }
}

fn classify_return(ty: &Type) -> Option<ReturnKind> {
    match ty {
        Type::Tuple(t) if t.elems.is_empty() => Some(ReturnKind::Unit),
        Type::Path(p) => {
            if let Some(ident) = p.path.get_ident() {
                return match ident.to_string().as_str() {
                    "i64"    => Some(ReturnKind::I64),
                    "f64"    => Some(ReturnKind::F64),
                    "bool"   => Some(ReturnKind::Bool),
                    "String" => Some(ReturnKind::OwnedStr),
                    "Value"  => Some(ReturnKind::Value),
                    _        => None,
                };
            }
            // Option<T>
            let seg = p.path.segments.last()?;
            if seg.ident != "Option" { return None; }
            if let syn::PathArguments::AngleBracketed(ab) = &seg.arguments
                && let Some(syn::GenericArgument::Type(Type::Path(ip))) = ab.args.first()
                && let Some(id) = ip.path.get_ident()
            {
                return match id.to_string().as_str() {
                    "i64"    => Some(ReturnKind::OptionI64),
                    "Value"  => Some(ReturnKind::OptionValue),
                    "String" => Some(ReturnKind::OptionStr),
                    _        => None,
                };
            }
            None
        }
        _ => None,
    }
}

// -- code generation ----------------------------------------------------------

/// Generates the extraction snippet for one parameter.
/// Returns (param_ident, tokens).
fn gen_extraction(idx: usize, name: &Ident, kind: &ParamKind) -> TokenStream2 {
    let holder = format_ident!("owned_str_{idx}");
    match kind {
        ParamKind::Str => quote! {
            let #holder = match args.get(#idx) {
                Some(Value::String(s)) => Rc::clone(s),
                _ => return Value::Unit,
            };
            let #name: &str = &*#holder;
        },
        ParamKind::I64 => quote! {
            let #name: i64 = match args.get(#idx) {
                Some(Value::Int(n)) => *n,
                _ => return Value::Unit,
            };
        },
        ParamKind::F64 => quote! {
            let #name: f64 = match args.get(#idx) {
                Some(Value::Float(n)) => *n,
                _ => return Value::Unit,
            };
        },
        ParamKind::Bool => quote! {
            let #name: bool = match args.get(#idx) {
                Some(Value::Object { fields, .. }) => {
                    matches!(fields.first(), Some(Value::Int(1)))
                }
                _ => return Value::Unit,
            };
        },
        ParamKind::Value => quote! {
            let #name: Value = match args.get(#idx) {
                Some(v) => v.clone(),
                None    => return Value::Unit,
            };
        },
    }
}

fn gen_return_wrap(kind: &ReturnKind, expr: TokenStream2) -> TokenStream2 {
    match kind {
        ReturnKind::Unit => quote! {
            #expr;
            Value::Unit
        },
        ReturnKind::I64      => quote! { Value::Int(#expr)                          },
        ReturnKind::F64      => quote! { Value::Float(#expr)                        },
        ReturnKind::Bool     => quote! { bool_val(#expr)                            },
        ReturnKind::OwnedStr => quote! { Value::String(Rc::from(#expr.as_str()))    },
        ReturnKind::Value    => quote! { #expr                                      },
        ReturnKind::OptionI64 => quote! {
            match #expr { Some(n) => option_some(Value::Int(n)), None => option_none() }
        },
        ReturnKind::OptionValue => quote! {
            match #expr { Some(v) => option_some(v), None => option_none() }
        },
        ReturnKind::OptionStr => quote! {
            match #expr {
                Some(s) => option_some(Value::String(Rc::from(s.as_str()))),
                None    => option_none(),
            }
        },
    }
}

// -- attribute helpers --------------------------------------------------------

/// Removes and returns the value of a `#[musi_src("...")]` attribute, if present.
fn take_musi_src(attrs: &mut Vec<Attribute>) -> Option<String> {
    let pos = attrs.iter().position(|a| a.path().is_ident("musi_src"))?;
    let attr = attrs.remove(pos);
    let s: LitStr = attr.parse_args().ok()?;
    Some(s.value())
}

// -- main proc-macro ----------------------------------------------------------

struct FnSpec {
    name:     Ident,
    params:   Vec<(Ident, ParamKind)>,
    ret:      ReturnKind,
    musi_src: Option<String>,
}

#[proc_macro_attribute]
pub fn musi_module(attr: TokenStream, item: TokenStream) -> TokenStream {
    let specifier_override: Option<String> = if attr.is_empty() {
        None
    } else {
        let lit = parse_macro_input!(attr as LitStr);
        Some(lit.value())
    };

    let mut module = parse_macro_input!(item as ItemMod);

    match generate(&mut module, specifier_override) {
        Ok(ts) => ts.into(),
        Err(e) => e.into_compile_error().into(),
    }
}

fn generate(module: &mut ItemMod, specifier_override: Option<String>) -> Result<TokenStream2, syn::Error> {
    let mod_ident = &module.ident;
    let mod_name  = mod_ident.to_string();
    let specifier = specifier_override.unwrap_or_else(|| format!("musi:{mod_name}"));
    let vis       = &module.vis;
    let mod_attrs = &module.attrs;

    let Some((_, items)) = module.content.as_mut() else {
        return Err(syn::Error::new(
            module.ident.span(),
            "#[musi_module] requires an inline module body",
        ));
    };

    // -- collect specs from pub fns --
    let mut specs: Vec<FnSpec> = Vec::new();
    for item in items.iter_mut() {
        let syn::Item::Fn(f) = item else { continue };
        if !matches!(f.vis, syn::Visibility::Public(_)) { continue }

        let musi_src = take_musi_src(&mut f.attrs);
        let name = f.sig.ident.clone();

        let mut params: Vec<(Ident, ParamKind)> = Vec::new();
        for arg in &f.sig.inputs {
            let FnArg::Typed(pt) = arg else { continue };
            let Pat::Ident(pi) = pt.pat.as_ref() else { continue };
            let kind = classify_param(&pt.ty).ok_or_else(|| {
                syn::Error::new(pt.ty.span(), "musi_module: unsupported parameter type")
            })?;
            params.push((pi.ident.clone(), kind));
        }

        let ret = match &f.sig.output {
            ReturnType::Default    => ReturnKind::Unit,
            ReturnType::Type(_, ty) => classify_return(ty).ok_or_else(|| {
                syn::Error::new(ty.span(), "musi_module: unsupported return type")
            })?,
        };

        specs.push(FnSpec { name, params, ret, musi_src });
    }

    // -- generate per-fn artifacts --
    let mut wrappers:     Vec<TokenStream2> = Vec::new();
    let mut source_lines: Vec<String>       = Vec::new();
    let mut fn_entries:   Vec<TokenStream2> = Vec::new();

    for spec in &specs {
        let fn_name     = &spec.name;
        let fn_name_str = fn_name.to_string();
        let wrapper_name = format_ident!("_native_{fn_name_str}");

        // extraction + call
        let mut extractions: Vec<TokenStream2> = Vec::new();
        let mut arg_names:   Vec<Ident>        = Vec::new();
        for (idx, (pname, pkind)) in spec.params.iter().enumerate() {
            extractions.push(gen_extraction(idx, pname, pkind));
            arg_names.push(pname.clone());
        }
        let call     = quote! { #fn_name(#(#arg_names),*) };
        let ret_expr = gen_return_wrap(&spec.ret, call);

        wrappers.push(quote! {
            fn #wrapper_name(args: &[Value]) -> Value {
                #(#extractions)*
                #ret_expr
            }
        });

        // musi source line
        let src = if let Some(sig) = &spec.musi_src {
            format!("#[intrinsic(\"{fn_name_str}\")] export extrin fn {fn_name_str}{sig};\n")
        } else {
            let params_str = spec.params.iter()
                .map(|(pn, pk)| format!("{pn}: {}", pk.musi_type()))
                .collect::<Vec<_>>()
                .join(", ");
            let ret_type = spec.ret.musi_type();
            format!("#[intrinsic(\"{fn_name_str}\")] export extrin fn {fn_name_str}({params_str}): {ret_type};\n")
        };
        source_lines.push(src);

        fn_entries.push(quote! { (#fn_name_str, #wrapper_name) });
    }

    let full_source: &str = &source_lines.concat();

    Ok(quote! {
        #(#mod_attrs)*
        #vis mod #mod_ident {
            #![allow(
                clippy::pedantic,
                clippy::nursery,
                unused_imports,
            )]
            use crate::registry::{NativeFn, Value, bool_val, option_none, option_some};
            use std::rc::Rc;

            #(#items)*

            #(#wrappers)*

            pub const SPECIFIER:   &str = #specifier;
            pub const MUSI_SOURCE: &str = #full_source;
            pub const FUNCTIONS: &[(&str, NativeFn)] = &[#(#fn_entries),*];
        }
    })
}
