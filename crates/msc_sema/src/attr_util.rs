//! Attribute validation and compile-time configuration helpers.

use msc_ast::attr::{Attr, AttrValue};
use msc_ast::lit::Lit;
use msc_shared::{Interner, Span};

use crate::error::SemaError;

/// Names that the compiler recognises as valid attributes.
const KNOWN_ATTRS: &[&str] = &[
    "lang",
    "link",
    "test",
    "inline",
    "abstract",
    "deprecated",
    "cfg",
    "variadic",
    "repr",
];

/// A lightweight warning produced during attribute validation.
pub struct AttrWarning {
    pub message: SemaError,
    pub span: Span,
}

/// Evaluates `#[cfg]` attributes and returns whether the item should be
/// included in the current build.
///
/// - No `#[cfg]` attrs present → `true` (always include).
/// - One or more `#[cfg]` attrs present → `true` iff at least one matches
///   the current platform/target triple.
///
/// Supported forms:
/// - `#[cfg := "macos"]` - string shorthand (os or family)
/// - `#[cfg("macos")]` - positional shorthand (os or family)
/// - `#[cfg(os := "macos")]` - named fields; all fields must match
/// - `#[cfg(arch := "x86_64")]`
/// - `#[cfg(family := "unix")]`
#[must_use]
pub fn check_cfg(attrs: &[Attr], interner: &Interner) -> bool {
    let cfg_attrs: Vec<_> = attrs
        .iter()
        .filter(|a| interner.resolve(a.name) == "cfg")
        .collect();

    if cfg_attrs.is_empty() {
        return true;
    }

    for attr in &cfg_attrs {
        match &attr.value {
            // #[cfg := "macos"] - legacy string shorthand
            Some(AttrValue::Lit {
                lit: Lit::Str { value, .. },
                ..
            }) => {
                let target = strip_quotes(interner.resolve(*value));
                if cfg_matches("os", target) || cfg_matches("family", target) {
                    return true;
                }
            }
            // #[cfg("macos")] - positional tuple shorthand
            Some(AttrValue::Tuple { lits, .. }) => {
                if let Some(Lit::Str { value, .. }) = lits.first() {
                    let target = strip_quotes(interner.resolve(*value));
                    if cfg_matches("os", target) || cfg_matches("family", target) {
                        return true;
                    }
                }
            }
            // #[cfg(target_os := "macos")] - named fields; all fields must match
            Some(AttrValue::Named { fields, .. }) => {
                if !fields.is_empty()
                    && fields.iter().all(|f| {
                        let key = interner.resolve(f.name);
                        if let Lit::Str { value, .. } = &f.value {
                            let val = strip_quotes(interner.resolve(*value));
                            cfg_matches(key, val)
                        } else {
                            false
                        }
                    })
                {
                    return true;
                }
            }
            // #[cfg] bare or unrecognised value form - skip
            None | Some(_) => {}
        }
    }

    false
}

fn strip_quotes(s: &str) -> &str {
    s.strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(s)
}

fn cfg_matches(key: &str, value: &str) -> bool {
    match key {
        "os" => match value {
            "macos" => cfg!(target_os = "macos"),
            "linux" => cfg!(target_os = "linux"),
            "windows" => cfg!(target_os = "windows"),
            "ios" => cfg!(target_os = "ios"),
            "android" => cfg!(target_os = "android"),
            "freebsd" => cfg!(target_os = "freebsd"),
            _ => false,
        },
        "arch" => match value {
            "x86_64" => cfg!(target_arch = "x86_64"),
            "aarch64" => cfg!(target_arch = "aarch64"),
            "wasm32" => cfg!(target_arch = "wasm32"),
            "arm" => cfg!(target_arch = "arm"),
            "x86" => cfg!(target_arch = "x86"),
            "riscv64" => cfg!(target_arch = "riscv64"),
            _ => false,
        },
        "family" => match value {
            "unix" => cfg!(unix),
            "windows" => cfg!(windows),
            "wasm" => cfg!(target_arch = "wasm32"),
            _ => false,
        },
        _ => false,
    }
}

/// Validates that every attribute name in `attrs` is known to the compiler.
///
/// Returns one [`AttrWarning`] per unrecognised attribute.
#[must_use]
pub fn validate_attrs(attrs: &[Attr], interner: &Interner) -> Vec<AttrWarning> {
    attrs
        .iter()
        .filter_map(|attr| {
            let name = interner.resolve(attr.name);
            if KNOWN_ATTRS.contains(&name) {
                None
            } else {
                Some(AttrWarning {
                    message: SemaError::UnknownAttribute {
                        name: Box::from(name),
                    },
                    span: attr.span,
                })
            }
        })
        .collect()
}
