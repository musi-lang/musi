//! Rust-backed native modules for Musi.
//!
//! Each submodule is declared with `#[musi_module]` and produces:
//! - `SPECIFIER`   — the `"musi:..."` import string
//! - `MUSI_SOURCE` — the Musi source the compiler parses
//! - `FUNCTIONS`   — `&[(&str, NativeFn)]` for VM dispatch (future wiring)
//!
//! The flat [`REGISTRY`] slice is consumed by the CLI resolver and eventually
//! by the VM to replace the hard-coded `native::dispatch` match.

#![allow(clippy::module_name_repetitions)]

pub mod registry;

mod array;
mod io;
mod math;
mod num;
mod string;

use registry::NativeModuleEntry;

// -- assert builtins (VM intercepts these; bodies are no-ops) -----------------

const ASSERT_SOURCE: &str = concat!(
    "#[intrinsic(\"assert\")] export extrin fn assert(cond: Bool): Unit;\n",
    "#[intrinsic(\"assert_msg\")] export extrin fn assert_msg(cond: Bool, msg: String): Unit;\n",
    "#[intrinsic(\"test\")] export extrin fn test(name: String, f: () -> Unit): Unit;\n",
);

fn _native_assert(_args: &[registry::Value]) -> registry::Value     { registry::Value::Unit }
fn _native_assert_msg(_args: &[registry::Value]) -> registry::Value { registry::Value::Unit }
fn _native_test(_args: &[registry::Value]) -> registry::Value       { registry::Value::Unit }

const ASSERT_FUNCTIONS: &[(&str, registry::NativeFn)] = &[
    ("assert",     _native_assert),
    ("assert_msg", _native_assert_msg),
    ("test",       _native_test),
];

// -- global registry ----------------------------------------------------------

/// All built-in `musi:*` modules, in declaration order.
pub static REGISTRY: &[NativeModuleEntry] = &[
    NativeModuleEntry {
        specifier: io::io::SPECIFIER,
        source:    io::io::MUSI_SOURCE,
        functions: io::io::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: string::string::SPECIFIER,
        source:    string::string::MUSI_SOURCE,
        functions: string::string::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: array::array::SPECIFIER,
        source:    array::array::MUSI_SOURCE,
        functions: array::array::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: math::math::SPECIFIER,
        source:    math::math::MUSI_SOURCE,
        functions: math::math::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: num::num::SPECIFIER,
        source:    num::num::MUSI_SOURCE,
        functions: num::num::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: "musi:assert",
        source:    ASSERT_SOURCE,
        functions: ASSERT_FUNCTIONS,
    },
];

/// Return the Musi source text for a given `musi:*` specifier, or `None`.
#[must_use]
pub fn source_for(specifier: &str) -> Option<&'static str> {
    REGISTRY.iter().find(|e| e.specifier == specifier).map(|e| e.source)
}
