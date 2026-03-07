//! Rust-backed `musi:*` system modules.
//!
//! Each submodule is annotated with `#[musi_module]` and produces:
//! - `SPECIFIER`   — the `"musi:..."` import string
//! - `MUSI_SOURCE` — Musi source the compiler parses (intrinsic declarations)
//! - `FUNCTIONS`   — `&[(&str, NativeFn)]` registered into the VM at startup
//!
//! The flat [`REGISTRY`] slice is consumed by the CLI resolver (to serve
//! inline source for `musi:*` imports) and by the VM (for dispatch).

#![allow(clippy::module_name_repetitions)]

pub mod registry;

mod fs;
mod os;
mod path;
mod process;
mod time;

use registry::NativeModuleEntry;

// -- musi:assert (VM-intercepted; wrapper bodies are unreachable) -------------

const ASSERT_SOURCE: &str = concat!(
    "#[intrinsic(\"assert\")] export extrin fn assert(cond: Bool): Unit;\n",
    "#[intrinsic(\"assert_msg\")] export extrin fn assert_msg(cond: Bool, msg: String): Unit;\n",
    "#[intrinsic(\"test\")] export extrin fn test(name: String, f: () -> Unit): Unit;\n",
);

const fn _native_assert(_args: &[registry::Value]) -> registry::Value     { registry::Value::Unit }
const fn _native_assert_msg(_args: &[registry::Value]) -> registry::Value { registry::Value::Unit }
const fn _native_test(_args: &[registry::Value]) -> registry::Value       { registry::Value::Unit }

const ASSERT_FUNCTIONS: &[(&str, registry::NativeFn)] = &[
    ("assert",     _native_assert),
    ("assert_msg", _native_assert_msg),
    ("test",       _native_test),
];

// -- global registry ----------------------------------------------------------

/// All `musi:*` system modules in declaration order.
pub static REGISTRY: &[NativeModuleEntry] = &[
    NativeModuleEntry {
        specifier: fs::fs::SPECIFIER,
        source:    fs::fs::MUSI_SOURCE,
        functions: fs::fs::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: path::path::SPECIFIER,
        source:    path::path::MUSI_SOURCE,
        functions: path::path::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: os::os::SPECIFIER,
        source:    os::os::MUSI_SOURCE,
        functions: os::os::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: process::process::SPECIFIER,
        source:    process::process::MUSI_SOURCE,
        functions: process::process::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: time::time::SPECIFIER,
        source:    time::time::MUSI_SOURCE,
        functions: time::time::FUNCTIONS,
    },
    NativeModuleEntry {
        specifier: "musi:assert",
        source:    ASSERT_SOURCE,
        functions: ASSERT_FUNCTIONS,
    },
];

/// Return the Musi source text for a `musi:*` specifier, or `None`.
#[must_use]
pub fn source_for(specifier: &str) -> Option<&'static str> {
    REGISTRY.iter().find(|e| e.specifier == specifier).map(|e| e.source)
}
