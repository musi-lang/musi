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
mod io;
mod math;
mod os;
mod path;
mod process;
mod string;
mod time;

use registry::NativeModuleEntry;

/// All `musi:*` system modules in declaration order.
pub static REGISTRY: &[NativeModuleEntry] = &[
    NativeModuleEntry { specifier: fs::fs::SPECIFIER,         source: fs::fs::MUSI_SOURCE,         functions: fs::fs::FUNCTIONS },
    NativeModuleEntry { specifier: io::io::SPECIFIER,         source: io::io::MUSI_SOURCE,         functions: io::io::FUNCTIONS },
    NativeModuleEntry { specifier: math::math::SPECIFIER,     source: math::math::MUSI_SOURCE,     functions: math::math::FUNCTIONS },
    NativeModuleEntry { specifier: os::os::SPECIFIER,         source: os::os::MUSI_SOURCE,         functions: os::os::FUNCTIONS },
    NativeModuleEntry { specifier: path::path::SPECIFIER,     source: path::path::MUSI_SOURCE,     functions: path::path::FUNCTIONS },
    NativeModuleEntry { specifier: process::process::SPECIFIER, source: process::process::MUSI_SOURCE, functions: process::process::FUNCTIONS },
    NativeModuleEntry { specifier: string::string::SPECIFIER, source: string::string::MUSI_SOURCE, functions: string::string::FUNCTIONS },
    NativeModuleEntry { specifier: time::time::SPECIFIER,     source: time::time::MUSI_SOURCE,     functions: time::time::FUNCTIONS },
];

/// Return the Musi source text for a `musi:*` specifier, or `None`.
#[must_use]
pub fn source_for(specifier: &str) -> Option<&'static str> {
    REGISTRY.iter().find(|e| e.specifier == specifier).map(|e| e.source)
}
