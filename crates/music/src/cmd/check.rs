//! `music check` — lex, parse, and type-check a `.ms` source file.

use std::{path::Path, process};

use crate::pipeline;

/// Runs the frontend on `path` and exits with `0` on success, `1` on error.
pub fn run(path: &Path) -> ! {
    match pipeline::run_frontend(path) {
        Ok(_) => process::exit(0),
        Err(()) => process::exit(1),
    }
}
