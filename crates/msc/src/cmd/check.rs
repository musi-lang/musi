//! `msc check` - lex, parse, and type-check a `.ms` source file.

use std::{path::Path, process};

use msc_manifest::MusiManifest;

use crate::pipeline;

/// Runs the frontend on `path` and exits with `0` on success, `1` on error.
pub fn run(path: &Path, manifest: &MusiManifest, project_root: &Path) -> ! {
    let result = pipeline::run_frontend_multi(path, manifest, project_root);
    match result {
        Ok(_) => process::exit(0),
        Err(()) => process::exit(1),
    }
}
