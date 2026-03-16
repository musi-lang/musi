//! `music check` — lex, parse, and type-check a `.ms` source file.

use std::{path::Path, process};

use musi_manifest::MusiManifest;

use crate::pipeline;

/// Runs the frontend on `path` and exits with `0` on success, `1` on error.
pub fn run(path: &Path, manifest: Option<&MusiManifest>, project_root: Option<&Path>) -> ! {
    let result = if manifest.is_some() {
        pipeline::run_frontend_multi(path, manifest, project_root)
    } else {
        pipeline::run_frontend(path)
    };
    match result {
        Ok(_) => process::exit(0),
        Err(()) => process::exit(1),
    }
}
