//! `music build` — compile a `.ms` source file to `.msbc` bytecode.

use std::{fs, path::Path, process};

use musi_manifest::MusiManifest;

use crate::pipeline;

/// Compiles `path` to bytecode and writes it to `output` (or `path.msbc`).
pub fn run(
    path: &Path,
    output: Option<&Path>,
    manifest: Option<&MusiManifest>,
    project_root: Option<&Path>,
) -> ! {
    let out = if manifest.is_some() {
        match pipeline::run_frontend_multi(path, manifest, project_root) {
            Ok(o) => o,
            Err(()) => process::exit(1),
        }
    } else {
        match pipeline::run_frontend(path) {
            Ok(o) => o,
            Err(()) => process::exit(1),
        }
    };
    let Ok(bytes) = pipeline::run_backend(&out) else {
        process::exit(1)
    };
    let out_path = output.map_or_else(|| path.with_extension("msbc"), Path::to_path_buf);
    match fs::write(&out_path, &bytes) {
        Ok(()) => process::exit(0),
        Err(e) => {
            eprintln!("error: {}: {e}", out_path.display());
            process::exit(1);
        }
    }
}
