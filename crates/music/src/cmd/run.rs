//! `music run` — compile and run a `.ms` source file.

use std::{path::Path, process};

use musi_manifest::MusiManifest;
use musi_vm::{Vm, load, verify};

use crate::pipeline;

/// Compiles `path` and immediately runs it in the VM.
pub fn run(path: &Path, manifest: Option<&MusiManifest>) -> ! {
    let out = if manifest.is_some() {
        match pipeline::run_frontend_multi(path, manifest) {
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
    let module = match load(&bytes) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: {e}");
            process::exit(1);
        }
    };
    if let Err(e) = verify(&module) {
        eprintln!("error: {e}");
        process::exit(1);
    }
    match Vm::new(module).run() {
        Ok(_) => process::exit(0),
        Err(e) => {
            eprintln!("error: {e}");
            process::exit(2);
        }
    }
}
