//! `music run` — compile and run a `.ms` source file.

use std::{path::Path, process};

use musi_manifest::MusiManifest;
use musi_std::StdHost;
use musi_vm::{Vm, load, verify};

use crate::pipeline;

/// Compiles `path` and immediately runs it in the VM.
pub fn run(path: &Path, manifest: Option<&MusiManifest>, project_root: Option<&Path>) -> ! {
    let mut out = if manifest.is_some() {
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
    let Ok(bytes) = pipeline::run_backend(&mut out) else {
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
    let host = match StdHost::new(&module.foreign_fns) {
        Ok(h) => h,
        Err(e) => {
            eprintln!("error: {e}");
            process::exit(1);
        }
    };
    let mut vm = Vm::new(module);
    vm.set_host(Box::new(host));
    match vm.run() {
        Ok(_) => process::exit(0),
        Err(e) => {
            eprintln!("error: {e}");
            process::exit(2);
        }
    }
}
