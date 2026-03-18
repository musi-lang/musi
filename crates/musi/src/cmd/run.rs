//! `musi run` - compile and run a `.ms` source file.

use std::{path::Path, process};

use msc::pipeline;
use msc_builtins::StdHost;
use msc_manifest::MusiManifest;
use msc_vm::{Vm, load, verify};

/// Compiles `path` and immediately runs it in the VM.
pub fn run(path: &Path, manifest: &MusiManifest, project_root: &Path) -> ! {
    let Ok(mut out) = pipeline::run_frontend_multi(path, manifest, project_root) else {
        process::exit(1)
    };
    let Ok(bytes) = pipeline::run_backend(&mut out, true) else {
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
    let host = match StdHost::new(&module.foreign_fns, &module.types) {
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
