//! `music run` — compile and run a `.ms` source file.

use std::{path::Path, process};

use crate::pipeline;

/// Compiles `path` and immediately runs it in the VM.
pub fn run(path: &Path) -> ! {
    let Ok(out) = pipeline::run_frontend(path) else {
        process::exit(1)
    };
    let Ok(bytes) = pipeline::run_backend(&out) else {
        process::exit(1)
    };
    let module = match musi_vm::load(&bytes) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: {e}");
            process::exit(1);
        }
    };
    if let Err(e) = musi_vm::verify(&module) {
        eprintln!("error: {e}");
        process::exit(1);
    }
    match musi_vm::Vm::new(module).run() {
        Ok(_) => process::exit(0),
        Err(e) => {
            eprintln!("error: {e}");
            process::exit(2);
        }
    }
}
