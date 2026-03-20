//! `musi exec` - execute a `.seam` bytecode file directly.

use std::path::Path;
use std::{fs, process};

use msc_builtins::StdHost;
use msc_vm::{Vm, load, verify};

pub fn run(path: &Path) -> ! {
    let bytes = match fs::read(path) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("error: {}: {e}", path.display());
            process::exit(1);
        }
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
