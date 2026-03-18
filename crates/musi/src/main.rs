//! `musi` - Musi bytecode interpreter.
//!
//! Loads and executes `.muse` binaries produced by the `msc` compiler.
//!
//! Exit codes:
//! - `0` success
//! - `1` load / verify error
//! - `2` runtime error

use std::{fs, path::PathBuf, process};

use clap::Parser;
use msc_builtins::StdHost;
use msc_vm::{Vm, load, verify};

#[derive(Parser)]
#[command(name = "musi", about = "Musi bytecode interpreter")]
struct Cli {
    /// Path to the `.muse` bytecode file
    file: PathBuf,
}

fn main() {
    let cli = Cli::parse();

    let bytes = match fs::read(&cli.file) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("error: {}: {e}", cli.file.display());
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
