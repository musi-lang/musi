use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Args;

#[derive(Args)]
pub struct RunArgs {
    /// SEAM bytecode file to execute.
    pub file: PathBuf,
}

pub fn run(args: &RunArgs) -> ExitCode {
    let bytes = match fs::read(&args.file) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("error: cannot read {}: {e}", args.file.display());
            return ExitCode::from(2);
        }
    };
    let module = match musi_vm::load(&bytes) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: load failed: {e}");
            return ExitCode::from(2);
        }
    };
    let mut vm = musi_vm::Vm::new(module);
    match vm.run() {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::FAILURE
        }
    }
}
