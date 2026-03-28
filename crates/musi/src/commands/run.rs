use std::fs;
use std::path::PathBuf;
use std::process::ExitCode;

use clap::Args;
use musi::driver::{compile_project, emit_project_diagnostics};
use musi_vm::Vm;
use music_codegen::{emit_project, write_seam};

#[derive(Args)]
pub struct RunArgs {
    /// Source entry file or `.seam` bytecode file to execute.
    pub file: PathBuf,
}

pub fn run(args: &RunArgs) -> ExitCode {
    if args.file.extension().is_some_and(|ext| ext == "seam") {
        return run_seam_file(&args.file);
    }

    let result = match compile_project(&args.file) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2);
        }
    };

    emit_project_diagnostics(&result);

    if result.has_errors {
        return ExitCode::FAILURE;
    }

    let emitted = match emit_project(result.project) {
        Ok(module) => module,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::FAILURE;
        }
    };
    let bytes = write_seam(&emitted.module);
    run_seam_bytes(&bytes)
}

fn run_seam_file(path: &std::path::Path) -> ExitCode {
    let bytes = match fs::read(path) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("error: cannot read {}: {e}", path.display());
            return ExitCode::from(2);
        }
    };
    run_seam_bytes(&bytes)
}

fn run_seam_bytes(bytes: &[u8]) -> ExitCode {
    let module = match musi_vm::load(bytes) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: load failed: {e}");
            return ExitCode::from(2);
        }
    };
    let mut vm = Vm::new(module);
    match vm.run() {
        Ok(_) => ExitCode::SUCCESS,
        Err(e) => {
            eprintln!("error: {e}");
            ExitCode::FAILURE
        }
    }
}
