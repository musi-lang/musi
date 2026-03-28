use std::path::PathBuf;
use std::process::ExitCode;

use clap::Args;

use musi::driver::{compile_project, emit_project_diagnostics};

#[derive(Args)]
pub struct CheckArgs {
    /// Source file to type-check.
    pub file: PathBuf,
}

pub fn run(args: &CheckArgs) -> ExitCode {
    let result = match compile_project(&args.file) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2_u8);
        }
    };

    emit_project_diagnostics(&result);

    if result.has_errors {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
