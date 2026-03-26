use std::path::PathBuf;
use std::process::ExitCode;

use clap::Args;

use musi::driver::compile;
use music_shared::diag::emit_to_stderr;

#[derive(Args)]
pub struct CheckArgs {
    /// Source file to type-check.
    pub file: PathBuf,
}

pub fn run(args: &CheckArgs) -> ExitCode {
    let result = match compile(&args.file) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2_u8);
        }
    };

    for diag in &result.diagnostics {
        emit_to_stderr(diag, &result.db.source);
    }

    if result.has_errors {
        ExitCode::FAILURE
    } else {
        ExitCode::SUCCESS
    }
}
