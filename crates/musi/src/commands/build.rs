use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Args;
use music_emit::{emit_project, write_seam};
use musi::driver::{compile_project, emit_project_diagnostics};

#[derive(Args)]
pub struct BuildArgs {
    /// Source file to compile.
    pub file: PathBuf,
    /// Output path for the `.seam` bytecode file (default: input path with `.seam` extension).
    #[arg(short, long)]
    pub output: Option<PathBuf>,
}

pub fn run(args: &BuildArgs) -> ExitCode {
    let result = match compile_project(&args.file) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2_u8);
        }
    };

    emit_project_diagnostics(&result);

    if result.has_errors {
        return ExitCode::FAILURE;
    }

    let module = match emit_project(result.project) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::FAILURE;
        }
    };
    let seam = write_seam(&module.module);

    let out_path = args
        .output
        .as_deref()
        .map_or_else(|| args.file.with_extension("seam"), Path::to_path_buf);

    if let Err(e) = fs::write(&out_path, &seam) {
        eprintln!("error: failed to write {}: {e}", out_path.display());
        return ExitCode::from(2_u8);
    }

    ExitCode::SUCCESS
}
