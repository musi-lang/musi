use std::fs;
use std::path::{Path, PathBuf};
use std::process::ExitCode;

use clap::Args;
use music_emit::{emit, write_seam};
use music_hir::HirBundle;

use musi::diagnostic::render;
use musi::driver::compile;

#[derive(Args)]
pub struct BuildArgs {
    /// Source file to compile.
    pub file: PathBuf,
    /// Output path for the `.seam` bytecode file (default: input path with `.seam` extension).
    #[arg(short, long)]
    pub output: Option<PathBuf>,
}

pub fn run(args: &BuildArgs) -> ExitCode {
    let result = match compile(&args.file) {
        Ok(r) => r,
        Err(e) => {
            eprintln!("error: {e}");
            return ExitCode::from(2_u8);
        }
    };

    for diag in &result.diagnostics {
        eprintln!("{}", render(diag, &result.db.source));
    }

    if result.has_errors {
        return ExitCode::FAILURE;
    }

    let bundle = HirBundle::new(result.db, result.resolution, result.type_env);
    let module = emit(&bundle);
    let seam = write_seam(&module);

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
