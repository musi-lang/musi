use std::fs;
use std::path::PathBuf;
use std::process;

use clap::Args;

use crate::compiler;

#[derive(Args)]
pub(crate) struct BuildArgs {
    /// Output directory for compiled artifacts
    #[arg(long)]
    pub(crate) out_dir: Option<PathBuf>,

    /// Entry file to compile
    #[arg(long)]
    pub(crate) entry: Option<PathBuf>,
}

pub(crate) fn run(args: BuildArgs) {
    let Some((cfg, _)) = compiler::load_project_config() else {
        eprintln!("error: no mspackage.json found; run 'musi init' to create one");
        process::exit(1);
    };

    let entry = args
        .entry
        .unwrap_or_else(|| PathBuf::from(cfg.main.unwrap_or_else(|| "./index.ms".to_owned())));

    let out_dir = args.out_dir.unwrap_or_else(|| {
        PathBuf::from(
            cfg.compiler_options
                .out_dir
                .unwrap_or_else(|| "./dist".to_owned()),
        )
    });

    if let Err(e) = fs::create_dir_all(&out_dir) {
        eprintln!("error: cannot create output directory '{}': {e}", out_dir.display());
        process::exit(1);
    }

    let path = entry.to_string_lossy();
    let module = compiler::compile_file(&path);

    let bytes = module.serialize();
    let out_path = out_dir.join("main.mso");
    if let Err(e) = fs::write(&out_path, bytes) {
        eprintln!("error: cannot write '{}': {e}", out_path.display());
        process::exit(1);
    }

    println!("Built -> {}", out_path.display());
}
