//! `msc` - Musi compiler.

use std::env;
use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use msc::cmd::{build, check};
use msc_manifest::MusiManifest;

#[derive(Parser)]
#[command(name = "msc", about = "Musi compiler")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Lex, parse, and type-check a `.ms` source file
    Check {
        /// Source file to check (defaults to `main` from `musi.json`)
        file: Option<PathBuf>,
    },
    /// Compile a `.ms` source file to `.seam` bytecode
    Build {
        /// Source file to compile (defaults to `main` from `musi.json`)
        file: Option<PathBuf>,
        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
}

fn main() {
    let cli = Cli::parse();

    let source_hint = match &cli.command {
        Command::Check { file } | Command::Build { file, .. } => file.as_deref(),
    };

    let (manifest, project_root) = load_manifest(source_hint);

    match cli.command {
        Command::Check { file } => {
            let path = resolve_entry(file.as_deref(), &manifest);
            check::run(&path, &manifest, &project_root);
        }
        Command::Build { file, output } => {
            let path = resolve_entry(file.as_deref(), &manifest);
            build::run(&path, output.as_deref(), &manifest, &project_root);
        }
    }
}

fn load_manifest(source_hint: Option<&Path>) -> (MusiManifest, PathBuf) {
    let start = source_hint
        .and_then(|p| p.parent())
        .map_or_else(|| PathBuf::from("."), Path::to_path_buf);

    let mut dir = if start.is_relative() {
        env::current_dir().map_or_else(|_| start.clone(), |cwd| cwd.join(&start))
    } else {
        start
    };

    loop {
        let candidate = dir.join("musi.json");
        if candidate.exists() {
            if let Ok(manifest) = msc_manifest::load(&candidate) {
                return (manifest, dir);
            }
        }
        if !dir.pop() {
            break;
        }
    }

    let root = source_hint.and_then(|p| p.parent()).map_or_else(
        || env::current_dir().unwrap_or_else(|_| PathBuf::from(".")),
        Path::to_path_buf,
    );
    (MusiManifest::default(), root)
}

fn resolve_entry(explicit: Option<&Path>, manifest: &MusiManifest) -> PathBuf {
    if let Some(p) = explicit {
        return p.to_path_buf();
    }
    PathBuf::from(manifest.entry_point())
}
