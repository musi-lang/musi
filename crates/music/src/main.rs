//! `music` — Musi compiler.

use std::path::{Path, PathBuf};
use std::process;

use clap::{Parser, Subcommand};
use musi_manifest::MusiManifest;

mod cmd;
mod pipeline;
mod resolve_config;

#[derive(Parser)]
#[command(name = "music", about = "Musi compiler")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Lex, parse, and type-check a `.ms` source file
    Check {
        /// Source file to check (defaults to `package.main` from `mspackage.toml`)
        file: Option<PathBuf>,
    },
    /// Compile a `.ms` source file to `.msbc` bytecode
    Build {
        /// Source file to compile (defaults to `package.main` from `mspackage.toml`)
        file: Option<PathBuf>,
        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Compile and run a `.ms` source file
    Run {
        /// Source file to run (defaults to `package.main` from `mspackage.toml`)
        file: Option<PathBuf>,
    },
}

fn main() {
    let cli = Cli::parse();
    let manifest = load_manifest();

    match cli.command {
        Command::Check { file } => {
            let path = resolve_entry(file.as_deref(), manifest.as_ref());
            cmd::check::run(&path, manifest.as_ref());
        }
        Command::Build { file, output } => {
            let path = resolve_entry(file.as_deref(), manifest.as_ref());
            cmd::build::run(&path, output.as_deref(), manifest.as_ref());
        }
        Command::Run { file } => {
            let path = resolve_entry(file.as_deref(), manifest.as_ref());
            cmd::run::run(&path, manifest.as_ref());
        }
    }
}

fn load_manifest() -> Option<MusiManifest> {
    let manifest_path = PathBuf::from("mspackage.toml");
    if manifest_path.exists() {
        musi_manifest::load(&manifest_path).ok()
    } else {
        None
    }
}

/// Resolves the source file path: uses the explicit argument if given,
/// otherwise falls back to `package.main` from `mspackage.toml`.
fn resolve_entry(explicit: Option<&Path>, manifest: Option<&MusiManifest>) -> PathBuf {
    if let Some(p) = explicit {
        return p.to_path_buf();
    }

    if let Some(m) = manifest {
        return PathBuf::from(&m.package.main);
    }

    eprintln!("error: no source file specified and no mspackage.toml found");
    process::exit(1);
}
