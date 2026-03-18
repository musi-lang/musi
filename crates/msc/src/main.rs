//! `msc` - Musi compiler.

use std::env;
use std::path::{Path, PathBuf};
use std::process;

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
    /// Compile a `.ms` source file to `.muse` bytecode
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

    let (manifest, project_root) = match load_manifest(source_hint) {
        Some((m, r)) => (Some(m), Some(r)),
        None => (None, None),
    };

    match cli.command {
        Command::Check { file } => {
            let path = resolve_entry(file.as_deref(), manifest.as_ref());
            check::run(&path, manifest.as_ref(), project_root.as_deref());
        }
        Command::Build { file, output } => {
            let path = resolve_entry(file.as_deref(), manifest.as_ref());
            build::run(
                &path,
                output.as_deref(),
                manifest.as_ref(),
                project_root.as_deref(),
            );
        }
    }
}

fn load_manifest(source_hint: Option<&Path>) -> Option<(MusiManifest, PathBuf)> {
    let start = source_hint
        .and_then(|p| p.parent())
        .map_or_else(|| PathBuf::from("."), Path::to_path_buf);

    let mut dir = if start.is_relative() {
        env::current_dir().ok()?.join(&start)
    } else {
        start
    };

    loop {
        let candidate = dir.join("musi.json");
        if candidate.exists() {
            let manifest = msc_manifest::load(&candidate).ok()?;
            return Some((manifest, dir));
        }
        if !dir.pop() {
            return None;
        }
    }
}

fn resolve_entry(explicit: Option<&Path>, manifest: Option<&MusiManifest>) -> PathBuf {
    if let Some(p) = explicit {
        return p.to_path_buf();
    }

    if let Some(m) = manifest {
        return PathBuf::from(m.entry_point());
    }

    eprintln!("error: no source file specified and no musi.json found");
    process::exit(1);
}
