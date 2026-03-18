//! `msc` - Musi compiler.

use std::env;
use std::path::{Path, PathBuf};
use std::process;

use clap::{Parser, Subcommand};
use msc_manifest::MusiManifest;

mod cmd;
mod pipeline;
mod resolve_config;

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
    /// Compile and run a `.ms` source file
    Run {
        /// Source file to run (defaults to `main` from `musi.json`)
        file: Option<PathBuf>,
    },
    /// Add a dependency to musi.json
    Add {
        /// Dependency specifier (e.g. "git:github.com/user/repo")
        specifier: String,
        /// Override dependency name
        #[arg(long)]
        name: Option<String>,
        /// Add as dev dependency
        #[arg(long)]
        dev: bool,
    },
    /// Format source files
    Fmt {
        /// Files to format (defaults to project files)
        files: Vec<PathBuf>,
        /// Check formatting without modifying files
        #[arg(long)]
        check: bool,
    },
    /// Initialize a Musi project in the current directory
    Init {
        /// Project template
        #[arg(long, default_value = "bin")]
        template: String,
    },
    /// Lint source files
    Lint {
        /// Files to lint
        files: Vec<PathBuf>,
    },
    /// Create a new Musi project
    New {
        /// Project name
        name: String,
        /// Project template
        #[arg(long, default_value = "bin")]
        template: String,
    },
    /// Run a task from musi.json
    Task {
        /// Task name (omit to list all tasks)
        name: Option<String>,
        /// List all available tasks
        #[arg(long)]
        list: bool,
    },
    /// Run tests
    Test {
        /// Filter test names
        filter: Option<String>,
    },
}

fn main() {
    let cli = Cli::parse();

    let source_hint = match &cli.command {
        Command::Check { file } | Command::Build { file, .. } | Command::Run { file } => {
            file.as_deref()
        }
        _ => None,
    };

    let (manifest, project_root) = match load_manifest(source_hint) {
        Some((m, r)) => (Some(m), Some(r)),
        None => (None, None),
    };

    match cli.command {
        Command::Check { file } => {
            let path = resolve_entry(file.as_deref(), manifest.as_ref());
            cmd::check::run(&path, manifest.as_ref(), project_root.as_deref());
        }
        Command::Build { file, output } => {
            let path = resolve_entry(file.as_deref(), manifest.as_ref());
            cmd::build::run(
                &path,
                output.as_deref(),
                manifest.as_ref(),
                project_root.as_deref(),
            );
        }
        Command::Run { file } => {
            let path = resolve_entry(file.as_deref(), manifest.as_ref());
            cmd::run::run(&path, manifest.as_ref(), project_root.as_deref());
        }
        Command::Add {
            specifier,
            name,
            dev,
        } => {
            cmd::add::run(&specifier, name.as_deref(), dev);
        }
        Command::Fmt { files, check } => {
            cmd::fmt::run(&files, check, manifest.as_ref());
        }
        Command::Init { template } => {
            cmd::init::run(&template);
        }
        Command::Lint { files } => {
            cmd::lint::run(&files, manifest.as_ref());
        }
        Command::New { name, template } => {
            cmd::new::run(&name, &template);
        }
        Command::Task { name, list } => {
            cmd::task::run(name.as_deref(), list, manifest.as_ref());
        }
        Command::Test { filter } => {
            cmd::test::run(
                filter.as_deref(),
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

/// Resolves the source file path: uses the explicit argument if given,
/// otherwise falls back to `main` from `musi.json`.
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
