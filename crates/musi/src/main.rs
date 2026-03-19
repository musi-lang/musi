//! `musi` - Musi toolchain.

use std::env;
use std::path::{Path, PathBuf};

use clap::{Parser, Subcommand};
use msc::cmd::{build, check};
use msc_manifest::MusiManifest;

mod cmd;

#[derive(Parser)]
#[command(name = "musi", about = "Musi toolchain")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Compile and run a `.ms` source file
    Run {
        /// Source file to run (defaults to `main` from `musi.json`)
        file: Option<PathBuf>,
    },
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
    /// Run tests
    Test {
        /// Filter by file path substring
        filter: Option<String>,
        /// Filter by test name substring (passed to runner)
        #[arg(long)]
        name: Option<String>,
    },
    /// Format source files
    Fmt {
        /// Files to format (defaults to project files)
        files: Vec<PathBuf>,
        /// Check formatting without modifying files
        #[arg(long)]
        check: bool,
    },
    /// Lint source files
    Lint {
        /// Files to lint
        files: Vec<PathBuf>,
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
    /// Create a new Musi project
    New {
        /// Project name
        name: String,
        /// Project template
        #[arg(long, default_value = "bin")]
        template: String,
    },
    /// Initialize a Musi project in the current directory
    Init {
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
    /// Execute a `.seam` bytecode file directly
    Exec {
        /// Path to the `.seam` bytecode file
        file: PathBuf,
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

    let (manifest, project_root) = load_manifest(source_hint);

    match cli.command {
        Command::Run { file } => {
            let path = resolve_entry(file.as_deref(), &manifest);
            cmd::run::run(&path, &manifest, &project_root);
        }
        Command::Check { file } => {
            let path = resolve_entry(file.as_deref(), &manifest);
            check::run(&path, &manifest, &project_root);
        }
        Command::Build { file, output } => {
            let path = resolve_entry(file.as_deref(), &manifest);
            build::run(&path, output.as_deref(), &manifest, &project_root);
        }
        Command::Test { filter, name } => {
            cmd::test::run(filter.as_deref(), name.as_deref(), &manifest, &project_root);
        }
        Command::Fmt { files, check } => {
            cmd::fmt::run(&files, check, &manifest);
        }
        Command::Lint { files } => {
            cmd::lint::run(&files, &manifest);
        }
        Command::Add {
            specifier,
            name,
            dev,
        } => {
            cmd::add::run(&specifier, name.as_deref(), dev);
        }
        Command::New { name, template } => {
            cmd::new::run(&name, &template);
        }
        Command::Init { template } => {
            cmd::init::run(&template);
        }
        Command::Task { name, list } => {
            cmd::task::run(name.as_deref(), list, &manifest);
        }
        Command::Exec { file } => {
            cmd::exec::run(&file);
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
