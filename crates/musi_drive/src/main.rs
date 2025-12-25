mod cmds;

use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "music", version, about = "Musi compiler")]
struct Cli {
    #[command(subcommand)]
    command: Option<Commands>,

    /// Source files to compile (when no subcommand given)
    files: Vec<PathBuf>,

    /// Emit intermediate representations
    #[arg(long, value_name = "KIND")]
    emit: Option<EmitKind>,
}

#[derive(Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum EmitKind {
    Tokens,
    Ast,
    Bytecode,
}

#[derive(Subcommand)]
enum Commands {
    /// Type check without emitting output
    Check {
        /// Source files to check
        files: Vec<PathBuf>,
    },
    /// Compile source files to .mso bytecode
    Compile {
        /// Source files to compile
        files: Vec<PathBuf>,

        /// Output directory
        #[arg(short, long, default_value = "./dist")]
        out_dir: PathBuf,
    },
    /// Build project from mspackage.json
    Build {
        /// Project directory
        #[arg(short, long, default_value = ".")]
        project: PathBuf,
    },
    /// Initiate new mspackage.json
    Init {
        /// Project directory
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    /// Watch for changes and recompile
    Watch {
        /// Source files to watch
        files: Vec<PathBuf>,
    },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Some(Commands::Check { files }) => cmds::check(&files, cli.emit),
        Some(Commands::Compile { files, out_dir }) => cmds::compile(&files, &out_dir, cli.emit),
        Some(Commands::Build { project }) => cmds::build(&project, cli.emit),
        Some(Commands::Init { path }) => cmds::init(&path),
        Some(Commands::Watch { files }) => cmds::watch(&files, cli.emit),
        None => {
            if cli.files.is_empty() {
                cmds::stdin(cli.emit);
            } else {
                cmds::check(&cli.files, cli.emit);
            }
        }
    }
}
