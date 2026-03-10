//! `music` — Musi compiler.

use std::path::PathBuf;

use clap::{Parser, Subcommand};

mod cmd;
mod pipeline;

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
        /// Source file to check
        file: PathBuf,
    },
    /// Compile a `.ms` source file to `.msbc` bytecode
    Build {
        /// Source file to compile
        file: PathBuf,
        /// Output file path
        #[arg(short, long)]
        output: Option<PathBuf>,
    },
    /// Compile and run a `.ms` source file
    Run {
        /// Source file to run
        file: PathBuf,
    },
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Command::Check { file } => cmd::check::run(&file),
        Command::Build { file, output } => cmd::build::run(&file, output.as_deref()),
        Command::Run { file } => cmd::run::run(&file),
    }
}
