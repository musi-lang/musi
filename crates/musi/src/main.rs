mod commands;

use std::process::ExitCode;

use clap::{Parser, Subcommand};

use commands::{build::BuildArgs, check::CheckArgs, run::RunArgs, test::TestArgs};

#[derive(Parser)]
#[command(name = "musi", about = "Musi language compiler")]
struct Cli {
    #[command(subcommand)]
    command: Command,
}

#[derive(Subcommand)]
enum Command {
    /// Type-check a source file without producing output.
    Check(CheckArgs),
    /// Compile a source file to SEAM bytecode.
    Build(BuildArgs),
    /// Execute a SEAM bytecode file.
    Run(RunArgs),
    /// Discover and run `*.test.ms` files.
    Test(TestArgs),
}

fn main() -> ExitCode {
    let cli = Cli::parse();
    match cli.command {
        Command::Check(ref args) => commands::check::run(args),
        Command::Build(ref args) => commands::build::run(args),
        Command::Run(ref args) => commands::run::run(args),
        Command::Test(ref args) => commands::test::run(args),
    }
}
