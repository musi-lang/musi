//! The `musi` command-line driver.

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

mod cmds;
mod compiler;
mod config;

use clap::{Parser, Subcommand};

use cmds::{check_cmd, run_cmd, test_cmd};

#[derive(Parser)]
#[command(name = "musi", about = "The Musi language toolchain", version)]
struct Cli {
    #[command(subcommand)]
    command: Cmd,
}

#[derive(Subcommand)]
enum Cmd {
    /// Compile and execute a Musi program
    Run(run_cmd::RunArgs),
    /// Type-check a Musi program
    Check(check_cmd::CheckArgs),
    /// Run tests (*.test.ms / *.spec.ms)
    Test(test_cmd::TestArgs),
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Cmd::Run(args) => run_cmd::run(args),
        Cmd::Check(args) => check_cmd::run(args),
        Cmd::Test(args) => test_cmd::run(args),
    }
}
