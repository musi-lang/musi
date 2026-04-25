mod cli;
mod commands;
mod diag;
mod error;
mod workspace;

use std::process::ExitCode;

use clap::Parser;
use cli::Cli;
use error::{MusiError, MusiResult};

fn main() -> ExitCode {
    match run() {
        Ok(()) => ExitCode::SUCCESS,
        Err(MusiError::CheckCommandFailed) => ExitCode::from(1),
        Err(error) => {
            eprintln!("{error}");
            ExitCode::from(1)
        }
    }
}

fn run() -> MusiResult {
    let cli = Cli::parse();
    commands::run_command(cli.command)
}
