mod cli;
mod commands;
mod diag;
mod error;
mod workspace;

use std::process::ExitCode;

use clap::Parser;
use cli::{Cli, Command};
use commands::{
    build, check, fmt_project, init_package, install_project, print_project_metadata,
    reserved_command_for, run_project, run_task, test_project,
};
use error::{MusiError, MusiResult};
use musi_lsp::run_stdio_server;

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
    let command = cli.command;
    match command {
        Command::Init { path } => init_package(path.as_deref()),
        Command::Check {
            target,
            workspace,
            diagnostics_format,
        } => check(target.as_deref(), workspace, diagnostics_format.into()),
        Command::Build {
            target,
            workspace,
            out,
            target_name,
        } => build(
            target.as_deref(),
            workspace,
            out.as_deref(),
            target_name.as_deref(),
        ),
        Command::Run { target, args } => run_project(target.as_deref(), &args),
        Command::Test { target, workspace } => test_project(target.as_deref(), workspace),
        Command::Task { name, target } => run_task(&name, target.as_deref()),
        Command::Info { target } => print_project_metadata(target.as_deref()),
        Command::Install { target } => install_project(target.as_deref()),
        Command::Lsp => run_stdio_server().map_err(|source| MusiError::LspServerFailed {
            message: source.to_string(),
        }),
        Command::Fmt(args) => fmt_project(&args),
        Command::Compile(_)
        | Command::Lint(_)
        | Command::Bench(_)
        | Command::Doc(_)
        | Command::Coverage(_)
        | Command::Serve(_)
        | Command::Repl(_)
        | Command::Eval(_)
        | Command::Add(_)
        | Command::Remove(_)
        | Command::Update(_)
        | Command::Outdated(_)
        | Command::Audit(_)
        | Command::Publish(_)
        | Command::Clean(_) => reserved_command_for(command),
    }
}
