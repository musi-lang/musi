mod build;
mod check;
mod format;
mod initialize;
mod install;
mod metadata;
mod project_target;
mod reserved;
mod run;
mod runtime;
mod task;
mod test_command;
mod test_report;

use crate::cli::Command;
use crate::error::{MusiError, MusiResult};
use musi_lsp::run_stdio_server;

pub fn run_command(command: Command) -> MusiResult {
    match command {
        Command::Init { path } => initialize::init_package(path.as_deref()),
        Command::Check {
            target,
            workspace,
            diagnostics_format,
        } => check::check(target.as_deref(), workspace, diagnostics_format.into()),
        Command::Build {
            target,
            workspace,
            out,
            target_name,
        } => build::build(
            target.as_deref(),
            workspace,
            out.as_deref(),
            target_name.as_deref(),
        ),
        Command::Run { target, args } => run::run_project(target.as_deref(), &args),
        Command::Test { target, workspace } => {
            test_command::test_project(target.as_deref(), workspace)
        }
        Command::Task { name, target } => task::run_task(&name, target.as_deref()),
        Command::Info { target } => metadata::print_project_metadata(target.as_deref()),
        Command::Install { target } => install::install_project(target.as_deref()),
        Command::Lsp => run_stdio_server().map_err(|source| MusiError::LspServerFailed {
            message: source.to_string(),
        }),
        Command::Fmt(args) => format::fmt_project(&args),
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
        | Command::Clean(_) => reserved::reserved_command_for(command),
    }
}
