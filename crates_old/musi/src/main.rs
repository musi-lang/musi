//! The `musi` command-line driver.

pub(crate) mod cmds;
pub(crate) mod compiler;
pub(crate) mod config;
pub(crate) mod deps;
pub(crate) mod fetch;
pub(crate) mod lock;

use clap::{Parser, Subcommand};

use cmds::{add_cmd, build_cmd, check_cmd, init_cmd, install_cmd, run_cmd, task_cmd, test_cmd};

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
    /// Compile a Musi program to a .mso binary
    Build(build_cmd::BuildArgs),
    /// Initialize a new Musi package
    Init(init_cmd::InitArgs),
    /// Type-check a Musi program
    Check(check_cmd::CheckArgs),
    /// Run tests (*.test.ms / *.spec.ms)
    Test(test_cmd::TestArgs),
    /// Run a project task defined in mspackage.json
    Task(task_cmd::TaskArgs),
    /// Install dependencies from mspackage.json
    Install(install_cmd::InstallArgs),
    /// Add a dependency to mspackage.json
    Add(add_cmd::AddArgs),
}

fn main() {
    let cli = Cli::parse();
    match cli.command {
        Cmd::Run(args) => run_cmd::run(args),
        Cmd::Build(args) => build_cmd::run(args),
        Cmd::Init(args) => init_cmd::run(args),
        Cmd::Check(args) => check_cmd::run(&args),
        Cmd::Test(args) => test_cmd::run(args),
        Cmd::Task(args) => task_cmd::run(&args),
        Cmd::Install(args) => install_cmd::run(&args),
        Cmd::Add(args) => add_cmd::run(&args),
    }
}
