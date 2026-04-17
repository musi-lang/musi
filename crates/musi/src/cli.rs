use std::path::PathBuf;

use clap::{Args, Parser, Subcommand, ValueEnum};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum DiagnosticsFormatArg {
    Text,
    Json,
}

#[derive(Debug, Parser)]
#[command(name = "musi")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Args)]
pub struct ReservedCommandArgs {
    #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
    pub args: Vec<String>,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    Init {
        path: Option<PathBuf>,
    },
    Check {
        target: Option<PathBuf>,
        #[arg(long, value_enum, default_value = "text")]
        diagnostics_format: DiagnosticsFormatArg,
    },
    Build {
        target: Option<PathBuf>,
        #[arg(long)]
        out: Option<PathBuf>,
        #[arg(long)]
        target_name: Option<String>,
    },
    Run {
        target: Option<PathBuf>,
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<String>,
    },
    Test {
        target: Option<PathBuf>,
    },
    Task {
        name: String,
        target: Option<PathBuf>,
    },
    Info {
        target: Option<PathBuf>,
    },
    Lsp,
    Compile(ReservedCommandArgs),
    Fmt(ReservedCommandArgs),
    Lint(ReservedCommandArgs),
    Bench(ReservedCommandArgs),
    Doc(ReservedCommandArgs),
    Coverage(ReservedCommandArgs),
    Serve(ReservedCommandArgs),
    Repl(ReservedCommandArgs),
    Eval(ReservedCommandArgs),
    Install {
        target: Option<PathBuf>,
    },
    Add(ReservedCommandArgs),
    Remove(ReservedCommandArgs),
    Update(ReservedCommandArgs),
    Outdated(ReservedCommandArgs),
    Audit(ReservedCommandArgs),
    Publish(ReservedCommandArgs),
    Clean(ReservedCommandArgs),
}
