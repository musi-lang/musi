use std::path::PathBuf;

use clap::{ArgAction, Args, Parser, Subcommand, ValueEnum};

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

#[derive(Debug, Args)]
pub struct FmtArgs {
    #[arg(long, action = ArgAction::Count)]
    pub all: u8,
    #[arg(long, action = ArgAction::Count)]
    pub check: u8,
    #[arg(long)]
    pub config: Option<PathBuf>,
    #[arg(long, action = ArgAction::Count)]
    pub no_config: u8,
    #[arg(long)]
    pub ext: Option<String>,
    #[arg(long)]
    pub line_width: Option<usize>,
    #[arg(long)]
    pub indent_width: Option<usize>,
    #[arg(long, action = ArgAction::Count)]
    pub use_tabs: u8,
    #[arg(long = "ignore")]
    pub ignore: Vec<String>,
    #[arg(long = "watch", action = ArgAction::Count)]
    pub watch: u8,
    #[arg(long = "watch-exclude")]
    pub watch_exclude: Vec<String>,
    #[arg(long = "no-clear-screen", action = ArgAction::Count)]
    pub no_clear_screen: u8,
    #[arg(long, action = ArgAction::Count)]
    pub permit_no_files: u8,
    #[arg(
        value_name = "PATH",
        trailing_var_arg = true,
        allow_hyphen_values = true
    )]
    pub paths: Vec<PathBuf>,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    Init {
        path: Option<PathBuf>,
    },
    Check {
        target: Option<PathBuf>,
        #[arg(long, action = ArgAction::Count)]
        workspace: u8,
        #[arg(long, value_enum, default_value = "text")]
        diagnostics_format: DiagnosticsFormatArg,
    },
    Build {
        target: Option<PathBuf>,
        #[arg(long, action = ArgAction::Count)]
        workspace: u8,
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
        #[arg(long, action = ArgAction::Count)]
        workspace: u8,
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
    Fmt(FmtArgs),
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
