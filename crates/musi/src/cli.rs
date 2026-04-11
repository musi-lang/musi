use std::path::PathBuf;

use clap::{Parser, Subcommand, ValueEnum};

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

#[derive(Debug, Subcommand)]
pub enum Command {
    New {
        name: String,
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
}
