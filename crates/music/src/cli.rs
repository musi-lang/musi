use std::path::PathBuf;

use clap::{Parser, Subcommand, ValueEnum};

#[derive(Debug, Clone, Copy, PartialEq, Eq, ValueEnum)]
pub enum DiagnosticsFormatArg {
    Text,
    Json,
}

#[derive(Debug, Parser)]
#[command(name = "music")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Command,
}

#[derive(Debug, Subcommand)]
pub enum Command {
    Check {
        path: PathBuf,
        #[arg(long, value_enum, default_value = "text")]
        diagnostics_format: DiagnosticsFormatArg,
    },
    Build {
        path: PathBuf,
        #[arg(long)]
        out: Option<PathBuf>,
    },
    Run {
        path: PathBuf,
        #[arg(trailing_var_arg = true, allow_hyphen_values = true)]
        args: Vec<String>,
    },
    Info {
        path: PathBuf,
    },
    Disasm {
        path: PathBuf,
    },
}
