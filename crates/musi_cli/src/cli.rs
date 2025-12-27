use clap::{Parser, Subcommand};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "music", version, about = "Musi compiler")]
pub struct Cli {
    #[command(subcommand)]
    pub command: Option<Commands>,

    pub files: Vec<PathBuf>,

    #[arg(long, value_name = "KIND")]
    pub emit: Option<EmitKind>,
}

#[derive(Clone, Copy, PartialEq, Eq, clap::ValueEnum)]
pub enum EmitKind {
    Tokens,
    Ast,
    Bytecode,
}

#[derive(Subcommand)]
pub enum Commands {
    Check {
        files: Vec<PathBuf>,
    },
    Compile {
        files: Vec<PathBuf>,

        #[arg(short, long, default_value = "./dist")]
        out_dir: PathBuf,
    },
    Build {
        #[arg(short, long, default_value = ".")]
        project: PathBuf,
    },
    Init {
        #[arg(default_value = ".")]
        path: PathBuf,
    },
    Watch {
        files: Vec<PathBuf>,
    },
}
