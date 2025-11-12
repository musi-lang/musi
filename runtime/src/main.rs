use clap::{Parser, Subcommand};

extern crate alloc;

pub mod binary;
pub mod cmds;
pub mod frame;
pub mod instr;
pub mod loader;
pub mod stack;
pub mod types;
pub mod value;
pub mod vm;

#[derive(Parser)]
#[clap(name = "musi", version, about = "Musi runtime")]
struct Cli {
    #[clap(subcommand)]
    command: Commands,
}

#[derive(Subcommand)]
enum Commands {
    Run { file: String },
    Disasm { file: String },
}

fn main() {
    let cli = Cli::parse();

    match cli.command {
        Commands::Run { file } => unsafe { cmds::run(&file) },
        Commands::Disasm { file } => cmds::disasm(&file),
    }
}
