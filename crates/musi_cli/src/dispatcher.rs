use crate::cli::{Cli, Commands};
use crate::commands;

pub fn run(cli: Cli) {
    match cli.command {
        Some(Commands::Check { files }) => commands::check(&files, cli.emit),
        Some(Commands::Compile { files, out_dir }) => commands::compile(&files, &out_dir, cli.emit),
        Some(Commands::Build { project }) => commands::build(&project, cli.emit),
        Some(Commands::Init { path }) => commands::init(&path),
        Some(Commands::Watch { files }) => commands::watch(&files, cli.emit),
        None => {
            if cli.files.is_empty() {
                commands::stdin(cli.emit);
            } else {
                commands::check(&cli.files, cli.emit);
            }
        }
    }
}
