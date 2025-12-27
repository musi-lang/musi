mod cli;
mod commands;
mod dispatcher;
mod types;

use clap::Parser;

use crate::cli::Cli;

fn main() {
    let args = Cli::parse();
    dispatcher::run(args);
}
