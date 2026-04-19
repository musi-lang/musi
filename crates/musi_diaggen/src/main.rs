mod command;
mod error;
mod model;
mod parser;
mod render;
mod validate;

use std::process::ExitCode;

fn main() -> ExitCode {
    command::run_from_env()
}
