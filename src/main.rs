use clap::Parser;
use musi::basic::{diagnostic, interner::Interner, source::SourceMap};
use musi::lex::lexer;
use thiserror as _;

use std::fs;
use std::io::{self, Read, Write as _};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "musi", version, about)]
struct Cli {
    file: Option<PathBuf>,

    #[arg(long)]
    dump_tokens: bool,
}

fn main() {
    let cli = Cli::parse();

    let input = match cli.file {
        Some(path) => match fs::read_to_string(&path) {
            Ok(contents) => (path.display().to_string(), contents),
            Err(err) => {
                error(&format!("cannot read '{}': {err}", path.display()));
                return;
            }
        },
        None => {
            let mut buf = String::new();
            if io::stdin().read_to_string(&mut buf).is_ok() {
                ("<stdin>".into(), buf)
            } else {
                error("unable to read stdin");
                return;
            }
        }
    };

    let mut source_map = SourceMap::new();
    let mut interner = Interner::new();

    let file_id = source_map.add_file(input.0, input.1);
    let file = source_map.get(file_id).cloned();
    let Some(file) = file else {
        error("internal: unable to add source file");
        return;
    };
    let (tokens, bag) = lexer::tokenize(&file, &mut interner);
    if bag.errors > 0 {
        diagnostic::emit_all(&bag, &source_map);
    }

    if cli.dump_tokens {
        for token in &tokens {
            let _ = eprintln!("{}", token.kind.display(&interner));
        }
    }
}

fn error(msg: &str) {
    let mut stderr = io::stderr();
    let _ = write!(stderr, "\x1b[1;31merror:\x1b[0m {msg}\n");
}
