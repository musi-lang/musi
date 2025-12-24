use clap::Parser;
use musi_basic::{diagnostic, interner::Interner, source::SourceMap};
use musi_lex::lexer;

use std::fs;
use std::io::{self, Read, Write as _};
use std::path::PathBuf;

#[derive(Parser)]
#[command(name = "music", version, about = "Musi compiler")]
struct Cli {
    file: Option<PathBuf>,

    #[arg(long)]
    dump_tokens: bool,
}

fn main() {
    let cli = Cli::parse();

    let input = if let Some(path) = cli.file {
        match fs::read_to_string(&path) {
            Ok(contents) => (path.display().to_string(), contents),
            Err(err) => {
                error(&format!("cannot read '{}': {err}", path.display()));
                return;
            }
        }
    } else {
        let mut buf = String::new();
        if io::stdin().read_to_string(&mut buf).is_ok() {
            ("<stdin>".into(), buf)
        } else {
            error("unable to read stdin");
            return;
        }
    };

    let mut source_map = SourceMap::new();
    let mut interner = Interner::new();

    let file_id = source_map.add_file(input.0, input.1);
    let Some(file) = source_map.get(file_id).cloned() else {
        error("internal: unable to add source file");
        return;
    };
    let (tokens, bag) = lexer::tokenize(&file, &mut interner);
    if bag.errors > 0 {
        diagnostic::emit_all(&bag, &source_map);
    }

    if cli.dump_tokens {
        for token in &tokens {
            eprintln!("{}", token.kind.display(&interner));
        }
    }
}

fn error(msg: &str) {
    let mut stderr = io::stderr();
    writeln!(stderr, "\x1b[1;31merror:\x1b[0m {msg}").unwrap_or(());
}
