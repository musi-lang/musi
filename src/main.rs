#![allow(clippy::all, reason = "development")]
#![allow(clippy::restriction, reason = "development")]

use std::path::PathBuf;

use musi::{
    analysis::{lexical::lexer::Lexer, syntactic::parser::Parser},
    core::source::SourceFile,
};

fn main() {
    let path = PathBuf::from("examples/main.musi");
    let name = path.file_name().unwrap().to_string_lossy().to_string();
    let content = std::fs::read_to_string(&path).unwrap();
    let source = SourceFile::new(&name, content.into());

    let mut lexer = Lexer::new(source.into());
    let tokens = lexer.lex().unwrap_or_else(|error| {
        eprintln!("{error}");
        std::process::exit(1);
    });
    for token in &tokens {
        println!("{token:?}");
    }

    let mut parser = Parser::new(tokens);
    let program = parser.parse().unwrap_or_else(|error| {
        eprintln!("{error}");
        std::process::exit(1);
    });
    println!("{program:?}");
}
