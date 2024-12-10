use common::{errors::MusiResult, source::Source};
use fe::lexer::Lexer;

mod be;
mod common;
mod fe;
mod me;

fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();

    let path = std::path::PathBuf::from("examples/conditional.musi");
    let name = path.file_stem().unwrap().to_string_lossy().to_string();
    let content = std::fs::read_to_string(&path).unwrap();
    let source = Source::new(&name, content.into());
    let mut lexer = Lexer::new(source.into());
    let tokens = lexer.lex().unwrap_or_else(|error| {
        eprintln!("{error}");
        std::process::exit(1);
    });
    for token in tokens {
        println!("{token:?}");
    }
    // let ast = parser::Parser::new(tokens).parse()?;
    // println!("{ast:?}");
}
