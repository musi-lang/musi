mod be;
mod common;
mod fe;
mod me;

fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();

    let input = std::fs::read_to_string("examples/conditional.musi").unwrap();
    let mut lexer = crate::fe::lexer::Lexer::new(input.as_bytes());
    let tokens = lexer.lex();
    for token in tokens {
        println!("{token:?}");
    }
    // let ast = parser::Parser::new(tokens).parse()?;
    // println!("{ast:?}");
}
