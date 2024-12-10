fn main() {
    env_logger::Builder::from_env(env_logger::Env::default().default_filter_or("debug")).init();

    let path = std::path::PathBuf::from("examples/main.musi");
    let name = path.file_stem().unwrap().to_string_lossy().to_string();
    let content = std::fs::read_to_string(&path).unwrap();
    let source = musi::source::Source::new(&name, content.into());
    let mut lexer = musi::fe::lexer::Lexer::new(source.into());
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
