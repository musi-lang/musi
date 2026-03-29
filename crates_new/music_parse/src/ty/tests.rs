use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_type_annotation() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "let x : A -> B := y;");
    let lexed = Lexer::new("let x : A -> B := y;").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}
