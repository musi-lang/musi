use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_pat_in_let() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "let x as y := z;");
    let lexed = Lexer::new("let x as y := z;").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}
