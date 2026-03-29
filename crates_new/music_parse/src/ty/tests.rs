use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_ty_annot() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "let x : A -> B := y;");
    let lexed = Lexer::new("let x : A -> B := y;").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}

#[test]
fn test_parse_rec_mut_ty_annot() {
    let source = "let x : mut (A -> B) := y;";
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", source);
    let lexed = Lexer::new(source).lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}
