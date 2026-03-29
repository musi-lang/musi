use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_import_expr() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "import \"x\" as Foo;");
    let lexed = Lexer::new("import \"x\" as Foo;").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}

#[test]
fn test_parse_typed_effect_set_in_let_expr() {
    let source = "let run with { Read[Int], Write } := work;";
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", source);
    let lexed = Lexer::new(source).lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}

#[test]
fn test_parse_empty_effect_set_in_let_expr() {
    let source = "let run with {} := work;";
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", source);
    let lexed = Lexer::new(source).lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}

#[test]
fn test_parse_sep_only_effect_set_in_let_expr() {
    let source = "let run with {,} := work;";
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", source);
    let lexed = Lexer::new(source).lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}
