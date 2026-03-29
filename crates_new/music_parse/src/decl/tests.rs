use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_import_expr() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "let Foo := import \"x\";");
    let lexed = Lexer::new("let Foo := import \"x\";").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}

#[test]
fn test_import_alias_is_rejected() {
    let source = r#"import "x" as Foo;"#;
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", source);
    let lexed = Lexer::new(source).lex();
    let parsed = parse(source_id, &lexed);

    assert!(
        parsed
            .errors()
            .iter()
            .any(|error| matches!(error.kind, crate::ParseErrorKind::ImportAliasNotSupported))
    );
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
