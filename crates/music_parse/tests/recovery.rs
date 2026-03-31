use music_basic::SourceMap;
use music_lex::Lexer;
use music_parse::parse;

#[test]
fn test_parse_recovery_keeps_tree_for_broken_fixture() {
    let source = include_str!("fixtures/broken_module.ms");
    let mut sources = SourceMap::default();
    let source_id = sources.add("broken_module.ms", source);
    let lexed = Lexer::new(source).lex();
    let parsed = parse(source_id, &lexed);

    assert!(!parsed.errors().is_empty());
}
