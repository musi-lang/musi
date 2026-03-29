use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_import_expression() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "import \"x\" as Foo;");
    let lexed = Lexer::new("import \"x\" as Foo;").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}
