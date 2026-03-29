use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_instance_member() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "instance Foo { law x := x };");
    let lexed = Lexer::new("instance Foo { law x := x };").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}
