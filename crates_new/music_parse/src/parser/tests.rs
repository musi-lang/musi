use music_ast::{ExprKindView, SourceFile};
use music_basic::SourceMap;
use music_lex::Lexer;

use super::parse;

#[test]
fn test_parse_root_builds_source_file() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "let x := y;");
    let lexed = Lexer::new("let x := y;").lex();
    let parsed = parse(source_id, &lexed);
    let root = SourceFile::cast(parsed.tree().root()).expect("source file");
    let expr = root.expressions().next().expect("statement sequence");

    assert_eq!(expr.kind(), ExprKindView::Sequence);
}
