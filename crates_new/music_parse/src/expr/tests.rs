use music_ast::{Expr, ExprKindView};
use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_binary_expression() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "x + y;");
    let lexed = Lexer::new("x + y;").lex();
    let parsed = parse(source_id, &lexed);
    let root = parsed.tree().root();
    let expr = Expr::cast(root.child_nodes().next().expect("stmt")).expect("sequence");

    assert_eq!(expr.kind(), ExprKindView::Sequence);
}
