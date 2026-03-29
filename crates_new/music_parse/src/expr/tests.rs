use music_ast::{Expr, ExprKindView};
use music_basic::SourceMap;
use music_lex::Lexer;

use crate::parse;

#[test]
fn test_parse_symbolic_binary_expr() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "x ++ y;");
    let lexed = Lexer::new("x ++ y;").lex();
    let parsed = parse(source_id, &lexed);
    let root = parsed.tree().root();
    let expr = Expr::cast(root.child_nodes().next().expect("stmt")).expect("sequence");

    assert!(parsed.errors().is_empty());
    assert_eq!(expr.kind(), ExprKindView::Sequence);
}

#[test]
fn test_parse_splice_expr() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "#(value);");
    let lexed = Lexer::new("#(value);").lex();
    let parsed = parse(source_id, &lexed);
    let root = parsed.tree().root();
    let sequence = Expr::cast(root.child_nodes().next().expect("stmt")).expect("sequence");
    let splice = sequence.child_expressions().next().expect("splice");

    assert!(parsed.errors().is_empty());
    assert_eq!(splice.kind(), ExprKindView::Splice);
}

#[test]
fn test_parse_call_arg_spread() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "f(...xs, x);");
    let lexed = Lexer::new("f(...xs, x);").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}
