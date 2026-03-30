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

#[test]
fn test_parse_handle_clause_empty_param_list() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "handle x with k of (| op() => y);");
    let lexed = Lexer::new("handle x with k of (| op() => y);").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}

#[test]
fn test_parse_handle_clause_param_list_allows_leading_trailing_commas() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "handle x with k of (| op(,a,b,) => y);");
    let lexed = Lexer::new("handle x with k of (| op(,a,b,) => y);").lex();
    let parsed = parse(source_id, &lexed);

    assert!(parsed.errors().is_empty());
}

#[test]
fn test_parse_fstring_interpolation_as_syntax_subtree() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "f\"x is {x + 1}\";");
    let lexed = Lexer::new("f\"x is {x + 1}\";").lex();
    let parsed = parse(source_id, &lexed);
    let root = parsed.tree().root();
    let sequence = Expr::cast(root.child_nodes().next().expect("stmt")).expect("sequence");
    let fstring = sequence.child_expressions().next().expect("fstring");

    assert!(parsed.errors().is_empty());
    assert_eq!(fstring.kind(), ExprKindView::FString);
    assert!(fstring.child_expressions().next().is_some());
}
