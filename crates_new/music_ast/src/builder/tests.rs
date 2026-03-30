use music_basic::{SourceMap, Span};
use music_lex::{Token, TokenKind, Trivias};

use crate::{Expr, ExprKindView, SourceFile};

use super::*;

#[test]
fn test_builder_creates_tree_from_tokens_and_nodes() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "instance X;");
    let mut builder = SyntaxTreeBuilder::with_capacity(source_id, 2, 2);

    let token_id = builder.push_lex_token(&Token {
        kind: TokenKind::KwInstance,
        span: Span::new(0, 8),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let expr_id = builder.push_node(
        SyntaxNodeKind::InstanceExpr,
        Span::new(0, 10),
        [SyntaxElementId::Token(token_id)],
    );
    let root_id = builder.push_node(
        SyntaxNodeKind::SourceFile,
        Span::new(0, 11),
        [SyntaxElementId::Node(expr_id)],
    );

    let tree = builder.finish(root_id);
    let root = SourceFile::cast(tree.root()).expect("source file");
    let expr = root.expressions().next().expect("expression");

    assert_eq!(expr.kind(), ExprKindView::Instance);
}

#[test]
fn test_builder_push_empty_node_allocates_without_children() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "_");
    let mut builder = SyntaxTreeBuilder::new(source_id);
    let expr_id = builder.push_empty_node(SyntaxNodeKind::NameExpr, Span::new(0, 1));
    let tree = builder.finish(expr_id);

    let expr = Expr::cast(tree.root()).expect("name expression");
    assert_eq!(expr.kind(), ExprKindView::Name);
    assert!(expr.syntax().child_nodes().next().is_none());
}
