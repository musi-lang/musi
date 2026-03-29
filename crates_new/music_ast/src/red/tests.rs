use music_basic::{SourceMap, Span};
use music_lex::{TokenKind, Trivias};
use music_storage::Arena;
use std::iter;

use crate::{SyntaxNodeData, SyntaxTokenData};

use super::*;

#[test]
fn test_root_node_exposes_children() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "instance Foo;");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();

    let token_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::KwInstance,
        span: Span::new(0, 8),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let expr_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::InstanceExpr,
        span: Span::new(0, 12),
        children: iter::once(SyntaxElementId::Token(token_id)).collect(),
    });
    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::SourceFile,
        span: Span::new(0, 13),
        children: iter::once(SyntaxElementId::Node(expr_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);
    let root = tree.root();
    let child = root.child_nodes().next().expect("root child");

    assert_eq!(root.kind(), SyntaxNodeKind::SourceFile);
    assert_eq!(child.kind(), SyntaxNodeKind::InstanceExpr);
}

#[test]
fn test_token_wrapper_exposes_trivia_and_span() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", " x");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();

    let token_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::Ident,
        span: Span::new(1, 2),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::NameExpr,
        span: Span::new(1, 2),
        children: iter::once(SyntaxElementId::Token(token_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);
    let token = tree.root().child_tokens().next().expect("token child");

    assert_eq!(token.span(), Span::new(1, 2));
    assert!(matches!(token.kind(), TokenKind::Ident));
    assert!(token.leading_trivia().is_empty());
}
