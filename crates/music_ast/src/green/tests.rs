use music_basic::{SourceMap, Span};
use music_lex::{TokenKind, Trivias};
use music_storage::Arena;
use std::iter;

use super::*;

#[test]
fn test_syntax_tree_returns_stored_node_and_token_data() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "let x := 1");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();

    let token_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::KwLet,
        span: Span::new(0, 3),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::SourceFile,
        span: Span::new(0, 3),
        children: iter::once(SyntaxElementId::Token(token_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);

    assert_eq!(tree.root_id(), root_id);
    assert_eq!(tree.node_data(root_id).kind, SyntaxNodeKind::SourceFile);
    assert_eq!(tree.token_data(token_id).kind, TokenKind::KwLet);
}

#[test]
fn test_element_span_uses_correct_arena() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "x");

    let mut nodes = Arena::new();
    let mut tokens = Arena::new();

    let token_id = tokens.alloc(SyntaxTokenData {
        kind: TokenKind::Ident,
        span: Span::new(0, 1),
        leading_trivia: Trivias::new(),
        trailing_trivia: Trivias::new(),
    });
    let root_id = nodes.alloc(SyntaxNodeData {
        kind: SyntaxNodeKind::NameExpr,
        span: Span::new(0, 1),
        children: iter::once(SyntaxElementId::Token(token_id)).collect(),
    });

    let tree = SyntaxTree::new(source_id, nodes, tokens, root_id);

    assert_eq!(
        tree.element_span(SyntaxElementId::Node(root_id)),
        Span::new(0, 1)
    );
    assert_eq!(
        tree.element_span(SyntaxElementId::Token(token_id)),
        Span::new(0, 1)
    );
}
