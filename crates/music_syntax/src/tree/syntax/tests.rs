use music_arena::{Arena, SliceArena};
use music_base::Span;

use crate::trivia::TriviaRange;
use crate::{LexedSource, Token, TokenKind};

use super::{
    SyntaxElement, SyntaxElementId, SyntaxNode, SyntaxNodeData, SyntaxNodeKind, SyntaxTokenId,
    SyntaxTree,
};

#[test]
fn syntax_tree_exposes_root_and_token_text() {
    let lexed = LexedSource::new(
        "x;",
        vec![
            Token::new(TokenKind::Ident, Span::new(0, 1)),
            Token::new(TokenKind::Semicolon, Span::new(1, 2)),
            Token::new(TokenKind::Eof, Span::new(2, 2)),
        ],
        Vec::new(),
        vec![TriviaRange::EMPTY, TriviaRange::EMPTY, TriviaRange::EMPTY],
        Vec::new(),
    );
    let mut nodes = Arena::new();
    let mut children = SliceArena::new();
    let stmt_children = children.alloc_from_iter([
        SyntaxElementId::Token(SyntaxTokenId::from_raw(0)),
        SyntaxElementId::Token(SyntaxTokenId::from_raw(1)),
    ]);
    let stmt = nodes.alloc(SyntaxNodeData::new(
        SyntaxNodeKind::SequenceExpr,
        Span::new(0, 2),
        stmt_children,
    ));
    let root_children = children.alloc_from_iter([SyntaxElementId::Node(stmt)]);
    let root = nodes.alloc(SyntaxNodeData::new(
        SyntaxNodeKind::SourceFile,
        Span::new(0, 2),
        root_children,
    ));
    nodes.get_mut(stmt).parent = Some(root);
    let tree = SyntaxTree::new(
        lexed,
        nodes,
        children,
        vec![Some(stmt), Some(stmt), None],
        root,
    );

    let root_node = tree.root();
    assert_eq!(root_node.kind(), SyntaxNodeKind::SourceFile);
    let stmt_node = root_node.child_nodes().next().unwrap();
    assert_eq!(stmt_node.kind(), SyntaxNodeKind::SequenceExpr);
    assert_eq!(stmt_node.child_tokens().next().unwrap().text(), Some("x"));
}

#[test]
fn syntax_element_span_matches_inner_item() {
    let lexed = LexedSource::new("", Vec::new(), Vec::new(), Vec::new(), Vec::new());
    let mut nodes = Arena::new();
    let mut children = SliceArena::new();
    let range = children.alloc_from_iter(Vec::<SyntaxElementId>::new());
    let root = nodes.alloc(SyntaxNodeData::new(
        SyntaxNodeKind::SourceFile,
        Span::new(0, 0),
        range,
    ));
    let tree = SyntaxTree::new(lexed, nodes, children, Vec::new(), root);
    let node = SyntaxNode::new(&tree, root);
    assert_eq!(SyntaxElement::Node(node).span(), Span::new(0, 0));
}
