use core::mem;

use music_ast::{SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken};
use music_lex::TokenKind;

pub(super) struct AstCursor<'tree> {
    elements: Vec<SyntaxElement<'tree>>,
    pos: usize,
}

impl<'tree> AstCursor<'tree> {
    pub(super) fn new(node: SyntaxNode<'tree>) -> Self {
        Self {
            elements: node.children().collect(),
            pos: 0,
        }
    }

    pub(super) fn peek(&self) -> Option<SyntaxElement<'tree>> {
        self.elements.get(self.pos).copied()
    }

    pub(super) fn bump(&mut self) -> Option<SyntaxElement<'tree>> {
        let el = self.elements.get(self.pos).copied();
        if el.is_some() {
            self.pos += 1;
        }
        el
    }

    pub(super) fn at_token(&self, expected: &TokenKind) -> bool {
        self.peek()
            .and_then(SyntaxElement::into_token)
            .is_some_and(|token| same_token_kind(token.kind(), expected))
    }

    pub(super) fn eat_token(&mut self, expected: &TokenKind) -> Option<SyntaxToken<'tree>> {
        if self.at_token(expected) {
            return self.bump().and_then(SyntaxElement::into_token);
        }
        None
    }

    pub(super) fn bump_token(&mut self) -> Option<SyntaxToken<'tree>> {
        self.bump().and_then(SyntaxElement::into_token)
    }

    pub(super) fn bump_node(&mut self) -> Option<SyntaxNode<'tree>> {
        self.bump().and_then(SyntaxElement::into_node)
    }

    pub(super) fn eat_node(&mut self, expected: SyntaxNodeKind) -> Option<SyntaxNode<'tree>> {
        let node = self.peek().and_then(SyntaxElement::into_node)?;
        if node.kind() == expected {
            self.pos += 1;
            Some(node)
        } else {
            None
        }
    }
}

pub(super) fn same_token_kind(left: &TokenKind, right: &TokenKind) -> bool {
    mem::discriminant(left) == mem::discriminant(right)
}
