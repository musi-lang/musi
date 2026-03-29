use music_basic::Span;
use music_lex::{TokenKind, Trivias};

use super::*;

#[derive(Debug, Clone, Copy)]
pub struct SyntaxNode<'tree> {
    tree: &'tree SyntaxTree,
    id: SyntaxNodeId,
}

#[derive(Debug, Clone, Copy)]
pub struct SyntaxToken<'tree> {
    tree: &'tree SyntaxTree,
    id: SyntaxTokenId,
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxElement<'tree> {
    Node(SyntaxNode<'tree>),
    Token(SyntaxToken<'tree>),
}

impl SyntaxTree {
    #[must_use]
    pub const fn root(&self) -> SyntaxNode<'_> {
        SyntaxNode::new(self, self.root_id())
    }
}

impl<'tree> SyntaxNode<'tree> {
    #[must_use]
    pub const fn new(tree: &'tree SyntaxTree, id: SyntaxNodeId) -> Self {
        Self { tree, id }
    }

    #[must_use]
    pub const fn id(self) -> SyntaxNodeId {
        self.id
    }

    #[must_use]
    pub fn kind(self) -> SyntaxNodeKind {
        self.tree.node_data(self.id).kind
    }

    #[must_use]
    pub fn span(self) -> Span {
        self.tree.node_data(self.id).span
    }

    pub fn children(self) -> impl Iterator<Item = SyntaxElement<'tree>> + 'tree {
        self.tree
            .node_data(self.id)
            .children
            .iter()
            .copied()
            .map(move |child| SyntaxElement::new(self.tree, child))
    }

    pub fn child_nodes(self) -> impl Iterator<Item = SyntaxNode<'tree>> + 'tree {
        self.children().filter_map(SyntaxElement::into_node)
    }

    pub fn child_tokens(self) -> impl Iterator<Item = SyntaxToken<'tree>> + 'tree {
        self.children().filter_map(SyntaxElement::into_token)
    }
}

impl<'tree> SyntaxToken<'tree> {
    #[must_use]
    pub const fn new(tree: &'tree SyntaxTree, id: SyntaxTokenId) -> Self {
        Self { tree, id }
    }

    #[must_use]
    pub const fn id(self) -> SyntaxTokenId {
        self.id
    }

    #[must_use]
    pub fn kind(self) -> &'tree TokenKind {
        &self.tree.token_data(self.id).kind
    }

    #[must_use]
    pub fn span(self) -> Span {
        self.tree.token_data(self.id).span
    }

    #[must_use]
    pub fn leading_trivia(self) -> &'tree Trivias {
        &self.tree.token_data(self.id).leading_trivia
    }

    #[must_use]
    pub fn trailing_trivia(self) -> &'tree Trivias {
        &self.tree.token_data(self.id).trailing_trivia
    }
}

impl<'tree> SyntaxElement<'tree> {
    #[must_use]
    pub const fn new(tree: &'tree SyntaxTree, id: SyntaxElementId) -> Self {
        match id {
            SyntaxElementId::Node(node) => Self::Node(SyntaxNode::new(tree, node)),
            SyntaxElementId::Token(token) => Self::Token(SyntaxToken::new(tree, token)),
        }
    }

    #[must_use]
    pub fn span(self) -> Span {
        match self {
            Self::Node(node) => node.span(),
            Self::Token(token) => token.span(),
        }
    }

    #[must_use]
    pub const fn into_node(self) -> Option<SyntaxNode<'tree>> {
        match self {
            Self::Node(node) => Some(node),
            Self::Token(_) => None,
        }
    }

    #[must_use]
    pub const fn into_token(self) -> Option<SyntaxToken<'tree>> {
        match self {
            Self::Node(_) => None,
            Self::Token(token) => Some(token),
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
