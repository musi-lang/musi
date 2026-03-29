use music_basic::{SourceId, Span};
use music_lex::{TokenKind, Trivias};
use music_storage::{Arena, Idx};
use smallvec::SmallVec;

use crate::kinds::SyntaxNodeKind;

pub type SyntaxNodeChildren = SmallVec<[SyntaxElementId; 6]>;
pub type SyntaxNodeId = Idx<SyntaxNodeData>;
pub type SyntaxTokenId = Idx<SyntaxTokenData>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxElementId {
    Node(SyntaxNodeId),
    Token(SyntaxTokenId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNodeData {
    pub kind: SyntaxNodeKind,
    pub span: Span,
    pub children: SyntaxNodeChildren,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxTokenData {
    pub kind: TokenKind,
    pub span: Span,
    pub leading_trivia: Trivias,
    pub trailing_trivia: Trivias,
}

#[derive(Debug)]
pub struct SyntaxTree {
    source_id: SourceId,
    nodes: Arena<SyntaxNodeData>,
    tokens: Arena<SyntaxTokenData>,
    root: SyntaxNodeId,
}

impl SyntaxTree {
    #[must_use]
    pub const fn new(
        source_id: SourceId,
        nodes: Arena<SyntaxNodeData>,
        tokens: Arena<SyntaxTokenData>,
        root: SyntaxNodeId,
    ) -> Self {
        Self {
            source_id,
            nodes,
            tokens,
            root,
        }
    }

    #[must_use]
    pub const fn source_id(&self) -> SourceId {
        self.source_id
    }

    #[must_use]
    pub const fn root_id(&self) -> SyntaxNodeId {
        self.root
    }

    #[must_use]
    pub fn node_data(&self, id: SyntaxNodeId) -> &SyntaxNodeData {
        self.nodes.get(id)
    }

    #[must_use]
    pub fn token_data(&self, id: SyntaxTokenId) -> &SyntaxTokenData {
        self.tokens.get(id)
    }

    #[must_use]
    pub fn element_span(&self, id: SyntaxElementId) -> Span {
        match id {
            SyntaxElementId::Node(node) => self.node_data(node).span,
            SyntaxElementId::Token(token) => self.token_data(token).span,
        }
    }

    #[must_use]
    pub const fn node_count(&self) -> usize {
        self.nodes.len()
    }

    #[must_use]
    pub const fn token_count(&self) -> usize {
        self.tokens.len()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
