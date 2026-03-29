use music_basic::{SourceId, Span};
use music_lex::{Token, TokenKind, Trivias};
use music_storage::Arena;

use super::*;

#[derive(Debug)]
pub struct SyntaxTreeBuilder {
    source_id: SourceId,
    nodes: Arena<SyntaxNodeData>,
    tokens: Arena<SyntaxTokenData>,
}

impl SyntaxTreeBuilder {
    #[must_use]
    pub const fn new(source_id: SourceId) -> Self {
        Self {
            source_id,
            nodes: Arena::new(),
            tokens: Arena::new(),
        }
    }

    #[must_use]
    pub fn with_capacity(source_id: SourceId, node_capacity: usize, token_capacity: usize) -> Self {
        Self {
            source_id,
            nodes: Arena::with_capacity(node_capacity),
            tokens: Arena::with_capacity(token_capacity),
        }
    }

    pub fn push_token(
        &mut self,
        kind: TokenKind,
        span: Span,
        leading_trivia: Trivias,
        trailing_trivia: Trivias,
    ) -> SyntaxTokenId {
        self.tokens.alloc(SyntaxTokenData {
            kind,
            span,
            leading_trivia,
            trailing_trivia,
        })
    }

    pub fn push_lex_token(&mut self, token: &Token) -> SyntaxTokenId {
        self.push_token(
            token.kind.clone(),
            token.span,
            token.leading_trivia.clone(),
            token.trailing_trivia.clone(),
        )
    }

    pub fn push_node(
        &mut self,
        kind: SyntaxNodeKind,
        span: Span,
        children: impl IntoIterator<Item = SyntaxElementId>,
    ) -> SyntaxNodeId {
        self.nodes.alloc(SyntaxNodeData {
            kind,
            span,
            children: children.into_iter().collect(),
        })
    }

    pub fn push_empty_node(&mut self, kind: SyntaxNodeKind, span: Span) -> SyntaxNodeId {
        self.push_node(kind, span, SyntaxNodeChildren::new())
    }

    #[must_use]
    pub fn finish(self, root: SyntaxNodeId) -> SyntaxTree {
        SyntaxTree::new(self.source_id, self.nodes, self.tokens, root)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
