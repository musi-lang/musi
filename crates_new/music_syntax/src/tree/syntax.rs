use music_arena::{Arena, Idx, SliceArena, SliceRange};
use music_base::Span;

use crate::{LexedSource, TokenKind, Trivia};

use super::kinds::SyntaxNodeKind;

pub type SyntaxNodeId = Idx<SyntaxNodeData>;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SyntaxTokenId {
    raw: u32,
}

impl SyntaxTokenId {
    #[must_use]
    pub const fn from_raw(raw: u32) -> Self {
        Self { raw }
    }

    #[must_use]
    pub const fn raw(self) -> u32 {
        self.raw
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum SyntaxElementId {
    Node(SyntaxNodeId),
    Token(SyntaxTokenId),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SyntaxNodeData {
    pub kind: SyntaxNodeKind,
    pub span: Span,
    pub parent: Option<SyntaxNodeId>,
    pub children: SliceRange<SyntaxElementId>,
}

#[derive(Debug)]
pub struct SyntaxTree<'src> {
    lexed: LexedSource<'src>,
    nodes: Arena<SyntaxNodeData>,
    children: SliceArena<SyntaxElementId>,
    token_parents: Vec<Option<SyntaxNodeId>>,
    root: SyntaxNodeId,
}

#[derive(Debug, Clone, Copy)]
pub struct SyntaxNode<'tree, 'src> {
    tree: &'tree SyntaxTree<'src>,
    id: SyntaxNodeId,
}

#[derive(Debug, Clone, Copy)]
pub struct SyntaxToken<'tree, 'src> {
    tree: &'tree SyntaxTree<'src>,
    id: SyntaxTokenId,
}

#[derive(Debug, Clone, Copy)]
pub enum SyntaxElement<'tree, 'src> {
    Node(SyntaxNode<'tree, 'src>),
    Token(SyntaxToken<'tree, 'src>),
}

impl<'src> SyntaxTree<'src> {
    #[must_use]
    pub const fn new(
        lexed: LexedSource<'src>,
        nodes: Arena<SyntaxNodeData>,
        children: SliceArena<SyntaxElementId>,
        token_parents: Vec<Option<SyntaxNodeId>>,
        root: SyntaxNodeId,
    ) -> Self {
        Self {
            lexed,
            nodes,
            children,
            token_parents,
            root,
        }
    }

    #[must_use]
    pub const fn lexed(&self) -> &LexedSource<'src> {
        &self.lexed
    }

    #[must_use]
    pub const fn root_id(&self) -> SyntaxNodeId {
        self.root
    }

    #[must_use]
    pub const fn root(&self) -> SyntaxNode<'_, 'src> {
        SyntaxNode::new(self, self.root)
    }

    #[must_use]
    pub fn node_data(&self, id: SyntaxNodeId) -> &SyntaxNodeData {
        self.nodes.get(id)
    }

    #[must_use]
    pub fn node_data_mut(&mut self, id: SyntaxNodeId) -> &mut SyntaxNodeData {
        self.nodes.get_mut(id)
    }

    #[must_use]
    pub fn node_children(&self, id: SyntaxNodeId) -> &[SyntaxElementId] {
        self.children.get(self.node_data(id).children)
    }

    #[must_use]
    pub fn token_kind(&self, id: SyntaxTokenId) -> TokenKind {
        let index = usize::try_from(id.raw()).unwrap_or(usize::MAX);
        self.lexed
            .tokens()
            .get(index)
            .map_or(TokenKind::Eof, |token| token.kind)
    }

    #[must_use]
    pub fn token_span(&self, id: SyntaxTokenId) -> Span {
        let index = usize::try_from(id.raw()).unwrap_or(usize::MAX);
        self.lexed
            .tokens()
            .get(index)
            .map(|token| token.span)
            .unwrap_or_default()
    }

    #[must_use]
    pub fn token_text(&self, id: SyntaxTokenId) -> Option<&'src str> {
        let index = usize::try_from(id.raw()).ok()?;
        self.lexed.token_text(index)
    }

    #[must_use]
    pub fn token_trivia(&self, id: SyntaxTokenId) -> &[Trivia] {
        let index = usize::try_from(id.raw()).unwrap_or(usize::MAX);
        self.lexed.token_trivia(index)
    }

    #[must_use]
    pub fn token_parent(&self, id: SyntaxTokenId) -> Option<SyntaxNodeId> {
        let index = usize::try_from(id.raw()).unwrap_or(usize::MAX);
        self.token_parents.get(index).copied().flatten()
    }

    #[must_use]
    pub const fn node_count(&self) -> usize {
        self.nodes.len()
    }

    #[must_use]
    pub fn token_count(&self) -> usize {
        self.lexed.tokens().len()
    }
}

impl<'tree, 'src> SyntaxNode<'tree, 'src> {
    #[must_use]
    pub const fn new(tree: &'tree SyntaxTree<'src>, id: SyntaxNodeId) -> Self {
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

    #[must_use]
    pub fn parent(self) -> Option<Self> {
        self.tree
            .node_data(self.id)
            .parent
            .map(|parent| Self::new(self.tree, parent))
    }

    pub fn children(self) -> impl Iterator<Item = SyntaxElement<'tree, 'src>> + 'tree {
        self.tree
            .node_children(self.id)
            .iter()
            .copied()
            .map(move |id| SyntaxElement::new(self.tree, id))
    }

    pub fn child_nodes(self) -> impl Iterator<Item = Self> + 'tree {
        self.children().filter_map(SyntaxElement::into_node)
    }

    pub fn child_tokens(self) -> impl Iterator<Item = SyntaxToken<'tree, 'src>> + 'tree {
        self.children().filter_map(SyntaxElement::into_token)
    }
}

impl<'tree, 'src> SyntaxToken<'tree, 'src> {
    #[must_use]
    pub const fn new(tree: &'tree SyntaxTree<'src>, id: SyntaxTokenId) -> Self {
        Self { tree, id }
    }

    #[must_use]
    pub const fn id(self) -> SyntaxTokenId {
        self.id
    }

    #[must_use]
    pub fn kind(self) -> TokenKind {
        self.tree.token_kind(self.id)
    }

    #[must_use]
    pub fn span(self) -> Span {
        self.tree.token_span(self.id)
    }

    #[must_use]
    pub fn text(self) -> Option<&'src str> {
        self.tree.token_text(self.id)
    }

    #[must_use]
    pub fn trivia(self) -> &'tree [Trivia] {
        self.tree.token_trivia(self.id)
    }

    #[must_use]
    pub fn parent(self) -> Option<SyntaxNode<'tree, 'src>> {
        self.tree
            .token_parent(self.id)
            .map(|parent| SyntaxNode::new(self.tree, parent))
    }
}

impl<'tree, 'src> SyntaxElement<'tree, 'src> {
    #[must_use]
    pub const fn new(tree: &'tree SyntaxTree<'src>, id: SyntaxElementId) -> Self {
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
    pub const fn into_node(self) -> Option<SyntaxNode<'tree, 'src>> {
        match self {
            Self::Node(node) => Some(node),
            Self::Token(_) => None,
        }
    }

    #[must_use]
    pub const fn into_token(self) -> Option<SyntaxToken<'tree, 'src>> {
        match self {
            Self::Token(token) => Some(token),
            Self::Node(_) => None,
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
