use crate::{SyntaxNode, SyntaxNodeKind};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PatKindView {
    Wildcard,
    Bind,
    Literal,
    Variant,
    Record,
    Tuple,
    Array,
    Or,
}

#[derive(Debug, Clone, Copy)]
pub struct Pat<'tree> {
    syntax: SyntaxNode<'tree>,
}

impl<'tree> Pat<'tree> {
    #[must_use]
    pub fn cast(node: SyntaxNode<'tree>) -> Option<Self> {
        node.kind().is_pat().then_some(Self { syntax: node })
    }

    #[must_use]
    pub const fn syntax(self) -> SyntaxNode<'tree> {
        self.syntax
    }

    /// Classify this pattern node.
    ///
    /// # Panics
    ///
    /// Panics if the wrapper does not actually hold a pattern node.
    /// This indicates a broken internal invariant after a bad cast.
    #[must_use]
    pub fn kind(self) -> PatKindView {
        PatKindView::from_syntax(self.syntax.kind()).expect("Pat wrapper always holds pattern node")
    }
}

impl PatKindView {
    #[must_use]
    pub const fn from_syntax(kind: SyntaxNodeKind) -> Option<Self> {
        match kind {
            SyntaxNodeKind::WildcardPat => Some(Self::Wildcard),
            SyntaxNodeKind::BindPat => Some(Self::Bind),
            SyntaxNodeKind::LiteralPat => Some(Self::Literal),
            SyntaxNodeKind::VariantPat => Some(Self::Variant),
            SyntaxNodeKind::RecordPat => Some(Self::Record),
            SyntaxNodeKind::TuplePat => Some(Self::Tuple),
            SyntaxNodeKind::ArrayPat => Some(Self::Array),
            SyntaxNodeKind::OrPat => Some(Self::Or),
            _ => None,
        }
    }
}
