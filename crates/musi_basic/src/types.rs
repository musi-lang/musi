use std::hash;

use crate::span::Span;

/// Identifier with source location.
#[derive(Debug, Clone, Copy, Eq)]
pub struct Ident {
    /// Interned string ID.
    pub id: u32,
    /// Source location.
    pub span: Span,
}

impl Ident {
    /// Creates new identifier.
    #[must_use]
    pub const fn new(id: u32, span: Span) -> Self {
        Self { id, span }
    }
}

impl PartialEq for Ident {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl hash::Hash for Ident {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
