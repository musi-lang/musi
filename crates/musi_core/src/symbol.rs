use std::hash;

use crate::span::Span;

/// Interned symbol with source location.
#[derive(Debug, Clone, Copy, Eq)]
pub struct Symbol {
    /// Interned string ID.
    pub id: u32,
    /// Source location.
    pub span: Span,
}

impl Symbol {
    /// Creates new symbol.
    #[must_use]
    pub const fn new(id: u32, span: Span) -> Self {
        Self { id, span }
    }
}

impl PartialEq for Symbol {
    fn eq(&self, other: &Self) -> bool {
        self.id == other.id
    }
}

impl hash::Hash for Symbol {
    fn hash<H: hash::Hasher>(&self, state: &mut H) {
        self.id.hash(state);
    }
}
