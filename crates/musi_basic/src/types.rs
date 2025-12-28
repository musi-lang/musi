use std::hash;

use crate::diagnostic::Diagnostic;
use crate::span::Span;

pub type Diagnostics = Vec<Diagnostic>;

#[derive(Debug, Clone, Copy, Eq)]
pub struct Ident {
    pub id: u32,
    pub span: Span,
}

impl Ident {
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
