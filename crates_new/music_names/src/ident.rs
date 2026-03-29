use music_basic::Span;

use crate::Symbol;

/// A named identifier with its source location.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Ident {
    pub name: Symbol,
    pub span: Span,
}

impl Ident {
    /// Create an identifier at a specific span.
    #[must_use]
    pub const fn new(name: Symbol, span: Span) -> Self {
        Self { name, span }
    }

    /// Create an identifier at `Span::DUMMY` for compiler-generated nodes.
    #[must_use]
    pub const fn dummy(name: Symbol) -> Self {
        Self {
            name,
            span: Span::DUMMY,
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
