use std::fmt;

use music_found::{Span, Symbol};

/// A name-resolution error with its source location.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ResolveError {
    pub kind: ResolveErrorKind,
    pub span: Span,
}

/// The specific kind of name-resolution failure.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolveErrorKind {
    /// A value-level name was used but never defined.
    UndefinedName(Symbol),
    /// A type-level name was used but never defined.
    UndefinedType(Symbol),
    /// A variant tag was used but never defined.
    UndefinedVariant(Symbol),
    /// A name was defined twice in the same scope.
    DuplicateDefinition { name: Symbol, original: Span },
    /// A name was referenced before its definition in sequential scope.
    NotYetDefined(Symbol),
}

impl ResolveError {
    #[must_use]
    pub const fn span(&self) -> Span {
        self.span
    }
}

impl fmt::Display for ResolveError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match &self.kind {
            ResolveErrorKind::UndefinedName(sym) => {
                write!(f, "undefined name `{sym}`")
            }
            ResolveErrorKind::UndefinedType(sym) => {
                write!(f, "undefined type `{sym}`")
            }
            ResolveErrorKind::UndefinedVariant(sym) => {
                write!(f, "undefined variant `{sym}`")
            }
            ResolveErrorKind::DuplicateDefinition { name, original } => {
                write!(
                    f,
                    "duplicate definition `{name}` (originally defined at {original})"
                )
            }
            ResolveErrorKind::NotYetDefined(sym) => {
                write!(f, "name `{sym}` used before its definition")
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
