use std::fmt;

use music_shared::{Span, Symbol, SymbolList};

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
    /// An import path could not be resolved to a file.
    ImportNotFound(Symbol),
    /// An import forms a cycle (module A imports B which imports A).
    CyclicImport(Symbol),
    /// A cyclic import with the full chain of module paths.
    CyclicImportChain { path: Symbol, chain: SymbolList },
    /// The `msr:` registry prefix was used but is not yet available.
    RegistryNotAvailable(Symbol),
    /// A `git:` import failed to resolve.
    GitResolutionFailed { path: Symbol, reason: String },
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
            ResolveErrorKind::ImportNotFound(sym) => {
                write!(f, "import `{sym}` not found")
            }
            ResolveErrorKind::CyclicImport(sym) => {
                write!(f, "cyclic import detected for `{sym}`")
            }
            ResolveErrorKind::CyclicImportChain { path, chain } => {
                write!(f, "cyclic import for `{path}`: ")?;
                for (i, module) in chain.iter().enumerate() {
                    if i > 0 {
                        write!(f, " -> ")?;
                    }
                    write!(f, "{module}")?;
                }
                Ok(())
            }
            ResolveErrorKind::RegistryNotAvailable(sym) => {
                write!(
                    f,
                    "`msr:` registry imports are not yet available (used in `{sym}`)"
                )
            }
            ResolveErrorKind::GitResolutionFailed { path, reason } => {
                write!(f, "failed to resolve git import `{path}`: {reason}")
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
