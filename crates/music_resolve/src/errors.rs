use std::fmt;

use music_shared::diag::{Diag, DiagCode};
use music_shared::{Interner, SourceId, Span, Symbol, SymbolList};

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

    #[must_use]
    pub fn diagnostic(&self, interner: &Interner, source_id: SourceId) -> Diag {
        let (code, message, hint): (DiagCode, String, Option<String>) = match &self.kind {
            ResolveErrorKind::UndefinedName(sym) => (
                DiagCode::new(3001),
                format!("undefined binding '{}'", interner.resolve(*sym)),
                None,
            ),
            ResolveErrorKind::UndefinedType(sym) => (
                DiagCode::new(3002),
                format!("undefined type '{}'", interner.resolve(*sym)),
                None,
            ),
            ResolveErrorKind::UndefinedVariant(sym) => (
                DiagCode::new(3003),
                format!("undefined variant '{}'", interner.resolve(*sym)),
                None,
            ),
            ResolveErrorKind::DuplicateDefinition { name, .. } => (
                DiagCode::new(3004),
                format!("duplicate definition '{}'", interner.resolve(*name)),
                Some(String::from("remove or rename one of the definitions")),
            ),
            ResolveErrorKind::NotYetDefined(sym) => (
                DiagCode::new(3005),
                format!(
                    "binding '{}' is used before its definition",
                    interner.resolve(*sym)
                ),
                None,
            ),
            ResolveErrorKind::ImportNotFound(sym) => (
                DiagCode::new(3006),
                format!("import '{}' was not found", interner.resolve(*sym)),
                None,
            ),
            ResolveErrorKind::CyclicImport(sym) => (
                DiagCode::new(3007),
                format!("cyclic import detected for '{}'", interner.resolve(*sym)),
                None,
            ),
            ResolveErrorKind::CyclicImportChain { path, chain } => (
                DiagCode::new(3008),
                format!("cyclic import detected for '{}'", interner.resolve(*path)),
                Some(format!(
                    "cycle: {}",
                    chain
                        .iter()
                        .map(|module| interner.resolve(*module))
                        .collect::<Vec<_>>()
                        .join(" -> ")
                )),
            ),
            ResolveErrorKind::RegistryNotAvailable(sym) => (
                DiagCode::new(3009),
                format!(
                    "registry import '{}' is not available",
                    interner.resolve(*sym)
                ),
                Some(String::from("use a local path or git import instead")),
            ),
            ResolveErrorKind::GitResolutionFailed { path, reason } => (
                DiagCode::new(3010),
                format!("failed to resolve git import '{}'", interner.resolve(*path)),
                Some(reason.clone()),
            ),
        };

        let mut diag = Diag::error(message)
            .with_code(code)
            .with_label(self.span, source_id, "");
        if let Some(hint) = hint {
            diag = diag.with_hint(hint);
        }
        diag
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
                write!(f, "cyclic import for `{path}`; ")?;
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
                write!(f, "failed to resolve git import `{path}`; {reason}")
            }
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
