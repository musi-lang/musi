use std::{error::Error, fmt};

use crate::{ModuleKey, ModuleSpecifier};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportErrorKind {
    NotFound,
    InvalidSpecifier,
}

impl fmt::Display for ImportErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::NotFound => "module not found",
            Self::InvalidSpecifier => "invalid module specifier",
        })
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportError {
    pub kind: ImportErrorKind,
    message: Box<str>,
}

impl ImportError {
    #[must_use]
    pub fn new(kind: ImportErrorKind, message: impl Into<Box<str>>) -> Self {
        Self {
            kind,
            message: message.into(),
        }
    }

    #[must_use]
    pub fn message(&self) -> &str {
        &self.message
    }

    #[must_use]
    pub const fn kind(&self) -> ImportErrorKind {
        self.kind
    }
}

impl fmt::Display for ImportError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}: {}", self.kind, self.message())
    }
}

impl Error for ImportError {}

pub type ImportResolveResult = Result<ModuleKey, ImportError>;

pub trait ImportEnv {
    /// Resolve `spec` as imported from `from`.
    ///
    /// # Errors
    ///
    /// Returns `ImportError` when the specifier is invalid or the environment cannot resolve it.
    fn resolve(&self, from: &ModuleKey, spec: &ModuleSpecifier) -> ImportResolveResult;
}

#[cfg(test)]
mod tests {
    use super::{ImportError, ImportErrorKind};

    #[test]
    fn import_error_kind_display_is_stable() {
        assert_eq!(ImportErrorKind::NotFound.to_string(), "module not found");
        assert_eq!(
            ImportErrorKind::InvalidSpecifier.to_string(),
            "invalid module specifier"
        );
    }

    #[test]
    fn import_error_display_includes_kind_and_message() {
        let err = ImportError::new(ImportErrorKind::NotFound, "dep/math");
        assert_eq!(err.kind(), ImportErrorKind::NotFound);
        assert_eq!(err.message(), "dep/math");
        assert_eq!(err.to_string(), "module not found: dep/math");
    }
}
