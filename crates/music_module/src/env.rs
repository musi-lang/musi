use std::{error::Error, fmt};

use crate::{ModuleKey, ModuleSpecifier};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportErrorKind {
    ModuleNotFound,
    SpecifierInvalid,
}

impl fmt::Display for ImportErrorKind {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.write_str(match self {
            Self::ModuleNotFound => "module not found",
            Self::SpecifierInvalid => "module specifier invalid",
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
    /// Returns `ImportError` when the specifier is invalid or resolution fails.
    fn resolve(&self, from: &ModuleKey, spec: &ModuleSpecifier) -> ImportResolveResult;
}

#[cfg(test)]
mod tests;
