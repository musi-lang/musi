use thiserror::Error;

use crate::{ModuleExportSummary, ModuleKey, ModuleSpecifier};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ImportErrorKind {
    NotFound,
    InvalidSpecifier,
}

#[derive(Debug, Error, Clone, PartialEq, Eq)]
#[error("{kind:?}: {message}")]
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
}

pub trait ImportEnv {
    /// Resolve `spec` as imported from `from`.
    ///
    /// # Errors
    ///
    /// Returns `ImportError` when the specifier is invalid or the environment cannot resolve it.
    fn resolve(&self, from: &ModuleKey, spec: &ModuleSpecifier) -> Result<ModuleKey, ImportError>;

    /// Returns a cached export summary for a resolved module key, if available.
    fn module_summary(&self, key: &ModuleKey) -> Option<&ModuleExportSummary>;
}
