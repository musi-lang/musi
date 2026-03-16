//! Structured module resolution errors.

use std::io;

use music_shared::{IntoDiagnostic, Severity};

#[derive(Debug, thiserror::Error)]
pub enum ResolveError {
    #[error("empty import path")]
    EmptyPath,
    #[error(
        "registry imports not yet supported: `{specifier}` (registry support is planned — see https://msr.musi-lang.org)"
    )]
    RegistryNotSupported { specifier: Box<str> },
    #[error("invalid git specifier `{raw}`")]
    InvalidGitSpecifier { raw: Box<str> },
    #[error("module not found `{path}`")]
    ModuleNotFound { path: Box<str> },
    #[error("circular import `{cycle}`")]
    CircularImport { cycle: Box<str> },
    #[error("bare import `{name}` not found in manifest")]
    UnresolvedBareImport { name: Box<str> },
    #[error("git fetch failed for `{url}`, {reason}")]
    GitFetchFailed { url: Box<str>, reason: Box<str> },
    #[error("failed to read `{path}`, {source}")]
    Io { path: Box<str>, source: io::Error },
}

impl IntoDiagnostic for ResolveError {
    fn severity(&self) -> Severity {
        Severity::Error
    }
}
