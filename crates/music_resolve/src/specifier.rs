//! Import specifier parsing.
//!
//! Parses raw import strings like `"musi:core"`, `"git:github.com/user/repo@v1.0"`,
//! `"./foo"`, etc. into structured [`ImportSpecifier`] values.

#[cfg(test)]
mod tests;

use crate::error::ResolveError;

/// The URI scheme of an import specifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ImportScheme {
    /// `musi:core`, `musi:io/print`
    Musi,
    /// `git:github.com/user/repo[@tag]`
    Git,
    /// `msr:package-name` (reserved, always errors)
    Msr,
    /// `./foo`, `../bar`
    Relative,
    /// `foo` (resolved via manifest `[imports]` or `[dependencies]`)
    Bare,
}

/// A parsed import specifier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ImportSpecifier {
    /// The URI scheme.
    pub scheme: ImportScheme,
    /// The full original string.
    pub raw: Box<str>,
    /// The scheme-specific path (after `musi:`, after `git:`, the full relative
    /// path, or the bare name).
    pub module_path: Box<str>,
}

/// A parsed git source reference.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitSource {
    /// The repository URL (without the `git:` prefix and `@tag` suffix).
    pub url: Box<str>,
    /// An optional tag or branch (e.g. `v1.0`, `main`).
    pub tag: Option<Box<str>>,
}

/// Parses a raw import string into an [`ImportSpecifier`].
///
/// # Errors
///
/// Returns [`ResolveError::EmptyPath`] for empty strings, and
/// [`ResolveError::RegistryNotSupported`] for `msr:` specifiers.
pub fn parse_specifier(raw: &str) -> Result<ImportSpecifier, ResolveError> {
    if raw.is_empty() {
        return Err(ResolveError::EmptyPath);
    }

    if let Some(rest) = raw.strip_prefix("musi:") {
        if rest.is_empty() {
            return Err(ResolveError::ModuleNotFound {
                path: Box::from(raw),
            });
        }
        return Ok(ImportSpecifier {
            scheme: ImportScheme::Musi,
            raw: Box::from(raw),
            module_path: Box::from(rest),
        });
    }

    if let Some(rest) = raw.strip_prefix("git:") {
        if rest.is_empty() {
            return Err(ResolveError::InvalidGitSpecifier {
                raw: Box::from(raw),
            });
        }
        return Ok(ImportSpecifier {
            scheme: ImportScheme::Git,
            raw: Box::from(raw),
            module_path: Box::from(rest),
        });
    }

    if let Some(rest) = raw.strip_prefix("msr:") {
        return Err(ResolveError::RegistryNotSupported {
            specifier: Box::from(rest),
        });
    }

    if raw.starts_with("./") || raw.starts_with("../") {
        return Ok(ImportSpecifier {
            scheme: ImportScheme::Relative,
            raw: Box::from(raw),
            module_path: Box::from(raw),
        });
    }

    Ok(ImportSpecifier {
        scheme: ImportScheme::Bare,
        raw: Box::from(raw),
        module_path: Box::from(raw),
    })
}

/// Parses a git module path (the part after `git:`) into a [`GitSource`].
///
/// The tag is separated by the last `@` character: `github.com/user/repo@v1.0`
/// yields url=`github.com/user/repo` and tag=`Some("v1.0")`.
///
/// # Errors
///
/// Returns [`ResolveError::InvalidGitSpecifier`] when the module path is empty
/// or when a tag separator `@` is present but the tag is empty.
pub fn parse_git_source(module_path: &str) -> Result<GitSource, ResolveError> {
    if module_path.is_empty() {
        return Err(ResolveError::InvalidGitSpecifier {
            raw: Box::from(module_path),
        });
    }

    if let Some(at_pos) = module_path.rfind('@') {
        let url = module_path.get(..at_pos).unwrap_or_default();
        let tag = module_path.get(at_pos + 1..).unwrap_or_default();

        if url.is_empty() || tag.is_empty() {
            return Err(ResolveError::InvalidGitSpecifier {
                raw: Box::from(module_path),
            });
        }

        Ok(GitSource {
            url: Box::from(url),
            tag: Some(Box::from(tag)),
        })
    } else {
        Ok(GitSource {
            url: Box::from(module_path),
            tag: None,
        })
    }
}
