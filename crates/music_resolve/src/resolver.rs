//! Path resolution: maps [`ImportSpecifier`] values to filesystem paths.

#[cfg(test)]
mod tests;

use std::collections::HashMap;
use std::env;
use std::path::{self, Path, PathBuf};

use crate::error::ResolveError;
use crate::git::fetch_git_package;
use crate::specifier::{ImportScheme, ImportSpecifier, parse_git_source, parse_specifier};

/// Configuration for import resolution.
pub struct ResolverConfig {
    /// Path to the `std/` directory (for `musi:` imports).
    pub std_root: PathBuf,
    /// Cache directory for fetched packages (`~/.cache/musi/packages/`).
    pub cache_dir: PathBuf,
    /// The project root directory (containing `mspackage.toml`).
    pub project_root: PathBuf,
    /// The `[imports]` table from `mspackage.toml`.
    pub manifest_imports: HashMap<String, String>,
    /// The `[dependencies]` table from `mspackage.toml`.
    pub manifest_deps: HashMap<String, String>,
}

/// Discovers the standard library root.
///
/// Searches in order:
/// 1. `MUSI_STD_ROOT` environment variable
/// 2. Relative to the current executable (`../std/`)
/// 3. Sibling of `project_root` (i.e. `project_root/std/`)
///
/// # Errors
///
/// Returns [`ResolveError::ModuleNotFound`] if no valid std root is found.
pub fn discover_std_root(project_root: &Path) -> Result<PathBuf, ResolveError> {
    if let Ok(env_root) = env::var("MUSI_STD_ROOT") {
        let p = PathBuf::from(env_root);
        if p.is_dir() {
            return Ok(p);
        }
    }

    if let Ok(exe) = env::current_exe()
        && let Some(parent) = exe.parent()
    {
        let candidate = parent.join("../std");
        if candidate.is_dir() {
            return Ok(candidate);
        }
    }

    let sibling = project_root.join("std");
    if sibling.is_dir() {
        return Ok(sibling);
    }

    Err(ResolveError::ModuleNotFound {
        path: Box::from("std/ (standard library root)"),
    })
}

/// Resolves an import specifier to a filesystem path.
///
/// # Errors
///
/// Returns a [`ResolveError`] if the module cannot be found, the scheme is
/// unsupported, or a bare import is not defined in the manifest.
pub fn resolve_import(
    specifier: &ImportSpecifier,
    importing_file: &Path,
    config: &ResolverConfig,
) -> Result<PathBuf, ResolveError> {
    match specifier.scheme {
        ImportScheme::Musi => resolve_musi(&specifier.module_path, config),
        ImportScheme::Relative => resolve_relative(&specifier.module_path, importing_file),
        ImportScheme::Bare => resolve_bare(&specifier.module_path, importing_file, config),
        ImportScheme::Git => resolve_git(&specifier.module_path, config),
        ImportScheme::Msr => Err(ResolveError::RegistryNotSupported {
            specifier: specifier.raw.clone(),
        }),
    }
}

fn resolve_musi(module_path: &str, config: &ResolverConfig) -> Result<PathBuf, ResolveError> {
    let segments = module_path.replace('/', path::MAIN_SEPARATOR_STR);

    let as_file = config.std_root.join(format!("{segments}.ms"));
    if as_file.is_file() {
        return Ok(as_file);
    }

    let as_dir_mod = config.std_root.join(&segments).join("index.ms");
    if as_dir_mod.is_file() {
        return Ok(as_dir_mod);
    }

    Err(ResolveError::ModuleNotFound {
        path: Box::from(format!("musi:{module_path}")),
    })
}

fn resolve_relative(module_path: &str, importing_file: &Path) -> Result<PathBuf, ResolveError> {
    let base_dir = importing_file.parent().unwrap_or_else(|| Path::new("."));

    let candidate = base_dir.join(module_path);

    if candidate.extension().is_some() {
        if candidate.is_file() {
            return Ok(candidate);
        }
        return Err(ResolveError::ModuleNotFound {
            path: Box::from(module_path),
        });
    }

    let with_ext = candidate.with_extension("ms");
    if with_ext.is_file() {
        return Ok(with_ext);
    }

    let as_dir_mod = candidate.join("index.ms");
    if as_dir_mod.is_file() {
        return Ok(as_dir_mod);
    }

    Err(ResolveError::ModuleNotFound {
        path: Box::from(module_path),
    })
}

fn resolve_bare(
    name: &str,
    importing_file: &Path,
    config: &ResolverConfig,
) -> Result<PathBuf, ResolveError> {
    if let Some(target) = config.manifest_imports.get(name) {
        let inner = parse_specifier(target)?;
        return resolve_import(&inner, importing_file, config);
    }
    if let Some(target) = config.manifest_deps.get(name) {
        let inner = parse_specifier(target)?;
        return resolve_import(&inner, importing_file, config);
    }
    Err(ResolveError::UnresolvedBareImport {
        name: Box::from(name),
    })
}

fn resolve_git(module_path: &str, config: &ResolverConfig) -> Result<PathBuf, ResolveError> {
    let source = parse_git_source(module_path)?;
    let result = fetch_git_package(&source, &config.cache_dir)?;
    Ok(result.entry_point)
}
