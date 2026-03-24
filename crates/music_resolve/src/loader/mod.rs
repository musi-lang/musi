use std::collections::HashMap;
use std::path::{Path, PathBuf};

use music_found::Symbol;

use crate::def::DefId;

/// Tracks whether a module is currently being loaded (cycle detection)
/// or has already been fully resolved.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleState {
    /// The module file has been entered but not yet finished resolving.
    Loading,
    /// The module has been fully resolved and its exports are available.
    Loaded(ModuleExports),
}

/// The public names exported by a resolved module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleExports {
    pub exports: HashMap<Symbol, DefId>,
}

/// Resolves import paths to filesystem paths and caches module state.
pub struct ModuleLoader {
    root: PathBuf,
    std_path: Option<PathBuf>,
    cache: HashMap<PathBuf, ModuleState>,
    config_imports: HashMap<String, String>,
}

impl ModuleLoader {
    /// Create a new loader rooted at the given project directory.
    /// If a `std/` subdirectory exists under `root`, it is used as the
    /// standard library path.
    #[must_use]
    pub fn new(root: PathBuf) -> Self {
        let std_candidate = root.join("std");
        let std_path = if std_candidate.exists() {
            Some(std_candidate)
        } else {
            None
        };
        Self {
            root,
            std_path,
            cache: HashMap::new(),
            config_imports: HashMap::new(),
        }
    }

    /// Set custom import mappings (e.g. from `musi.json`).
    #[must_use]
    pub fn with_config_imports(mut self, imports: HashMap<String, String>) -> Self {
        self.config_imports = imports;
        self
    }

    /// Override the standard library search path.
    #[must_use]
    pub fn with_std_path(mut self, path: PathBuf) -> Self {
        self.std_path = Some(path);
        self
    }

    /// Resolve an import path string to an absolute filesystem path.
    ///
    /// Resolution order:
    /// 1. `./` or `../` -- relative to the directory of `current_file`
    /// 2. `@std/` -- standard library lookup
    /// 3. Config-mapped imports
    /// 4. Relative to project root
    ///
    /// For each candidate, tries `.ms` extension, then `mod.ms` and
    /// `index.ms` inside a directory of that name.
    #[must_use]
    pub fn resolve_path(&self, import_path: &str, current_file: &Path) -> Option<PathBuf> {
        if import_path.starts_with("./") || import_path.starts_with("../") {
            Self::resolve_relative(import_path, current_file)
        } else if import_path.starts_with("@std/") {
            Self::resolve_std(import_path, self.std_path.as_deref())
        } else if let Some(mapped) = self.config_imports.get(import_path) {
            let mapped_owned = mapped.clone();
            self.resolve_path(&mapped_owned, current_file)
        } else {
            Self::resolve_from_root(import_path, &self.root)
        }
    }

    /// Check whether a module is currently in the `Loading` state (cycle).
    #[must_use]
    pub fn is_loading(&self, path: &Path) -> bool {
        matches!(self.cache.get(path), Some(ModuleState::Loading))
    }

    /// Return cached exports for a fully loaded module, if available.
    #[must_use]
    pub fn get_cached(&self, path: &Path) -> Option<&ModuleExports> {
        match self.cache.get(path) {
            Some(ModuleState::Loaded(exports)) => Some(exports),
            _ => None,
        }
    }

    /// Mark a module as currently being loaded (for cycle detection).
    pub fn mark_loading(&mut self, path: PathBuf) {
        let _prev = self.cache.insert(path, ModuleState::Loading);
    }

    /// Mark a module as fully loaded with its resolved exports.
    pub fn mark_loaded(&mut self, path: PathBuf, exports: ModuleExports) {
        let _prev = self.cache.insert(path, ModuleState::Loaded(exports));
    }

    fn resolve_relative(import_path: &str, current_file: &Path) -> Option<PathBuf> {
        let dir = current_file.parent()?;
        let candidate = dir.join(import_path);
        Self::try_file_candidates(&candidate)
    }

    fn resolve_std(import_path: &str, std_path: Option<&Path>) -> Option<PathBuf> {
        let std_path = std_path?;
        let lib_name = import_path.strip_prefix("@std/")?;
        let candidate = std_path.join(lib_name);
        Self::try_file_candidates(&candidate)
    }

    fn resolve_from_root(import_path: &str, root: &Path) -> Option<PathBuf> {
        let candidate = root.join(import_path);
        Self::try_file_candidates(&candidate)
    }

    /// Try a candidate path with `.ms` extension, then as a directory
    /// containing `mod.ms` or `index.ms`. Returns a canonicalized path
    /// so that `../` segments are resolved.
    fn try_file_candidates(candidate: &Path) -> Option<PathBuf> {
        // If candidate already has an extension and exists, use it directly
        if candidate.extension().is_some() && candidate.exists() {
            return candidate.canonicalize().ok();
        }

        // Try adding .ms extension
        let with_ext = candidate.with_extension("ms");
        if with_ext.exists() {
            return with_ext.canonicalize().ok();
        }

        // Try as directory with mod.ms
        let mod_file = candidate.join("mod.ms");
        if mod_file.exists() {
            return mod_file.canonicalize().ok();
        }

        // Try as directory with index.ms
        let index_file = candidate.join("index.ms");
        if index_file.exists() {
            return index_file.canonicalize().ok();
        }

        None
    }
}

impl Default for ModuleLoader {
    fn default() -> Self {
        Self::new(PathBuf::from("."))
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
