pub mod git;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

/// Result of resolving an import specifier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedImport {
    /// Local filesystem module.
    File(PathBuf),
    /// `musi:` prefix --compiler builtins, no file to load.
    Builtin,
    /// `git:` prefix --resolved to a cached local checkout.
    Git(PathBuf),
    /// `msr:` prefix --recognized but not yet available.
    ReservedRegistry,
}

/// Resolves import path strings to filesystem paths or scheme markers.
///
/// Handles URI scheme routing (`musi:`, `git:`, `msr:`) and local path
/// resolution (relative, config-mapped, root-relative). Module state
/// tracking is handled by `ModuleGraph`, not the loader.
pub struct ModuleLoader {
    root: PathBuf,
    config_imports: HashMap<String, String>,
    git_cache_dir: PathBuf,
}

impl ModuleLoader {
    /// Create a new loader rooted at the given project directory.
    #[must_use]
    pub fn new(root: PathBuf) -> Self {
        let git_cache_dir = musi_home().join("cache/git");
        Self {
            root,
            config_imports: HashMap::new(),
            git_cache_dir,
        }
    }

    /// Set custom import mappings (e.g. from `musi.json` `imports` field).
    #[must_use]
    pub fn with_config_imports(mut self, imports: HashMap<String, String>) -> Self {
        self.config_imports = imports;
        self
    }

    /// Override the git cache directory (for testing).
    #[must_use]
    pub fn with_git_cache_dir(mut self, dir: PathBuf) -> Self {
        self.git_cache_dir = dir;
        self
    }

    /// Resolve an import specifier to a `ResolvedImport`.
    ///
    /// Resolution order:
    /// 1. `musi:` prefix → `Builtin`
    /// 2. `git:` prefix → `Git(cached_path)` via git clone/cache
    /// 3. `msr:` prefix → `ReservedRegistry`
    /// 4. `./` or `../` → relative to `current_file`
    /// 5. Config-mapped imports (from `musi.json`)
    /// 6. Relative to project root
    #[must_use]
    pub fn resolve(&self, import_path: &str, current_file: &Path) -> Option<ResolvedImport> {
        if let Some(_rest) = import_path.strip_prefix("musi:") {
            return Some(ResolvedImport::Builtin);
        }
        if let Some(specifier) = import_path.strip_prefix("git:") {
            return self.resolve_git(specifier);
        }
        if import_path.starts_with("msr:") {
            return Some(ResolvedImport::ReservedRegistry);
        }

        // Config-mapped imports: the mapped value may itself be a scheme URI
        if let Some(mapped) = self.config_imports.get(import_path) {
            let mapped_owned = mapped.clone();
            return self.resolve(&mapped_owned, current_file);
        }

        if import_path.starts_with("./") || import_path.starts_with("../") {
            Self::resolve_relative(import_path, current_file).map(ResolvedImport::File)
        } else {
            Self::resolve_from_root(import_path, &self.root).map(ResolvedImport::File)
        }
    }

    /// Resolve a `git:` specifier to a cached local checkout.
    fn resolve_git(&self, specifier: &str) -> Option<ResolvedImport> {
        let parsed = git::GitSpecifier::parse(specifier)?;
        let cached = parsed.cache_path(&self.git_cache_dir);

        if !cached.exists() {
            git::clone_repo(&parsed, &cached).ok()?;
        }

        let entry = git::find_entry_point(&cached)?;
        Some(ResolvedImport::Git(entry))
    }

    /// The project root this loader is anchored to.
    #[must_use]
    pub fn root(&self) -> &Path {
        &self.root
    }

    /// The git cache directory.
    #[must_use]
    pub fn git_cache_dir(&self) -> &Path {
        &self.git_cache_dir
    }

    fn resolve_relative(import_path: &str, current_file: &Path) -> Option<PathBuf> {
        let dir = current_file.parent()?;
        let candidate = dir.join(import_path);
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
        if candidate.extension().is_some() && candidate.exists() {
            return candidate.canonicalize().ok();
        }

        let with_ext = candidate.with_extension("ms");
        if with_ext.exists() {
            return with_ext.canonicalize().ok();
        }

        let mod_file = candidate.join("mod.ms");
        if mod_file.exists() {
            return mod_file.canonicalize().ok();
        }

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

fn musi_home() -> PathBuf {
    dirs::home_dir()
        .unwrap_or_else(|| PathBuf::from("."))
        .join(".musi")
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
