pub mod git;

use std::collections::HashMap;
use std::path::{Path, PathBuf};

use music_builtins::modules;
use music_config::{Exports, load_config};

/// Result of resolving an import specifier.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResolvedImport {
    /// Local filesystem module.
    File(PathBuf),
    /// `git:` prefix --resolved to a cached local checkout.
    Git(PathBuf),
    /// `msr:` prefix --recognized but not yet available.
    ReservedRegistry,
}

/// Resolves import path strings to filesystem paths or scheme markers.
///
/// Handles URI scheme routing (`musi:`, `git:`, `msr:`) and local path
/// resolution (relative, config-mapped, root-relative). `musi:` points at
/// compiler-owned intrinsic modules, while package stdlib imports belong under
/// config-mapped names such as `@std`. Module state tracking is handled by
/// `ModuleGraph`, not the loader.
#[derive(Debug, Clone)]
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
    /// 1. `musi:` prefix → intrinsic module source
    /// 2. `git:` prefix → `Git(cached_path)` via git clone/cache
    /// 3. `msr:` prefix → `ReservedRegistry`
    /// 4. `./` or `../` → relative to `current_file`
    /// 5. Config-mapped imports (from `musi.json`)
    /// 6. Relative to project root
    #[must_use]
    pub fn resolve(&self, import_path: &str, current_file: &Path) -> Option<ResolvedImport> {
        if import_path.starts_with("musi:") {
            return modules::resolve_module(import_path)
                .and_then(|path| path.canonicalize().ok())
                .map(ResolvedImport::File);
        }
        if let Some(specifier) = import_path.strip_prefix("git:") {
            return self.resolve_git(specifier);
        }
        if import_path.starts_with("msr:") {
            return Some(ResolvedImport::ReservedRegistry);
        }

        if let Some((mapped, suffix)) = self.match_config_import(import_path) {
            return self.resolve_mapped_target(mapped, suffix, current_file);
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

    fn resolve_mapped_target(
        &self,
        mapped: &str,
        suffix: &str,
        current_file: &Path,
    ) -> Option<ResolvedImport> {
        if let Some(specifier) = mapped.strip_prefix("git:") {
            return self.resolve_git_with_suffix(specifier, suffix);
        }

        let suffix = suffix.trim_start_matches('/');
        if mapped.starts_with("./") || mapped.starts_with("../") {
            let base = self.root.join(mapped);
            return Self::resolve_candidate_with_suffix(&base, suffix).map(ResolvedImport::File);
        }

        if mapped.starts_with('/') {
            let base = PathBuf::from(mapped);
            return Self::resolve_candidate_with_suffix(&base, suffix).map(ResolvedImport::File);
        }

        if suffix.is_empty() {
            return self.resolve(mapped, current_file);
        }

        self.resolve(&format!("{mapped}/{suffix}"), current_file)
    }

    fn resolve_git_with_suffix(&self, specifier: &str, suffix: &str) -> Option<ResolvedImport> {
        let parsed = git::GitSpecifier::parse(specifier)?;
        let cached = parsed.cache_path(&self.git_cache_dir);

        if !cached.exists() {
            git::clone_repo(&parsed, &cached).ok()?;
        }

        let suffix = suffix.trim_start_matches('/');
        if suffix.is_empty() {
            return git::find_entry_point(&cached).map(ResolvedImport::Git);
        }

        Self::resolve_candidate_with_suffix(&cached, suffix).map(ResolvedImport::Git)
    }

    fn resolve_candidate_with_suffix(base: &Path, suffix: &str) -> Option<PathBuf> {
        if suffix.is_empty() {
            return Self::try_file_candidates(base);
        }

        let suffix = suffix.trim_start_matches('/');
        Self::resolve_package_subpath(base, suffix)
    }

    fn match_config_import<'a>(&'a self, import_path: &'a str) -> Option<(&'a str, &'a str)> {
        let mut best: Option<(&'a str, &'a str)> = None;

        for (key, mapped) in &self.config_imports {
            if import_path == key {
                let mapped = mapped.as_str();
                let score = key.len();
                if best.is_none_or(|(best_key, _)| score > best_key.len()) {
                    best = Some((key.as_str(), mapped));
                }
                continue;
            }

            if let Some(prefix) = key.strip_suffix('/') {
                if let Some(rest) = import_path.strip_prefix(key) {
                    let score = prefix.len();
                    if best.is_none_or(|(best_key, _)| score > best_key.len()) {
                        best = Some((key.as_str(), mapped.as_str()));
                    }
                    let _ = rest;
                }
                continue;
            }

            let prefix = format!("{key}/");
            if import_path.starts_with(&prefix) {
                let score = key.len();
                if best.is_none_or(|(best_key, _)| score > best_key.len()) {
                    best = Some((key.as_str(), mapped.as_str()));
                }
            }
        }

        best.map(|(key, mapped)| {
            let suffix = import_path.strip_prefix(key).unwrap_or_default();
            (mapped, suffix)
        })
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
    /// containing the manifest-selected package entry or `index.ms`.
    /// Returns a canonicalized path
    /// so that `../` segments are resolved.
    fn try_file_candidates(candidate: &Path) -> Option<PathBuf> {
        if candidate.extension().is_some() && candidate.exists() {
            if Self::is_explicit_package_entry(candidate) {
                return None;
            }
            return candidate.canonicalize().ok();
        }

        let with_ext = candidate.with_extension("ms");
        if with_ext.exists() {
            if Self::is_explicit_package_entry(&with_ext) {
                return None;
            }
            return with_ext.canonicalize().ok();
        }

        if candidate.is_dir() {
            return Self::resolve_package_root(candidate);
        }

        if let (Some(parent), Some(name)) = (candidate.parent(), candidate.file_name()) {
            if Self::load_package_config(parent).is_some() {
                let suffix = name.to_str()?;
                if let Some(path) = Self::resolve_package_subpath(parent, suffix) {
                    return Some(path);
                }
            }
        }

        None
    }

    fn resolve_package_root(package_root: &Path) -> Option<PathBuf> {
        if !package_root.is_dir() {
            return None;
        }

        if let Some(entry) = Self::resolve_manifest_root(package_root) {
            return entry.canonicalize().ok();
        }

        let index_file = package_root.join("index.ms");
        if index_file.exists() {
            return index_file.canonicalize().ok();
        }

        None
    }

    fn resolve_package_subpath(package_root: &Path, suffix: &str) -> Option<PathBuf> {
        if !package_root.is_dir() {
            return None;
        }

        if let Some(entry) = Self::resolve_manifest_export(package_root, suffix) {
            return entry.canonicalize().ok();
        }

        let candidate = package_root.join(suffix);
        Self::try_file_candidates(&candidate)
    }

    fn resolve_manifest_root(package_root: &Path) -> Option<PathBuf> {
        let config = Self::load_package_config(package_root)?;
        let main = config.main?;
        Self::resolve_manifest_target(package_root, &main)
    }

    fn resolve_manifest_export(package_root: &Path, suffix: &str) -> Option<PathBuf> {
        let config = Self::load_package_config(package_root)?;
        let exports = config.exports?;
        let key = format!("./{suffix}");

        match exports {
            Exports::Path(path) if suffix.is_empty() => {
                Self::resolve_manifest_target(package_root, &path)
            }
            Exports::Map(map) => map
                .get(&key)
                .and_then(|path| Self::resolve_manifest_target(package_root, path)),
            _ => None,
        }
    }

    fn load_package_config(package_root: &Path) -> Option<music_config::MusiConfig> {
        let config_path = package_root.join("musi.json");
        if !config_path.exists() {
            return None;
        }
        load_config(&config_path).ok()
    }

    fn resolve_manifest_target(package_root: &Path, target: &str) -> Option<PathBuf> {
        let candidate = package_root.join(target);
        if candidate.extension().is_some() && candidate.exists() {
            return Some(candidate);
        }

        let with_ext = candidate.with_extension("ms");
        if with_ext.exists() {
            return Some(with_ext);
        }

        if candidate.is_dir() {
            return Self::resolve_package_root(&candidate);
        }

        None
    }

    fn is_explicit_package_entry(candidate: &Path) -> bool {
        let Some(parent) = candidate.parent() else {
            return false;
        };

        let Some(entry) = Self::resolve_package_root(parent) else {
            return false;
        };

        let Ok(candidate) = candidate.canonicalize() else {
            return false;
        };

        candidate == entry
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
