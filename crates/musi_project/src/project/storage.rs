use std::env;
use std::path::{Path, PathBuf};

use crate::ProjectResult;
use crate::errors::ProjectError;
use crate::manifest::PackageManifest;
use crate::project::ProjectOptions;

#[derive(Debug, Clone)]
pub(super) struct ProjectStorage {
    pub(super) global_cache_dir: PathBuf,
    pub(super) modules_dir: Option<PathBuf>,
}

impl ProjectStorage {
    pub(super) fn new(
        root_dir: &Path,
        manifest: &PackageManifest,
        options: &ProjectOptions,
    ) -> ProjectResult<Self> {
        let global_cache_dir = options
            .global_cache_root
            .clone()
            .map_or_else(default_global_cache_root, Ok)?;
        let modules_dir = manifest
            .modules_dir()
            .map(|path| resolve_project_path(root_dir, path));
        Ok(Self {
            global_cache_dir,
            modules_dir,
        })
    }
}

pub(super) fn resolve_project_path(root_dir: &Path, raw: &str) -> PathBuf {
    let path = Path::new(raw);
    if path.is_absolute() {
        path.to_path_buf()
    } else {
        root_dir.join(path)
    }
}

fn default_global_cache_root() -> ProjectResult<PathBuf> {
    if let Some(path) = env_path("MUSI_CACHE_DIR") {
        return Ok(path);
    }
    #[cfg(windows)]
    {
        if let Some(path) = env_path("LOCALAPPDATA") {
            return Ok(path.join("musi").join("cache"));
        }
    }
    #[cfg(not(windows))]
    {
        if let Some(path) = env_path("XDG_CACHE_HOME") {
            return Ok(path.join("musi"));
        }
        if let Some(path) = env_path("HOME") {
            return Ok(path.join(".cache").join("musi"));
        }
    }
    Err(ProjectError::MissingGlobalCacheRoot)
}

fn env_path(name: &str) -> Option<PathBuf> {
    env::var_os(name)
        .filter(|value| !value.is_empty())
        .map(PathBuf::from)
}
