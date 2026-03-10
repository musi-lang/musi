//! Builds a [`ResolverConfig`] from project root and manifest.

use std::collections::HashMap;
use std::env;
use std::path::{Path, PathBuf};

use musi_manifest::MusiManifest;
use music_resolve::{ResolverConfig, discover_std_root};

/// Builds a resolver configuration from the project root and optional manifest.
#[must_use]
pub fn build(project_root: &Path, manifest: Option<&MusiManifest>) -> ResolverConfig {
    let std_root = discover_std_root(project_root).unwrap_or_else(|_| project_root.join("std"));

    let cache_dir = cache_directory().join("musi/packages");

    let (manifest_imports, manifest_deps) = manifest.map_or_else(
        || (HashMap::new(), HashMap::new()),
        |m| (m.imports.clone(), m.dependencies.clone()),
    );

    ResolverConfig {
        std_root,
        cache_dir,
        project_root: project_root.to_path_buf(),
        manifest_imports,
        manifest_deps,
    }
}

fn cache_directory() -> PathBuf {
    env::var("HOME").map_or_else(
        |_| PathBuf::from("/tmp"),
        |home| PathBuf::from(home).join(".cache"),
    )
}
