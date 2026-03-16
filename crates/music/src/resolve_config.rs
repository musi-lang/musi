//! Builds a [`ResolverConfig`] from project root and manifest.

use std::path::Path;

use musi_manifest::MusiManifest;
use music_resolve::ResolverConfig;

/// Builds a resolver configuration from the project root and optional manifest.
#[must_use]
pub fn build(project_root: &Path, manifest: Option<&MusiManifest>) -> ResolverConfig {
    ResolverConfig::from_project(project_root, manifest)
}
