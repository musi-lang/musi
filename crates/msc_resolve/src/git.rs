//! Git-based package fetching and caching.

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

use sha2::{Digest, Sha256};

use msc_manifest::MusiManifest;

use crate::error::ResolveError;
use crate::specifier::GitSource;

/// Result of fetching a git package.
pub struct GitFetchResult {
    /// Root directory of the fetched package.
    pub local_path: PathBuf,
    /// Resolved entry point file.
    pub entry_point: PathBuf,
    /// Parsed manifest, if one exists.
    pub manifest: Option<MusiManifest>,
}

/// Computes the cache directory name for a git URL.
///
/// Uses the first 16 hex characters of the SHA-256 hash of the URL.
#[must_use]
pub fn cache_key(url: &str) -> String {
    let mut hasher = Sha256::new();
    hasher.update(url.as_bytes());
    let hash = hasher.finalize();
    hex::encode(&hash[..8])
}

/// Fetches a git package into the cache directory.
///
/// If the package is already cached (`.musi-fetched` marker exists), it is
/// reused. Otherwise, `git clone --depth 1` is invoked. When a tag is
/// specified, the repository is checked out to that tag.
///
/// The entry point is determined from `musi.json` (`main`) or
/// defaults to `index.ms`.
///
/// # Errors
///
/// Returns [`ResolveError::GitFetchFailed`] when the git command fails,
/// or [`ResolveError::Io`] on filesystem errors.
pub fn fetch_git_package(
    source: &GitSource,
    cache_dir: &Path,
) -> Result<GitFetchResult, ResolveError> {
    let key = cache_key(&source.url);
    let pkg_dir = cache_dir.join("git").join(&key);
    let marker = pkg_dir.join(".musi-fetched");

    if !marker.exists() {
        fs::create_dir_all(&pkg_dir).map_err(|e| ResolveError::Io {
            path: Box::from(pkg_dir.display().to_string()),
            source: e,
        })?;

        run_git_clone(&source.url, &pkg_dir)?;

        if let Some(tag) = &source.tag {
            run_git_checkout(&pkg_dir, tag)?;
        }

        fs::write(&marker, "").map_err(|e| ResolveError::Io {
            path: Box::from(marker.display().to_string()),
            source: e,
        })?;
    }

    let manifest = load_manifest(&pkg_dir);
    let entry_point = resolve_entry_point(&pkg_dir, manifest.as_ref());

    Ok(GitFetchResult {
        local_path: pkg_dir,
        entry_point,
        manifest,
    })
}

fn run_git_clone(url: &str, target: &Path) -> Result<(), ResolveError> {
    let output = Command::new("git")
        .args(["clone", "--depth", "1", url])
        .arg(target)
        .output()
        .map_err(|e| ResolveError::GitFetchFailed {
            url: Box::from(url),
            reason: Box::from(e.to_string()),
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ResolveError::GitFetchFailed {
            url: Box::from(url),
            reason: Box::from(stderr.trim()),
        });
    }

    Ok(())
}

fn run_git_checkout(repo: &Path, tag: &str) -> Result<(), ResolveError> {
    let output = Command::new("git")
        .args(["-C"])
        .arg(repo)
        .args(["fetch", "--depth", "1", "origin", "tag", tag])
        .output()
        .map_err(|e| ResolveError::GitFetchFailed {
            url: Box::from(repo.display().to_string()),
            reason: Box::from(e.to_string()),
        })?;

    if !output.status.success() {
        let stderr = String::from_utf8_lossy(&output.stderr);
        return Err(ResolveError::GitFetchFailed {
            url: Box::from(repo.display().to_string()),
            reason: Box::from(stderr.trim()),
        });
    }

    let checkout = Command::new("git")
        .args(["-C"])
        .arg(repo)
        .args(["checkout", tag])
        .output()
        .map_err(|e| ResolveError::GitFetchFailed {
            url: Box::from(repo.display().to_string()),
            reason: Box::from(e.to_string()),
        })?;

    if !checkout.status.success() {
        let stderr = String::from_utf8_lossy(&checkout.stderr);
        return Err(ResolveError::GitFetchFailed {
            url: Box::from(repo.display().to_string()),
            reason: Box::from(stderr.trim()),
        });
    }

    Ok(())
}

fn load_manifest(pkg_dir: &Path) -> Option<MusiManifest> {
    let manifest_path = pkg_dir.join("musi.json");
    msc_manifest::load(&manifest_path).ok()
}

fn resolve_entry_point(pkg_dir: &Path, manifest: Option<&MusiManifest>) -> PathBuf {
    pkg_dir.join(manifest.map_or("index.ms", |m| m.entry_point()))
}

#[cfg(test)]
mod tests;
