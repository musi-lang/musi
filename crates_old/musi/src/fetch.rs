//! GitHub API and tarball fetching for package downloads.

use std::collections::HashMap;
use std::fs;
use std::io::{self, Read};
use std::path::{Path, PathBuf};

use flate2::read::GzDecoder;
use semver::{Version, VersionReq};
use sha2::{Digest, Sha256};
use tar::Archive;
use thiserror::Error;

use crate::deps::{self, DepSource, DepSpec, ResolvedDep, cache_dir};

const USER_AGENT: &str = "musi-package-manager/0.1";

#[derive(Debug, Error)]
pub enum FetchError {
    #[error("HTTP error: {0}")]
    Http(String),
    #[error("no matching version for {name} ({req})")]
    NoMatchingVersion { name: String, req: String },
    #[error("failed to parse GitHub tags response")]
    ParseTags,
    #[error("io error: {0}")]
    Io(#[from] io::Error),
    #[error("tarball extraction failed: {0}")]
    Extract(String),
    #[error("package has no mspackage.json")]
    NoManifest,
}

/// A GitHub tag with name and tarball URL.
#[derive(Debug)]
struct GitHubTag {
    name: String,
    tarball_url: String,
}

/// Fetch list of tags from a GitHub repository.
fn fetch_github_tags(owner: &str, repo: &str) -> Result<Vec<GitHubTag>, FetchError> {
    let url = format!("https://api.github.com/repos/{owner}/{repo}/tags");

    let response = ureq::get(&url)
        .header("User-Agent", USER_AGENT)
        .header("Accept", "application/vnd.github.v3+json")
        .call()
        .map_err(|e| FetchError::Http(e.to_string()))?;

    let body = response
        .into_body()
        .read_to_string()
        .map_err(|e| FetchError::Http(e.to_string()))?;

    // Parse JSON manually to avoid serde dependency for this simple case
    parse_tags_json(&body)
}

fn parse_tags_json(json: &str) -> Result<Vec<GitHubTag>, FetchError> {
    let parsed: serde_json::Value =
        serde_json::from_str(json).map_err(|_| FetchError::ParseTags)?;

    let arr = parsed.as_array().ok_or(FetchError::ParseTags)?;
    let mut tags = Vec::new();

    for item in arr {
        let name = item
            .get("name")
            .and_then(|v| v.as_str())
            .ok_or(FetchError::ParseTags)?;
        let tarball_url = item
            .get("tarball_url")
            .and_then(|v| v.as_str())
            .ok_or(FetchError::ParseTags)?;
        tags.push(GitHubTag {
            name: name.to_owned(),
            tarball_url: tarball_url.to_owned(),
        });
    }
    Ok(tags)
}

/// Parse a tag name into a semver Version, stripping common prefixes.
fn parse_tag_version(tag: &str) -> Option<Version> {
    let stripped = tag.strip_prefix('v').unwrap_or(tag);
    Version::parse(stripped).ok()
}

/// Find the best matching tag for a version requirement.
fn find_matching_tag<'a>(tags: &'a [GitHubTag], req: &VersionReq) -> Option<&'a GitHubTag> {
    tags.iter()
        .filter_map(|t| {
            let v = parse_tag_version(&t.name)?;
            if req.matches(&v) { Some((t, v)) } else { None }
        })
        .max_by(|(_, a), (_, b)| a.cmp(b))
        .map(|(t, _)| t)
}

/// Resolve the best matching version for a dependency spec.
pub fn resolve_version(spec: &DepSpec) -> Result<String, FetchError> {
    match &spec.source {
        DepSource::File { .. } => Ok("local".to_owned()),
        DepSource::GitHub { owner, repo } => {
            let tags = fetch_github_tags(owner, repo)?;
            let tag = find_matching_tag(&tags, &spec.version_req).ok_or_else(|| {
                FetchError::NoMatchingVersion {
                    name: spec.name.clone(),
                    req: spec.version_req.to_string(),
                }
            })?;
            // Return version without 'v' prefix
            Ok(tag.name.strip_prefix('v').unwrap_or(&tag.name).to_owned())
        }
    }
}

/// Download and extract a GitHub tarball to the cache.
fn fetch_and_extract_tarball(url: &str, dest: &Path) -> Result<String, FetchError> {
    // Create parent directories
    if let Some(parent) = dest.parent() {
        fs::create_dir_all(parent)?;
    }

    // Download tarball
    let response = ureq::get(url)
        .header("User-Agent", USER_AGENT)
        .call()
        .map_err(|e| FetchError::Http(e.to_string()))?;

    let mut body = Vec::new();
    let _ = response
        .into_body()
        .into_reader()
        .read_to_end(&mut body)
        .map_err(|e| FetchError::Http(e.to_string()))?;

    // Compute SHA256 integrity hash
    let mut hasher = Sha256::new();
    hasher.update(&body);
    let hash = hex::encode(hasher.finalize());
    let integrity = format!("sha256-{hash}");

    // Extract tarball (GitHub tarballs have a single root directory)
    let gz = GzDecoder::new(&body[..]);
    let mut archive = Archive::new(gz);

    // Create a temp directory for extraction
    let temp_dir = dest.with_extension("tmp");
    if temp_dir.exists() {
        fs::remove_dir_all(&temp_dir)?;
    }
    fs::create_dir_all(&temp_dir)?;

    archive
        .unpack(&temp_dir)
        .map_err(|e| FetchError::Extract(e.to_string()))?;

    // Find the extracted root directory (GitHub format: owner-repo-hash)
    let entries: Vec<_> = fs::read_dir(&temp_dir)?.filter_map(Result::ok).collect();

    if entries.len() != 1 {
        drop(fs::remove_dir_all(&temp_dir));
        return Err(FetchError::Extract(
            "expected single root directory in tarball".to_owned(),
        ));
    }

    let extracted_root = entries[0].path();

    // Move extracted contents to final destination
    if dest.exists() {
        fs::remove_dir_all(dest)?;
    }
    fs::rename(&extracted_root, dest)?;
    drop(fs::remove_dir_all(&temp_dir));

    // Verify mspackage.json exists
    if !dest.join("mspackage.json").exists() {
        drop(fs::remove_dir_all(dest));
        return Err(FetchError::NoManifest);
    }

    Ok(integrity)
}

/// Ensure a dependency is installed to the cache, returning its path and metadata.
pub fn ensure_installed(spec: &DepSpec) -> Result<ResolvedDep, FetchError> {
    match &spec.source {
        DepSource::File { path } => {
            // Local dependency - just verify it exists
            if !path.exists() {
                return Err(FetchError::Io(io::Error::new(
                    io::ErrorKind::NotFound,
                    format!("local dependency not found: {}", path.display()),
                )));
            }
            Ok(ResolvedDep {
                spec: spec.clone(),
                version: "local".to_owned(),
                cache_path: path.clone(),
                integrity: None,
            })
        }
        DepSource::GitHub { owner, repo } => {
            // Resolve version
            let version = resolve_version(spec)?;

            // Check cache
            let cache = cache_dir().ok_or_else(|| {
                FetchError::Io(io::Error::new(
                    io::ErrorKind::NotFound,
                    "could not determine home directory",
                ))
            })?;
            let cache_path = cache
                .join("github.com")
                .join(owner)
                .join(repo)
                .join(format!("v{version}"));

            // If already cached, return it
            if cache_path.join("mspackage.json").exists() {
                return Ok(ResolvedDep {
                    spec: spec.clone(),
                    version,
                    cache_path,
                    integrity: None, // Could read from lock file
                });
            }

            // Fetch tags to get tarball URL
            let tags = fetch_github_tags(owner, repo)?;
            let tag = find_matching_tag(&tags, &spec.version_req).ok_or_else(|| {
                FetchError::NoMatchingVersion {
                    name: spec.name.clone(),
                    req: spec.version_req.to_string(),
                }
            })?;

            // Download and extract
            let integrity = fetch_and_extract_tarball(&tag.tarball_url, &cache_path)?;

            Ok(ResolvedDep {
                spec: spec.clone(),
                version,
                cache_path,
                integrity: Some(integrity),
            })
        }
    }
}

/// Resolve a package import path to its cache directory.
/// Returns `None` if the import is not a package dependency.
#[allow(dead_code)]
pub fn resolve_package_import(
    import_path: &str,
    dep_map: &HashMap<String, String>,
) -> Option<PathBuf> {
    // Check if import starts with a known dependency name
    for (name, version) in dep_map {
        if import_path == name || import_path.starts_with(&format!("{name}/")) {
            let spec = deps::parse_dep_spec(name, version).ok()?;
            let resolved = ensure_installed(&spec).ok()?;

            // If importing a subpath like "@scope/pkg/submodule", append it
            if import_path.len() > name.len() {
                let subpath = import_path.get(name.len()..)?.trim_start_matches('/');
                return Some(resolved.cache_path.join(subpath));
            }
            return Some(resolved.cache_path);
        }
    }
    None
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_semver_tags() {
        assert_eq!(parse_tag_version("v0.1.0"), Some(Version::new(0, 1, 0)));
        assert_eq!(parse_tag_version("1.2.3"), Some(Version::new(1, 2, 3)));
        assert_eq!(
            parse_tag_version("v2.0.0-alpha"),
            Some(Version::parse("2.0.0-alpha").unwrap())
        );
        assert_eq!(parse_tag_version("not-a-version"), None);
    }

    #[test]
    fn find_best_match() {
        let tags = vec![
            GitHubTag {
                name: "v0.1.0".to_owned(),
                tarball_url: "url1".to_owned(),
            },
            GitHubTag {
                name: "v0.1.5".to_owned(),
                tarball_url: "url1b".to_owned(),
            },
            GitHubTag {
                name: "v0.2.0".to_owned(),
                tarball_url: "url2".to_owned(),
            },
            GitHubTag {
                name: "v1.0.0".to_owned(),
                tarball_url: "url3".to_owned(),
            },
        ];

        // ^0.1.0 means >=0.1.0, <0.2.0 (caret is restrictive for 0.x)
        let req = VersionReq::parse("^0.1.0").unwrap();
        let matched = find_matching_tag(&tags, &req).unwrap();
        assert_eq!(matched.name, "v0.1.5"); // highest in 0.1.x range

        // ^1.0.0 means >=1.0.0, <2.0.0
        let req = VersionReq::parse("^1.0.0").unwrap();
        let matched = find_matching_tag(&tags, &req).unwrap();
        assert_eq!(matched.name, "v1.0.0");
    }
}
