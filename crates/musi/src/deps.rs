//! Dependency specification parsing and resolution.

use std::collections::HashMap;
use std::io;
use std::path::PathBuf;

use semver::VersionReq;
use thiserror::Error;

use crate::config::MusiConfig;

/// Source of a dependency.
#[derive(Debug, Clone)]
pub enum DepSource {
    /// GitHub repository: `github.com/{owner}/{repo}`.
    GitHub { owner: String, repo: String },
    /// Local file path.
    File { path: PathBuf },
}

/// Parsed dependency specification.
#[derive(Debug, Clone)]
pub struct DepSpec {
    /// Original name from mspackage.json (e.g., `@musi-lang/libc-ms`).
    pub name: String,
    /// Scope without `@` prefix (e.g., `musi-lang`).
    #[allow(dead_code)]
    pub scope: Option<String>,
    /// Package name (e.g., `libc-ms`).
    #[allow(dead_code)]
    pub package: String,
    /// Semver version requirement.
    pub version_req: VersionReq,
    /// Resolved source location.
    pub source: DepSource,
}

#[derive(Debug, Error)]
#[allow(dead_code)]
pub enum DepError {
    #[error("invalid dependency name: {0}")]
    InvalidName(String),
    #[error("invalid version requirement '{0}': {1}")]
    InvalidVersion(String, semver::Error),
    #[error("dependency not found: {0}")]
    NotFound(String),
    #[error("fetch error: {0}")]
    Fetch(String),
    #[error("io error: {0}")]
    Io(#[from] io::Error),
}

/// Parse a URL import path into a DepSpec.
///
/// Supported formats:
/// - `github:scope/repo` (latest version)
/// - `github:scope/repo@v0.1.0` (specific version)
/// - `github:scope/repo/path/to/module` (subpath, uses latest)
/// - `https://github.com/scope/repo` (full URL)
///
/// Returns `(DepSpec, Option<subpath>)` where subpath is the path within the repo.
pub fn parse_url_import(import_path: &str) -> Option<(DepSpec, Option<String>)> {
    // Handle github: prefix
    if let Some(rest) = import_path.strip_prefix("github:") {
        return parse_github_shorthand(rest);
    }

    // Handle https://github.com/ URLs
    if let Some(rest) = import_path.strip_prefix("https://github.com/") {
        return parse_github_shorthand(rest);
    }

    None
}

/// Parse GitHub shorthand: `scope/repo[@version][/path]`
fn parse_github_shorthand(input: &str) -> Option<(DepSpec, Option<String>)> {
    // Split into parts: scope/repo[@version][/subpath...]
    let mut parts = input.splitn(2, '/');
    let owner = parts.next()?;
    let rest = parts.next()?;

    // Check for version specifier
    let (repo_and_version, subpath) = if let Some(slash_pos) = rest.find('/') {
        let (rv, sub) = rest.split_at(slash_pos);
        (rv, Some(sub.trim_start_matches('/').to_owned()))
    } else {
        (rest, None)
    };

    // Parse repo@version or just repo
    let (repo, version_str) = if let Some(at_pos) = repo_and_version.find('@') {
        let (r, v) = repo_and_version.split_at(at_pos);
        (r, v.trim_start_matches('@').trim_start_matches('v'))
    } else {
        (repo_and_version, "*")
    };

    // Build the DepSpec
    let version_req = semver::VersionReq::parse(version_str).ok()?;
    let name = format!("@{owner}/{repo}");

    Some((
        DepSpec {
            name,
            scope: Some(owner.to_owned()),
            package: repo.to_owned(),
            version_req,
            source: DepSource::GitHub {
                owner: owner.to_owned(),
                repo: repo.to_owned(),
            },
        },
        subpath,
    ))
}

/// Check if an import path is a URL import.
pub fn is_url_import(import_path: &str) -> bool {
    import_path.starts_with("github:")
        || import_path.starts_with("https://github.com/")
}

/// Parse a dependency spec from name and version string.
///
/// Formats:
/// - `@scope/package` with version `^0.1.0` -> GitHub
/// - `local-lib` with version `file:./path` -> Local file
pub fn parse_dep_spec(name: &str, version: &str) -> Result<DepSpec, DepError> {
    // Handle file: prefix for local dependencies
    if let Some(path_str) = version.strip_prefix("file:") {
        let path = PathBuf::from(path_str);
        let package = name.trim_start_matches('@');
        let package = package.rsplit('/').next().unwrap_or(package);
        return Ok(DepSpec {
            name: name.to_owned(),
            scope: None,
            package: package.to_owned(),
            version_req: VersionReq::STAR,
            source: DepSource::File { path },
        });
    }

    // Parse version requirement
    let version_req =
        VersionReq::parse(version).map_err(|e| DepError::InvalidVersion(version.to_owned(), e))?;

    // Parse scoped package name: @scope/package
    if let Some(rest) = name.strip_prefix('@') {
        let parts: Vec<&str> = rest.splitn(2, '/').collect();
        if parts.len() != 2 {
            return Err(DepError::InvalidName(name.to_owned()));
        }
        let scope = parts[0];
        let package = parts[1];
        return Ok(DepSpec {
            name: name.to_owned(),
            scope: Some(scope.to_owned()),
            package: package.to_owned(),
            version_req,
            source: DepSource::GitHub {
                owner: scope.to_owned(),
                repo: package.to_owned(),
            },
        });
    }

    // Unscoped package: assume GitHub user/repo format
    Err(DepError::InvalidName(format!(
        "unscoped package '{name}' not supported; use @scope/package format"
    )))
}

/// Get the global cache directory (`~/.musi/cache`).
pub fn cache_dir() -> Option<PathBuf> {
    dirs::home_dir().map(|h| h.join(".musi").join("cache"))
}

/// Resolve cache path for a dependency at a specific version.
pub fn resolve_cache_path(spec: &DepSpec, version: &str) -> Option<PathBuf> {
    match &spec.source {
        DepSource::File { path } => Some(path.clone()),
        DepSource::GitHub { owner, repo } => {
            let cache = cache_dir()?;
            Some(
                cache
                    .join("github.com")
                    .join(owner)
                    .join(repo)
                    .join(format!("v{version}")),
            )
        }
    }
}

/// Check if a dependency is already cached at the specified version.
pub fn is_cached(spec: &DepSpec, version: &str) -> bool {
    resolve_cache_path(spec, version)
        .is_some_and(|p| p.join("mspackage.json").exists())
}

/// Parse all dependencies from a config into `DepSpec` list.
pub fn parse_all_deps(config: &MusiConfig) -> Result<Vec<DepSpec>, DepError> {
    let mut specs = Vec::new();
    for (name, version) in &config.dependencies {
        specs.push(parse_dep_spec(name, version)?);
    }
    Ok(specs)
}

/// Parse all dev dependencies from a config into `DepSpec` list.
pub fn parse_dev_deps(config: &MusiConfig) -> Result<Vec<DepSpec>, DepError> {
    let mut specs = Vec::new();
    for (name, version) in &config.dev_dependencies {
        specs.push(parse_dep_spec(name, version)?);
    }
    Ok(specs)
}

/// Resolved dependency with exact version and cache path.
#[derive(Debug, Clone)]
pub struct ResolvedDep {
    pub spec: DepSpec,
    pub version: String,
    #[allow(dead_code)]
    pub cache_path: PathBuf,
    pub integrity: Option<String>,
}

/// Map of dependency name -> resolved dependency.
#[allow(dead_code)]
pub type ResolvedDeps = HashMap<String, ResolvedDep>;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn parse_scoped_github_dep() {
        let spec = parse_dep_spec("@musi-lang/libc-ms", "^0.1.0").unwrap();
        assert_eq!(spec.name, "@musi-lang/libc-ms");
        assert_eq!(spec.scope, Some("musi-lang".to_owned()));
        assert_eq!(spec.package, "libc-ms");
        assert!(
            matches!(spec.source, DepSource::GitHub { owner, repo } if owner == "musi-lang" && repo == "libc-ms")
        );
    }

    #[test]
    fn parse_file_dep() {
        let spec = parse_dep_spec("local-lib", "file:./libs/local").unwrap();
        assert_eq!(spec.package, "local-lib");
        assert!(
            matches!(spec.source, DepSource::File { path } if path == PathBuf::from("./libs/local"))
        );
    }

    #[test]
    fn parse_exact_version() {
        // In semver, "1.2.3" is equivalent to "^1.2.3" (allows compatible updates)
        // Use "=1.2.3" for exact match
        let spec = parse_dep_spec("@user/pkg", "=1.2.3").unwrap();
        assert!(spec.version_req.matches(&semver::Version::new(1, 2, 3)));
        assert!(!spec.version_req.matches(&semver::Version::new(1, 2, 4)));
    }

    #[test]
    fn parse_star_version() {
        let spec = parse_dep_spec("@user/pkg", "*").unwrap();
        assert!(spec.version_req.matches(&semver::Version::new(99, 99, 99)));
    }

    #[test]
    fn reject_unscoped() {
        let err = parse_dep_spec("plain-name", "^1.0.0").unwrap_err();
        assert!(matches!(err, DepError::InvalidName(_)));
    }

    #[test]
    fn url_import_github_shorthand() {
        let (spec, subpath) = parse_url_import("github:musi-lang/libc-ms").unwrap();
        assert_eq!(spec.name, "@musi-lang/libc-ms");
        assert_eq!(spec.scope, Some("musi-lang".to_owned()));
        assert_eq!(spec.package, "libc-ms");
        assert!(subpath.is_none());
        assert!(matches!(
            spec.source,
            DepSource::GitHub { owner, repo } if owner == "musi-lang" && repo == "libc-ms"
        ));
    }

    #[test]
    fn url_import_github_with_version() {
        let (spec, subpath) = parse_url_import("github:user/repo@0.1.0").unwrap();
        assert_eq!(spec.name, "@user/repo");
        assert!(subpath.is_none());
        assert!(spec.version_req.matches(&semver::Version::new(0, 1, 0)));
    }

    #[test]
    fn url_import_github_with_v_prefix() {
        let (spec, _) = parse_url_import("github:user/repo@v1.2.3").unwrap();
        assert!(spec.version_req.matches(&semver::Version::new(1, 2, 3)));
    }

    #[test]
    fn url_import_github_with_subpath() {
        let (spec, subpath) = parse_url_import("github:user/repo/src/utils").unwrap();
        assert_eq!(spec.name, "@user/repo");
        assert_eq!(subpath, Some("src/utils".to_owned()));
    }

    #[test]
    fn url_import_https_github() {
        let (spec, _) = parse_url_import("https://github.com/musi-lang/libc-ms").unwrap();
        assert_eq!(spec.name, "@musi-lang/libc-ms");
    }

    #[test]
    fn is_url_import_detection() {
        assert!(is_url_import("github:user/repo"));
        assert!(is_url_import("https://github.com/user/repo"));
        assert!(!is_url_import("@user/repo"));
        assert!(!is_url_import("./local/path"));
        assert!(!is_url_import("std/math"));
    }
}
