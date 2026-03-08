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
}
