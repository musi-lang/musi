use std::error::Error;
use std::fmt;
use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

/// Parsed `git:` import specifier.
///
/// Format: `git:<domain>/<owner>/<repo>[@<version>]`
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GitSpecifier {
    pub domain: String,
    pub owner: String,
    pub repo: String,
    pub version: Option<String>,
}

impl GitSpecifier {
    /// Parse a specifier string (the part after `git:`).
    ///
    /// Expected format: `<domain>/<owner>/<repo>[@<version>]`
    ///
    /// Returns `None` if the format is invalid.
    #[must_use]
    pub fn parse(specifier: &str) -> Option<Self> {
        if specifier.is_empty() {
            return None;
        }

        let (path_part, version) = match specifier.rsplit_once('@') {
            Some((path, ver)) if !ver.is_empty() => (path, Some(ver.to_owned())),
            _ => (specifier, None),
        };

        // Must have exactly domain/owner/repo (3 segments)
        let segments: Vec<&str> = path_part.split('/').collect();
        if segments.len() != 3 {
            return None;
        }

        // Domain must contain a dot (e.g., github.com, gitlab.com)
        let domain = segments[0];
        if !domain.contains('.') {
            return None;
        }

        if segments[1].is_empty() || segments[2].is_empty() {
            return None;
        }

        Some(Self {
            domain: domain.to_owned(),
            owner: segments[1].to_owned(),
            repo: segments[2].to_owned(),
            version,
        })
    }

    /// Build the HTTPS clone URL.
    #[must_use]
    pub fn clone_url(&self) -> String {
        format!("https://{}/{}/{}", self.domain, self.owner, self.repo)
    }

    /// Compute the local cache path for this specifier.
    #[must_use]
    pub fn cache_path(&self, cache_root: &Path) -> PathBuf {
        let version_dir = self.version.as_deref().unwrap_or("latest");
        cache_root
            .join(&self.domain)
            .join(&self.owner)
            .join(&self.repo)
            .join(version_dir)
    }
}

/// Clone a git repository into the cache directory.
///
/// Uses `git clone --depth 1` for efficiency. If a version is specified,
/// clones that specific branch/tag.
///
/// # Errors
///
/// Returns an error if the git command fails.
pub fn clone_repo(spec: &GitSpecifier, dest: &Path) -> Result<(), GitError> {
    if let Some(parent) = dest.parent() {
        fs::create_dir_all(parent).map_err(|e| GitError::Io(e.to_string()))?;
    }

    let mut cmd = Command::new("git");
    let _ = cmd.arg("clone").arg("--depth").arg("1");

    if let Some(version) = &spec.version {
        let _ = cmd.arg("--branch").arg(version);
    }

    let _ = cmd.arg(spec.clone_url()).arg(dest);

    let output = cmd.output().map_err(|e| GitError::Io(e.to_string()))?;

    if output.status.success() {
        Ok(())
    } else {
        let stderr = String::from_utf8_lossy(&output.stderr).to_string();
        Err(GitError::CloneFailed {
            url: spec.clone_url(),
            stderr,
        })
    }
}

/// Find the entry point of a cloned package.
///
/// Reads `musi.json` for the `main` field. Falls back to `index.ms`.
#[must_use]
pub fn find_entry_point(package_root: &Path) -> Option<PathBuf> {
    let config_path = package_root.join("musi.json");
    if config_path.exists() {
        if let Ok(contents) = fs::read_to_string(config_path) {
            if let Ok(config) = serde_json::from_str::<serde_json::Value>(&contents) {
                if let Some(main) = config.get("main").and_then(|v| v.as_str()) {
                    let main_path = package_root.join(main);
                    if main_path.exists() {
                        return Some(main_path);
                    }
                    let with_ext = main_path.with_extension("ms");
                    if with_ext.exists() {
                        return Some(with_ext);
                    }
                }
            }
        }
    }

    let index_file = package_root.join("index.ms");
    if index_file.exists() {
        return Some(index_file);
    }

    None
}

/// Errors that can occur during git-based resolution.
#[derive(Debug, Clone)]
pub enum GitError {
    Io(String),
    CloneFailed { url: String, stderr: String },
}

impl fmt::Display for GitError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io(msg) => write!(f, "git I/O error: {msg}"),
            Self::CloneFailed { url, stderr } => {
                write!(f, "failed to clone `{url}`: {stderr}")
            }
        }
    }
}

impl Error for GitError {}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
