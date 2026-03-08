//! Lock file support for reproducible builds.
//!
//! Lock file format (`musi.lock`):
//! ```json
//! {
//!   "version": 1,
//!   "packages": {
//!     "@scope/name": {
//!       "version": "0.1.0",
//!       "resolved": "github.com/scope/name",
//!       "integrity": "sha256-...",
//!       "dependencies": {}
//!     }
//!   }
//! }
//! ```

use std::collections::HashMap;
use std::fs;
use std::io;
use std::path::Path;

use serde::{Deserialize, Serialize};

use crate::deps::{DepSource, ResolvedDep};

const LOCK_FILENAME: &str = "musi.lock";
const LOCK_VERSION: u32 = 1;

/// Entry for a single package in the lock file.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LockEntry {
    /// Exact resolved version.
    pub version: String,
    /// Resolved source (e.g., `github.com/scope/name`).
    pub resolved: String,
    /// Integrity hash (e.g., `sha256-...`).
    #[serde(skip_serializing_if = "Option::is_none")]
    pub integrity: Option<String>,
    /// Transitive dependencies of this package.
    #[serde(default, skip_serializing_if = "HashMap::is_empty")]
    pub dependencies: HashMap<String, String>,
}

/// The lock file structure.
#[derive(Debug, Serialize, Deserialize)]
pub struct LockFile {
    /// Lock file format version.
    pub version: u32,
    /// Map of package name -> lock entry.
    #[serde(default)]
    pub packages: HashMap<String, LockEntry>,
}

impl Default for LockFile {
    fn default() -> Self {
        Self {
            version: LOCK_VERSION,
            packages: HashMap::new(),
        }
    }
}

#[allow(dead_code)]
impl LockFile {
    /// Create a new empty lock file.
    pub fn new() -> Self {
        Self::default()
    }

    /// Read lock file from a project directory.
    pub fn read_from(dir: &Path) -> Option<Self> {
        let path = dir.join(LOCK_FILENAME);
        let text = fs::read_to_string(&path).ok()?;
        serde_json::from_str(&text).ok()
    }

    /// Write lock file to a project directory.
    pub fn write_to(&self, dir: &Path) -> io::Result<()> {
        let path = dir.join(LOCK_FILENAME);
        // Lock file serialization is infallible since all fields are serializable
        let text = serde_json::to_string_pretty(self).unwrap_or_else(|_| "{}".to_owned());
        fs::write(&path, text)
    }

    /// Add or update a resolved dependency in the lock file.
    pub fn add_resolved(&mut self, resolved: &ResolvedDep) {
        let source = match &resolved.spec.source {
            DepSource::GitHub { owner, repo } => {
                format!("github.com/{owner}/{repo}")
            }
            DepSource::File { path } => {
                format!("file:{}", path.display())
            }
        };

        let _ = self.packages.insert(
            resolved.spec.name.clone(),
            LockEntry {
                version: resolved.version.clone(),
                resolved: source,
                integrity: resolved.integrity.clone(),
                dependencies: HashMap::new(),
            },
        );
    }

    /// Get a locked version for a package, if present.
    pub fn get_locked_version(&self, name: &str) -> Option<&str> {
        self.packages.get(name).map(|e| e.version.as_str())
    }

    /// Check if lock file is up-to-date with config dependencies.
    pub fn is_up_to_date(&self, deps: &HashMap<String, String>) -> bool {
        if self.packages.len() != deps.len() {
            return false;
        }
        for name in deps.keys() {
            if !self.packages.contains_key(name) {
                return false;
            }
        }
        true
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn roundtrip_lock_file() {
        let mut lock = LockFile::new();
        let _ = lock.packages.insert(
            "@musi-lang/libc-ms".to_owned(),
            LockEntry {
                version: "0.1.0".to_owned(),
                resolved: "github.com/musi-lang/libc-ms".to_owned(),
                integrity: Some("sha256-abc123".to_owned()),
                dependencies: HashMap::new(),
            },
        );

        let json = serde_json::to_string(&lock).unwrap();
        let parsed: LockFile = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.version, 1);
        assert!(parsed.packages.contains_key("@musi-lang/libc-ms"));
    }
}
