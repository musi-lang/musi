use std::io;
use std::path::PathBuf;

use thiserror::Error;
use toml::de;

#[derive(Debug, Error)]
pub enum ManifestError {
    #[error("failed to read {path}: {source}")]
    Read { path: PathBuf, source: io::Error },
    #[error("failed to parse {path}: {source}")]
    Parse { path: PathBuf, source: de::Error },
}
