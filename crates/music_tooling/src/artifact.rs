use std::fs;
use std::path::Path;

use crate::{ToolingError, ToolingResult};

pub fn read_artifact_bytes(path: &Path) -> ToolingResult<Vec<u8>> {
    fs::read(path).map_err(|source| ToolingError::Io {
        path: path.to_path_buf(),
        source,
    })
}

pub fn write_artifact_bytes(path: &Path, bytes: &[u8]) -> ToolingResult {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|source| ToolingError::Io {
            path: parent.to_path_buf(),
            source,
        })?;
    }
    fs::write(path, bytes).map_err(|source| ToolingError::Io {
        path: path.to_path_buf(),
        source,
    })
}

pub fn write_text_output(path: &Path, text: &str) -> ToolingResult {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|source| ToolingError::Io {
            path: parent.to_path_buf(),
            source,
        })?;
    }
    fs::write(path, text).map_err(|source| ToolingError::Io {
        path: path.to_path_buf(),
        source,
    })
}
