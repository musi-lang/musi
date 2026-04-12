use std::fs;
use std::path::Path;

use crate::{ToolingError, ToolingResult};

fn create_parent_dir(path: &Path) -> ToolingResult {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|source| ToolingError::ToolingIoFailed {
            path: parent.to_path_buf(),
            source,
        })?;
    }
    Ok(())
}

/// # Errors
///
/// Returns [`ToolingError::ToolingIoFailed`] when the artifact bytes cannot be read from `path`.
pub fn read_artifact_bytes(path: &Path) -> ToolingResult<Vec<u8>> {
    fs::read(path).map_err(|source| ToolingError::ToolingIoFailed {
        path: path.to_path_buf(),
        source,
    })
}

/// # Errors
///
/// Returns [`ToolingError::ToolingIoFailed`] when the parent directory cannot be created or the
/// artifact bytes cannot be written to `path`.
pub fn write_artifact_bytes(path: &Path, bytes: &[u8]) -> ToolingResult {
    create_parent_dir(path)?;
    fs::write(path, bytes).map_err(|source| ToolingError::ToolingIoFailed {
        path: path.to_path_buf(),
        source,
    })
}

/// # Errors
///
/// Returns [`ToolingError::ToolingIoFailed`] when the parent directory cannot be created or the
/// text output cannot be written to `path`.
pub fn write_text_output(path: &Path, text: &str) -> ToolingResult {
    create_parent_dir(path)?;
    fs::write(path, text).map_err(|source| ToolingError::ToolingIoFailed {
        path: path.to_path_buf(),
        source,
    })
}
