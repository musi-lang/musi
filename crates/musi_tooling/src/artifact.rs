use std::fs;
use std::io;
use std::path::{Path, PathBuf};

use crate::{ToolingError, ToolingResult};

struct ParentDir {
    created: Vec<PathBuf>,
}

fn ensure_parent_dir(path: &Path) -> ToolingResult<Option<ParentDir>> {
    let Some(parent) = path
        .parent()
        .filter(|parent| !parent.as_os_str().is_empty())
    else {
        return Ok(None);
    };
    let mut created = Vec::new();
    let mut current = Some(parent);
    while let Some(candidate) = current {
        if candidate
            .try_exists()
            .map_err(|source| ToolingError::ToolingIoFailed {
                path: candidate.to_path_buf(),
                source,
            })?
        {
            break;
        }
        created.push(candidate.to_path_buf());
        current = candidate
            .parent()
            .filter(|parent| !parent.as_os_str().is_empty());
    }
    fs::create_dir_all(parent).map_err(|source| ToolingError::ToolingIoFailed {
        path: parent.to_path_buf(),
        source,
    })?;
    Ok(Some(ParentDir { created }))
}

fn remove_empty_created_parent(parent: Option<&ParentDir>) {
    if let Some(parent) = parent {
        for path in &parent.created {
            drop(fs::remove_dir(path));
        }
    }
}

pub fn write_output(path: &Path, write: impl FnOnce(&Path) -> io::Result<()>) -> ToolingResult {
    let parent = ensure_parent_dir(path)?;
    write(path).map_err(|source| {
        remove_empty_created_parent(parent.as_ref());
        ToolingError::ToolingIoFailed {
            path: path.to_path_buf(),
            source,
        }
    })
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
    write_output(path, |path| fs::write(path, bytes))
}

/// # Errors
///
/// Returns [`ToolingError::ToolingIoFailed`] when the parent directory cannot be created or the
/// text output cannot be written to `path`.
pub fn write_text_output(path: &Path, text: &str) -> ToolingResult {
    write_output(path, |path| fs::write(path, text))
}
