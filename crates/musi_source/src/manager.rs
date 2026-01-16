use crate::{SourceFile, SourceSpan};
use std::{io, path::PathBuf};

/// Collection of source files with lookup capabilities
pub struct SourceManager {
    files: Vec<SourceFile>,
}

impl SourceManager {
    /// Create new empty source manager
    #[must_use]
    pub const fn new() -> Self {
        Self { files: vec![] }
    }

    /// Add source file to manager
    ///
    /// # Errors
    /// - File read fails
    /// - Path lacks `.ms` extension
    pub fn add_file(&mut self, path: PathBuf) -> io::Result<usize> {
        let file = SourceFile::from_path(path)?;
        let id = self.files.len();
        self.files.push(file);
        Ok(id)
    }

    /// Get source file by ID
    ///
    /// # Panics
    /// - ID is out of bounds
    #[must_use]
    pub fn get(&self, id: usize) -> &SourceFile {
        &self.files[id]
    }

    /// Get file path, line, and column for given span
    #[must_use]
    pub fn locate(&self, span: SourceSpan, file_id: usize) -> (String, usize, usize) {
        let file = self.get(file_id);
        let (line, column) = file.line_column(span.lo);
        (file.path.display().to_string(), line, column)
    }

    /// Check if manager contains no files
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.files.is_empty()
    }

    /// Get number of source files
    #[must_use]
    pub const fn len(&self) -> usize {
        self.files.len()
    }
}

impl Default for SourceManager {
    /// Get default source manager (empty)
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
