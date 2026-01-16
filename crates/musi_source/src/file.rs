//! Source file representation with line mapping

use crate::{SourcePosition, SourceSpan};
use std::{ffi::OsStr, fs, io, path::PathBuf};

/// Source file with contents and safe UTF-8-aware slicing
#[derive(Debug)]
pub struct SourceFile {
    /// Filesystem path
    pub path: PathBuf,
    /// File contents
    pub contents: String,
    /// Byte offsets where each line starts
    pub(crate) line_starts: Vec<u32>,
}

impl SourceFile {
    /// Load `.ms` source file from path
    ///
    /// # Errors
    /// If file read fails or path lacks `.ms` extension
    pub fn from_path(path: PathBuf) -> io::Result<Self> {
        if path.extension() != Some(OsStr::new("ms")) {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                format!(
                    "invalid file extension, expected '.ms' file: {}",
                    path.display()
                ),
            ));
        }

        let contents = fs::read_to_string(&path)?;
        let line_starts = Self::compute_line_starts(&contents);

        Ok(Self {
            path,
            contents,
            line_starts,
        })
    }

    /// Compute byte offsets where each line begins
    pub(crate) fn compute_line_starts(contents: &str) -> Vec<u32> {
        let mut starts = vec![0];
        for (i, byte) in contents.bytes().enumerate() {
            if byte == b'\n' {
                starts.push(
                    (i + 1)
                        .try_into()
                        .expect("file too large: line start offset exceeds u32"),
                );
            }
        }
        starts
    }

    /// Find line index for given byte offset
    pub(crate) fn line_index(&self, offset: SourcePosition) -> usize {
        match self.line_starts.binary_search(&offset.position) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        }
    }

    /// Converts byte offset to `(line, column)` coordinates
    /// # Panics
    /// If byte offset is out of bounds
    #[must_use]
    pub fn line_column(&self, offset: SourcePosition) -> (usize, usize) {
        let line_idx = self.line_index(offset);
        let line_start = self.line_starts[line_idx];
        let column = usize::try_from(offset.position - line_start).expect("column overflow");
        (line_idx + 1, column + 1)
    }

    /// Extract substring from file contents with UTF-8 safety
    ///
    /// # Panics
    /// If span byte offsets overflow `usize`
    #[must_use]
    pub fn slice(&self, span: SourceSpan) -> &str {
        let start = usize::try_from(span.lo.position).expect("span start overflow");
        let end = usize::try_from(span.hi.position).expect("span end overflow");

        let clamped_start = start.min(self.contents.len());
        let clamped_end = end.min(self.contents.len());
        if clamped_start >= clamped_end {
            return "";
        }

        let char_start = self.find_char_boundary(clamped_start);
        let char_end = self.find_char_boundary(clamped_end);

        self.contents.get(char_start..char_end).unwrap_or("")
    }

    /// Find nearest UTF-8 character boundary at or after given position
    fn find_char_boundary(&self, mut pos: usize) -> usize {
        if pos >= self.contents.len() {
            return self.contents.len();
        }

        if self.contents.is_char_boundary(pos) {
            return pos;
        }

        while pos < self.contents.len() && !self.contents.is_char_boundary(pos) {
            pos += 1;
        }

        pos
    }
}

#[cfg(test)]
mod tests;
