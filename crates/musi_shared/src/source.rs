//! File registry with line/column lookup.

use core::fmt;

/// Identifies a source file within a [`SourceDb`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct FileId(pub u32);

/// A single source file: its identity, name, contents, and line-start index.
pub struct SourceFile {
    /// Unique identifier for this file within the [`SourceDb`].
    pub id: FileId,
    /// Display name (typically the file path).
    pub name: Box<str>,
    /// Full source text.
    pub source: Box<str>,
    /// Byte offsets of each line start. `line_starts[0]` is always 0.
    line_starts: Vec<u32>,
}

impl SourceFile {
    /// Creates a new `SourceFile`, computing line-start offsets from `source`.
    fn new(id: FileId, name: String, source: String) -> Self {
        let line_starts = compute_line_starts(&source);
        Self {
            id,
            name: name.into_boxed_str(),
            source: source.into_boxed_str(),
            line_starts,
        }
    }
}

impl fmt::Debug for SourceFile {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("SourceFile")
            .field("id", &self.id)
            .field("name", &self.name)
            .field("source", &self.source)
            .field("line_starts", &self.line_starts)
            .finish()
    }
}

/// Registry of all source files loaded in a compilation session.
#[derive(Debug, Default)]
pub struct SourceDb {
    files: Vec<SourceFile>,
}

impl SourceDb {
    /// Creates an empty `SourceDb`.
    #[must_use]
    pub const fn new() -> Self {
        Self { files: Vec::new() }
    }

    /// Adds a file to the database, returning its [`FileId`].
    ///
    /// # Panics
    ///
    /// Panics if the number of files exceeds `u32::MAX`.
    pub fn add(&mut self, name: impl Into<String>, source: impl Into<String>) -> FileId {
        let id = FileId(u32::try_from(self.files.len()).expect("file count fits in u32"));
        let file = SourceFile::new(id, name.into(), source.into());
        self.files.push(file);
        id
    }

    /// Returns the full source text for the given file.
    #[must_use]
    pub fn source(&self, file_id: FileId) -> &str {
        &self.file(file_id).source
    }

    /// Returns the display name for the given file.
    #[must_use]
    pub fn name(&self, file_id: FileId) -> &str {
        &self.file(file_id).name
    }

    /// Resolves a byte offset to a 1-based `(line, column)` pair.
    ///
    /// # Panics
    ///
    /// Panics if `file_id` is out of range or `byte_offset` exceeds the source length.
    #[must_use]
    pub fn lookup(&self, file_id: FileId, byte_offset: u32) -> (u32, u32) {
        let file = self.file(file_id);
        let line_idx = match file.line_starts.binary_search(&byte_offset) {
            Ok(idx) => idx,
            Err(idx) => idx.saturating_sub(1),
        };
        let line = u32::try_from(line_idx).expect("line index fits in u32") + 1;
        let col = byte_offset - file.line_starts[line_idx] + 1;
        (line, col)
    }

    /// Returns the text of a 1-based `line` number, without any trailing newline.
    ///
    /// # Panics
    ///
    /// Panics if `line` is zero or beyond the number of lines in the file.
    #[allow(clippy::string_slice)] // line-start offsets are always at char boundaries
    #[must_use]
    pub fn get_line(&self, file_id: FileId, line: u32) -> &str {
        let file = self.file(file_id);
        let line_idx = usize::try_from(line).expect("line number fits in usize") - 1;
        assert!(
            line_idx < file.line_starts.len(),
            "line {line} is out of range (file has {} lines)",
            file.line_starts.len()
        );
        let start =
            usize::try_from(file.line_starts[line_idx]).expect("line start offset fits in usize");
        let raw = if line_idx + 1 < file.line_starts.len() {
            let end = usize::try_from(file.line_starts[line_idx + 1])
                .expect("next line start offset fits in usize");
            &file.source[start..end]
        } else {
            &file.source[start..]
        };
        raw.trim_end_matches('\n').trim_end_matches('\r')
    }

    /// Returns the [`SourceFile`] for a given [`FileId`].
    fn file(&self, file_id: FileId) -> &SourceFile {
        let idx = usize::try_from(file_id.0).expect("file id fits in usize");
        &self.files[idx]
    }
}

/// Computes byte offsets of line starts within `source`.
///
/// The first entry is always 0 (the start of the first line).  Every subsequent
/// entry is the byte offset immediately after a `\n`.
fn compute_line_starts(source: &str) -> Vec<u32> {
    let mut starts = vec![0u32];
    for (i, byte) in source.bytes().enumerate() {
        if byte == b'\n' {
            let offset = u32::try_from(i).expect("source byte offset fits in u32") + 1;
            starts.push(offset);
        }
    }
    starts
}

#[cfg(test)]
mod tests;
