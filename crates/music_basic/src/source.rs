use std::fmt;
use std::iter;
use std::path::{Path, PathBuf};

use crate::Span;

/// Opaque identifier for a source file within a [`SourceMap`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(u32);

impl SourceId {
    /// Return the raw `u32` index.
    #[must_use]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

impl fmt::Display for SourceId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A single source file with precomputed line-start offsets.
#[derive(Debug)]
pub struct Source {
    id: SourceId,
    path: PathBuf,
    text: String,
    line_starts: Vec<u32>,
}

impl Source {
    /// Create a new source, scanning the text for line boundaries.
    fn new(id: SourceId, path: PathBuf, text: String) -> Self {
        let line_starts: Vec<u32> = iter::once(0)
            .chain(
                text.bytes()
                    .enumerate()
                    .filter(|&(_, b)| b == b'\n')
                    .filter_map(|(i, _)| u32::try_from(i + 1).ok()),
            )
            .collect();
        Self {
            id,
            path,
            text,
            line_starts,
        }
    }

    /// Source identifier.
    #[must_use]
    pub const fn id(&self) -> SourceId {
        self.id
    }

    /// File path.
    #[must_use]
    pub fn path(&self) -> &Path {
        &self.path
    }

    /// Full source text.
    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

    /// The span covering the entire source text.
    #[must_use]
    pub fn span(&self) -> Span {
        let len = u32::try_from(self.text.len()).unwrap_or(u32::MAX);
        Span::new(0, len)
    }

    /// Convert a byte offset to a 1-based (line, column) pair.
    ///
    /// Uses binary search over the precomputed line-start table.
    #[must_use]
    pub fn line_col(&self, offset: u32) -> (usize, usize) {
        let text_len = u32::try_from(self.text.len()).unwrap_or(u32::MAX);
        let offset = offset.min(text_len);
        let line_index = match self.line_starts.binary_search(&offset) {
            Ok(exact) => exact,
            Err(insert) => insert.saturating_sub(1),
        };
        let col = offset.saturating_sub(self.line_starts[line_index]);
        let col_usize = usize::try_from(col).unwrap_or(0);
        (line_index + 1, col_usize + 1)
    }

    /// Return the text of a 1-based line number, without the trailing newline.
    #[must_use]
    pub fn line_text(&self, line: usize) -> Option<&str> {
        if line == 0 || line > self.line_starts.len() {
            return None;
        }
        let start_idx = usize::try_from(self.line_starts[line - 1]).unwrap_or(0);
        let end_idx = if line < self.line_starts.len() {
            let raw_end = usize::try_from(self.line_starts[line]).unwrap_or(self.text.len());
            raw_end.saturating_sub(1)
        } else {
            self.text.len()
        };
        self.text
            .get(start_idx..end_idx)
            .map(|s| s.strip_suffix('\r').unwrap_or(s))
    }

    /// Total number of lines.
    #[must_use]
    pub const fn line_count(&self) -> usize {
        self.line_starts.len()
    }
}

/// Registry of source files, indexed by [`SourceId`].
#[derive(Debug, Default)]
pub struct SourceMap {
    sources: Vec<Source>,
}

impl SourceMap {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Register a source file and return its id.
    ///
    /// # Panics
    ///
    /// Panics if more than `u32::MAX` sources are registered.
    pub fn add(&mut self, path: impl Into<PathBuf>, text: impl Into<String>) -> SourceId {
        let id_raw = u32::try_from(self.sources.len()).expect("source map overflow");
        let id = SourceId(id_raw);
        self.sources.push(Source::new(id, path.into(), text.into()));
        id
    }

    /// Look up a source by its id.
    #[must_use]
    pub fn get(&self, id: SourceId) -> Option<&Source> {
        let idx = usize::try_from(id.0).unwrap_or(usize::MAX);
        self.sources.get(idx)
    }

    /// Iterate over all registered sources.
    pub fn iter(&self) -> impl Iterator<Item = &Source> {
        self.sources.iter()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
