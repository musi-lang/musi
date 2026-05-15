use std::fmt;
use std::iter;
use std::path::{Path, PathBuf};

use thiserror::Error;

use crate::Span;

/// Opaque identifier for a source file within a [`SourceMap`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SourceId(u32);

impl SourceId {
    /// Construct a `SourceId` from a raw `u32`.
    #[must_use]
    pub const fn from_raw(raw: u32) -> Self {
        Self(raw)
    }

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

/// Errors produced by [`SourceMap::add`].
#[derive(Debug, Error)]
pub enum SourceMapError {
    #[error("source map overflow")]
    Overflow,

    #[error("source text too large ({len} bytes)")]
    SourceTooLarge { len: usize },
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
        let line_starts = iter::once(0)
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
        let line_start = self.line_starts[line_index];
        let start_idx = usize::try_from(line_start).unwrap_or(0);
        let end_idx = usize::try_from(offset).unwrap_or(self.text.len());
        let col = self.text.get(start_idx..end_idx).map_or_else(
            || usize::try_from(offset.saturating_sub(line_start)).unwrap_or(0),
            |line_prefix| line_prefix.chars().count(),
        );
        (line_index + 1, col + 1)
    }

    /// Convert a 1-based (line, column) pair to a byte offset.
    #[must_use]
    pub fn offset(&self, line: usize, column: usize) -> Option<u32> {
        if line == 0 || column == 0 || line > self.line_starts.len() {
            return None;
        }
        let line_start = *self.line_starts.get(line - 1)?;
        let line_text = self.line_text(line)?;
        let max_column = line_text.chars().count().saturating_add(1);
        if column > max_column {
            return None;
        }
        let char_offset = line_text
            .chars()
            .take(column.saturating_sub(1))
            .map(char::len_utf8)
            .sum::<usize>();
        let byte_offset = line_start.saturating_add(u32::try_from(char_offset).ok()?);
        Some(byte_offset)
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
    pub const fn new() -> Self {
        Self {
            sources: Vec::new(),
        }
    }

    /// Register a source file and return its id.
    ///
    /// # Errors
    ///
    /// Returns `Err` if the source registry overflows `u32` indexing or if the
    /// source text is too large to address with `Span` offsets.
    pub fn add(
        &mut self,
        path: impl Into<PathBuf>,
        text: impl Into<String>,
    ) -> Result<SourceId, SourceMapError> {
        let id_raw = u32::try_from(self.sources.len()).map_err(|_| SourceMapError::Overflow)?;
        let text: String = text.into();
        let max_span_len = usize::try_from(u32::MAX).unwrap_or(usize::MAX);
        if text.len() > max_span_len {
            return Err(SourceMapError::SourceTooLarge { len: text.len() });
        }
        let id = SourceId(id_raw);
        self.sources.push(Source::new(id, path.into(), text));
        Ok(id)
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

    #[must_use]
    pub const fn len(&self) -> usize {
        self.sources.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.sources.is_empty()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
