//! Source location descriptors.

/// A contiguous byte range within a source file.
///
/// `start` is the byte offset of the first byte; `length` is the byte count of the region.
/// All span operations are byte-based, matching the lexer's byte-offset model.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct Span {
    /// Byte offset of the first byte of this span.
    pub start: u32,
    /// Number of bytes in this span.
    pub length: u32,
}

impl Span {
    /// Sentinel for synthetic or compiler-generated AST nodes that have no source location.
    pub const DUMMY: Self = Self { start: 0, length: 0 };

    /// Creates a span from a byte offset and byte length.
    #[must_use]
    pub const fn new(start: u32, length: u32) -> Self {
        Self { start, length }
    }

    /// One past the last byte of this span (exclusive end).
    #[must_use]
    pub const fn end(self) -> u32 {
        self.start + self.length
    }

    /// The smallest span that covers both `self` and `other`.
    ///
    /// Equivalent to the union of the two byte ranges.
    #[must_use]
    pub fn merge(self, other: Self) -> Self {
        let start = self.start.min(other.start);
        let end = self.end().max(other.end());
        Self {
            start,
            length: end - start,
        }
    }
}

#[cfg(test)]
mod tests;
