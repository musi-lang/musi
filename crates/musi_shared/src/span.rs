//! Source location descriptors.

/// A contiguous byte range within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord, Default)]
pub struct Span {
    pub start: u32,
    pub length: u32,
}

impl Span {
    pub const DUMMY: Self = Self {
        start: 0,
        length: 0,
    };

    #[must_use]
    pub const fn new(start: u32, length: u32) -> Self {
        Self { start, length }
    }

    #[must_use]
    pub const fn end(self) -> u32 {
        self.start + self.length
    }

    /// The smallest span covering both `self` and `other`.
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
