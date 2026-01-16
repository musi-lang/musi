use super::SourcePosition;

/// Source code span between two byte offsets
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct SourceSpan {
    /// Start offset (inclusive)
    pub lo: SourcePosition,
    /// End offset (exclusive)
    pub hi: SourcePosition,
}

impl SourceSpan {
    /// Create new source range from start and end offsets
    #[must_use]
    pub const fn new(lo: SourcePosition, hi: SourcePosition) -> Self {
        Self { lo, hi }
    }

    /// Create dummy source range at position `0`
    #[must_use]
    pub const fn dummy() -> Self {
        Self::new(SourcePosition::new(0), SourcePosition::new(0))
    }

    /// Check if this is dummy range (both offsets at `0`)
    #[must_use]
    pub const fn is_dummy(self) -> bool {
        self.lo.position == 0 && self.hi.position == 0
    }

    /// Check if offset falls within range (inclusive)
    #[must_use]
    pub const fn contains(self, offset: SourcePosition) -> bool {
        offset.position >= self.lo.position && offset.position <= self.hi.position
    }

    /// Check if two ranges overlap
    #[must_use]
    pub const fn overlaps(self, other: Self) -> bool {
        self.lo.position <= other.hi.position && other.lo.position <= self.hi.position
    }
}

#[cfg(test)]
mod tests;
