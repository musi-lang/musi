/// Range of source code.
#[derive(Clone, Copy, Debug, PartialEq, Eq, Hash)]
pub struct Span {
    /// Start offset (inclusive).
    pub lo: u32,
    /// End offset (exclusive).
    pub hi: u32,
}

impl Span {
    /// Creates new span.
    #[must_use]
    pub const fn new(lo: u32, hi: u32) -> Self {
        Self { lo, hi }
    }

    /// Empty span used as placeholder.
    pub const DUMMY: Self = Self::new(0, 0);

    /// Returns length of span.
    #[must_use]
    pub const fn len(&self) -> u32 {
        self.hi - self.lo
    }

    /// Returns `true` if span is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.lo >= self.hi
    }

    /// Merges two spans into one covering both.
    #[must_use]
    pub fn merge(self, other: Self) -> Self {
        Self {
            lo: self.lo.min(other.lo),
            hi: self.hi.max(other.hi),
        }
    }
}

impl Default for Span {
    fn default() -> Self {
        Self::DUMMY
    }
}
