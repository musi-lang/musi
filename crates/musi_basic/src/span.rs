#[derive(Clone, Copy, Debug, PartialEq, Eq, Default)]
pub struct Span {
    pub lo: u32,
    pub hi: u32,
}

impl Span {
    #[must_use]
    pub const fn new(lo: u32, hi: u32) -> Self {
        Self { lo, hi }
    }

    #[must_use]
    pub const fn len(&self) -> u32 {
        self.hi - self.lo
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.lo >= self.hi
    }

    #[must_use]
    pub fn merge(self, other: Self) -> Self {
        Self {
            lo: self.lo.min(other.lo),
            hi: self.hi.max(other.hi),
        }
    }
}
