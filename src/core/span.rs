#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Span {
    pub start: usize,
    pub end: usize,
}

impl Default for Span {
    #[inline]
    fn default() -> Self {
        Self { start: 0, end: 0 }
    }
}
