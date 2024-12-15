#[derive(Clone, Copy, Debug)]
#[non_exhaustive]
pub struct Span {
    pub start: usize,
    pub end: usize,
}
