#[derive(Clone, Copy, Debug)]
pub struct Span {
    pub start: Location,
    pub end: Location,
}

#[derive(Clone, Copy, Debug)]
pub struct Location {
    pub line: u32,
    pub column: u32,
    pub offset: usize,
}

impl Location {
    pub const fn new() -> Self {
        Self {
            line: 1,
            column: 1,
            offset: 0,
        }
    }
}
