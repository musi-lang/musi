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

impl std::fmt::Display for Location {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}:{}", self.line, self.column)
    }
}
