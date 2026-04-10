use music_base::Span;
use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriviaKind {
    Whitespace,
    Newline,
    LineComment { doc: bool },
    BlockComment { doc: bool },
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Trivia {
    pub kind: TriviaKind,
    pub span: Span,
}

pub type TriviaList = Vec<Trivia>;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct TriviaRange {
    pub start: usize,
    pub end: usize,
}

impl TriviaRange {
    pub const EMPTY: Self = Self { start: 0, end: 0 };

    #[must_use]
    pub const fn new(start: usize, end: usize) -> Self {
        Self { start, end }
    }

    #[must_use]
    pub const fn as_range(self) -> Range<usize> {
        self.start..self.end
    }
}

impl Trivia {
    #[must_use]
    pub const fn new(kind: TriviaKind, span: Span) -> Self {
        Self { kind, span }
    }
}
