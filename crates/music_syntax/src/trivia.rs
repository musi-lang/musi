use music_base::Span;
use std::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum TriviaKind {
    Whitespace,
    Newline,
    LineComment,
    LineDocComment,
    LineModuleDocComment,
    BlockComment,
    BlockDocComment,
    BlockModuleDocComment,
}

impl TriviaKind {
    #[must_use]
    pub const fn is_comment(self) -> bool {
        matches!(
            self,
            Self::LineComment
                | Self::LineDocComment
                | Self::LineModuleDocComment
                | Self::BlockComment
                | Self::BlockDocComment
                | Self::BlockModuleDocComment
        )
    }

    #[must_use]
    pub const fn is_line_comment(self) -> bool {
        matches!(
            self,
            Self::LineComment | Self::LineDocComment | Self::LineModuleDocComment
        )
    }

    #[must_use]
    pub const fn is_doc_comment(self) -> bool {
        matches!(
            self,
            Self::LineDocComment
                | Self::LineModuleDocComment
                | Self::BlockDocComment
                | Self::BlockModuleDocComment
        )
    }

    #[must_use]
    pub const fn is_module_doc_comment(self) -> bool {
        matches!(
            self,
            Self::LineModuleDocComment | Self::BlockModuleDocComment
        )
    }
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
