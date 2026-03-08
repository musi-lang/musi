//! Trivia attached to tokens: whitespace, newlines, and comments.

use musi_shared::Span;

#[derive(Debug, Clone)]
pub struct Trivia {
    pub kind: TriviaKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum TriviaKind {
    Whitespace,
    Newline,
    /// `doc_style` is true for `///` comments, false for `//`.
    LineComment {
        doc_style: bool,
    },
    /// `doc_style` is true for `/** */` comments, false for `/* */`.
    BlockComment {
        doc_style: bool,
    },
}

/// Index range into `LexedSource::trivia`.
#[derive(Clone, Copy, Debug, Default)]
pub struct TriviaRange {
    pub start: u32,
    pub len: u16,
}
