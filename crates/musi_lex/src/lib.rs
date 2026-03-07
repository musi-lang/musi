//! Lexer for the Musi compiler.
//!
//! Converts a source string into a flat stream of [`Token`]s via the [`lex`]
//! free function, which returns a [`LexedSource`] containing both tokens and
//! attached trivia (whitespace, newlines, comments).
//!
//! # Error handling
//!
//! Lexical errors (unknown characters, unterminated strings, etc.) are recorded
//! into the caller-supplied [`DiagnosticBag`] and produce a [`TokenKind::Error`]
//! token so that the rest of the stream remains valid.

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

mod lexer;
pub mod token;
mod trivia;

pub use token::{Token, TokenKind};
pub use trivia::{Trivia, TriviaKind, TriviaRange};

use musi_shared::{DiagnosticBag, FileId, Interner};

/// The result of lexing a single source string.
pub struct LexedSource {
    /// Non-trivia tokens, ending with [`TokenKind::Eof`].
    pub tokens: Vec<Token>,
    /// All trivia in document order. Tokens reference ranges into this vec
    /// via their `leading_trivia` and `trailing_trivia` fields.
    pub trivia: Vec<Trivia>,
}

/// Lexes `source` into a [`LexedSource`].
pub fn lex(
    source: &str,
    file_id: FileId,
    interner: &mut Interner,
    diags: &mut DiagnosticBag,
) -> LexedSource {
    let mut lx = lexer::Lexer::new(source, file_id, interner, diags);
    let tokens: Vec<Token> = lx.by_ref().collect();
    LexedSource {
        tokens,
        trivia: lx.trivia,
    }
}
