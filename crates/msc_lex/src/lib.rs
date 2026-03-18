//! Lexer for the Musi compiler.
//!
//! Converts a source string into a flat stream of [`Token`]s via the [`lex`]
//! free function, which returns a [`LexedSource`] containing both tokens and
//! attached trivia (whitespace, newlines, comments).

mod error;
mod lexer;
pub mod token;
mod trivia;

pub use token::{Token, TokenKind};
pub use trivia::{Trivia, TriviaKind, TriviaRange};

use msc_shared::{DiagnosticBag, FileId, Interner};

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
    let mut lexer = lexer::Lexer::new(source, file_id, interner, diags);
    let tokens: Vec<Token> = lexer.by_ref().collect();
    LexedSource {
        tokens,
        trivia: lexer.trivia,
    }
}
