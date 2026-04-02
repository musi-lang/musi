mod cursor;
mod errors;
mod lexer;
mod token;
mod trivia;

pub use errors::{LexError, LexErrorKind};
pub use lexer::Lexer;
pub use token::{Token, TokenKind};
pub use trivia::{Trivia, TriviaKind};

use trivia::TriviaRange;

#[derive(Debug, Default)]
pub struct LexedSource {
    pub(crate) tokens: Vec<Token>,
    pub(crate) trivia: Vec<Trivia>,
    pub(crate) token_trivia: Vec<TriviaRange>,
    pub(crate) errors: Vec<LexError>,
}

impl LexedSource {
    #[must_use]
    pub fn tokens(&self) -> &[Token] {
        &self.tokens
    }

    #[must_use]
    pub fn trivia(&self) -> &[Trivia] {
        &self.trivia
    }

    #[must_use]
    pub fn errors(&self) -> &[LexError] {
        &self.errors
    }

    #[must_use]
    pub fn token_trivia(&self, token_index: usize) -> &[Trivia] {
        let trivia_range = self
            .token_trivia
            .get(token_index)
            .copied()
            .unwrap_or(TriviaRange::EMPTY);
        self.trivia.get(trivia_range.as_range()).unwrap_or_default()
    }
}
