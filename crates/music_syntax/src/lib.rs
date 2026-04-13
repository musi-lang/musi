mod cursor;
mod errors;
mod intern;
mod lexer;
mod parser;
pub mod string_lit;
mod token;
mod tree;
mod trivia;

pub use errors::{
    LexError, LexErrorKind, LexErrorList, ParseError, ParseErrorKind, ParseErrorList, ParseResult,
};
pub use intern::canonical_name_text;
pub use lexer::Lexer;
pub use parser::{ParsedSource, parse};
pub use token::{Token, TokenKind};
pub use tree::{
    Program, SyntaxElement, SyntaxNode, SyntaxNodeKind, SyntaxToken, SyntaxTree,
    pattern_binder_tokens,
};
pub use trivia::{Trivia, TriviaKind, TriviaList};

use music_base::Span;
use trivia::TriviaRange;

#[derive(Debug, Clone, Default)]
pub struct LexedSource {
    pub(crate) text: Box<str>,
    pub(crate) tokens: Vec<Token>,
    pub(crate) trivia: TriviaList,
    pub(crate) token_trivia: Vec<TriviaRange>,
    pub(crate) errors: LexErrorList,
}

impl LexedSource {
    #[must_use]
    pub fn new(
        text: impl Into<Box<str>>,
        tokens: Vec<Token>,
        trivia: TriviaList,
        token_trivia: Vec<TriviaRange>,
        errors: LexErrorList,
    ) -> Self {
        Self {
            text: text.into(),
            tokens,
            trivia,
            token_trivia,
            errors,
        }
    }

    #[must_use]
    pub fn text(&self) -> &str {
        &self.text
    }

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
        let trivia_range = self.token_trivia_range(token_index);
        self.trivia.get(trivia_range.as_range()).unwrap_or_default()
    }

    #[must_use]
    pub fn token_trivia_range(&self, token_index: usize) -> TriviaRange {
        self.token_trivia
            .get(token_index)
            .copied()
            .unwrap_or(TriviaRange::EMPTY)
    }

    #[must_use]
    pub fn token_text(&self, token_index: usize) -> Option<&str> {
        let token = self.tokens.get(token_index)?;
        self.slice_span(token.span)
    }

    #[must_use]
    pub fn slice_span(&self, span: Span) -> Option<&str> {
        let start = usize::try_from(span.start).ok()?;
        let end = usize::try_from(span.end).ok()?;
        self.text.get(start..end)
    }
}
