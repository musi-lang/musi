mod cursor;
mod errors;
mod intern;
mod lexer;
mod parser;
mod token;
mod tree;
mod trivia;

pub use errors::{LexError, LexErrorKind, ParseError, ParseErrorKind, ParseResult};
pub use intern::{canonical_name_text, intern_name_token};
pub use lexer::Lexer;
pub use parser::{ParsedSource, parse};
pub use token::{Token, TokenKind};
pub use tree::{
    Program, SyntaxElement, SyntaxElementId, SyntaxNode, SyntaxNodeData, SyntaxNodeId,
    SyntaxNodeKind, SyntaxToken, SyntaxTokenId, SyntaxTree,
};
pub use trivia::{Trivia, TriviaKind};

use music_base::Span;
use trivia::TriviaRange;

#[derive(Debug, Clone, Default)]
pub struct LexedSource<'src> {
    pub(crate) text: &'src str,
    pub(crate) tokens: Vec<Token>,
    pub(crate) trivia: Vec<Trivia>,
    pub(crate) token_trivia: Vec<TriviaRange>,
    pub(crate) errors: Vec<LexError>,
}

impl<'src> LexedSource<'src> {
    #[must_use]
    pub const fn text(&self) -> &'src str {
        self.text
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
    pub fn token_text(&self, token_index: usize) -> Option<&'src str> {
        let token = self.tokens.get(token_index)?;
        self.slice_span(token.span)
    }

    #[must_use]
    pub fn slice_span(&self, span: Span) -> Option<&'src str> {
        let start = usize::try_from(span.start).ok()?;
        let end = usize::try_from(span.end).ok()?;
        self.text.get(start..end)
    }
}
