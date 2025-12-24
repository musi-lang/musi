use musi_basic::{
    diagnostic::{Diagnostic, DiagnosticBag},
    error::{IntoMusiError, MusiError},
    span::Span,
};
use musi_lex::token::{Token, TokenKind};

use crate::error::ParseErrorKind;

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    pub diagnostics: DiagnosticBag,
}

impl<'a> Parser<'a> {
    #[must_use]
    pub fn new(tokens: &'a [Token]) -> Self {
        Self {
            tokens,
            pos: 0,
            diagnostics: DiagnosticBag::default(),
        }
    }

    #[must_use]
    pub fn peek(&self) -> Option<&Token> {
        self.tokens.get(self.pos)
    }

    #[must_use]
    pub fn peek_kind(&self) -> Option<TokenKind> {
        self.peek().map(|t| t.kind)
    }

    #[must_use]
    pub fn peek_nth(&self, n: usize) -> Option<TokenKind> {
        self.tokens.get(self.pos + n).map(|t| t.kind)
    }

    pub fn advance(&mut self) -> Option<&Token> {
        let tok = self.tokens.get(self.pos);
        if tok.is_some() {
            self.pos += 1;
        }
        tok
    }

    pub fn advance_by(&mut self, n: usize) {
        self.pos = (self.pos + n).min(self.tokens.len());
    }

    #[must_use]
    pub fn at(&self, kind: TokenKind) -> bool {
        self.peek_kind() == Some(kind)
    }

    #[must_use]
    pub fn at_any(&self, kinds: &[TokenKind]) -> bool {
        self.peek_kind().is_some_and(|k| kinds.contains(&k))
    }

    pub fn eat(&mut self, kind: TokenKind) -> bool {
        matches!(self.peek_kind(), Some(k) if k == kind)
    }

    #[must_use]
    pub const fn is_eof(&self) -> bool {
        self.pos >= self.tokens.len()
    }

    #[must_use]
    pub const fn prev_span(&self) -> Span {
        if self.pos > 0 {
            self.tokens[self.pos - 1].span
        } else {
            Span::new(0, 0)
        }
    }

    #[must_use]
    pub fn curr_span(&self) -> Span {
        self.peek().map_or_else(|| self.prev_span(), |t| t.span)
    }

    /// # Errors
    /// Returns `ParseErrorKind::Expected` if current token does not match.
    ///
    /// # Panics
    /// Never panics if `at(kind)` is true.
    pub fn expect(&mut self, kind: TokenKind) -> Result<&Token, MusiError> {
        if self.at(kind) {
            Ok(self.advance().expect("checked by `at()`"))
        } else {
            let err = ParseErrorKind::Expected(kind.as_str());
            Err(err.into_musi_error(self.curr_span()))
        }
    }

    pub fn report(&mut self, err: MusiError) {
        self.diagnostics.add(Diagnostic::from(err));
    }

    pub fn sync(&mut self) {
        loop {
            match self.peek_kind() {
                None | Some(TokenKind::RBrace) => break,
                Some(TokenKind::Semicolon) => {
                    _ = self.advance();
                    break;
                }
                _ => {
                    _ = self.advance();
                }
            }
        }
    }

    pub fn sync_to_stmt(&mut self) {
        loop {
            match self.peek_kind() {
                None
                | Some(
                    TokenKind::RBrace
                    | TokenKind::KwVal
                    | TokenKind::KwVar
                    | TokenKind::KwFn
                    | TokenKind::KwRecord
                    | TokenKind::KwSum
                    | TokenKind::KwIf
                    | TokenKind::KwWhile
                    | TokenKind::KwFor,
                ) => break,
                Some(TokenKind::Semicolon) => {
                    _ = self.advance();
                    break;
                }
                _ => {
                    _ = self.advance();
                }
            }
        }
    }

    pub fn sync_to_block_end(&mut self) {
        let mut depth = 1;
        loop {
            match self.peek_kind() {
                None => break,
                Some(TokenKind::LBrace) => {
                    depth += 1;
                    _ = self.advance();
                }
                Some(TokenKind::RBrace) => {
                    depth -= 1;
                    if depth == 0 {
                        break;
                    }
                    _ = self.advance();
                }
                _ => {
                    _ = self.advance();
                }
            }
        }
    }
}
