use core::mem;

use music_ast::common::Attr;
use music_ast::data::AstData;
use music_ast::expr::ExprKind;
use music_ast::pat::PatKind;
use music_ast::ty::TyKind;
use music_ast::{AttrId, ExprId, PatId, TyId};
use music_found::{Ident, Interner, Span, Spanned, Symbol};
use music_lex::{Token, TokenKind};

use crate::errors::{ParseError, ParseErrorKind, ParseResult, describe_token};

/// Internal parser state. Only the public `parse()` function is exposed.
pub struct Parser<'tok> {
    pub tokens: &'tok [Token],
    pub source: &'tok str,
    pub pos: usize,
    pub ast: AstData,
    pub interner: &'tok mut Interner,
    pub errors: Vec<ParseError>,
}

impl<'tok> Parser<'tok> {
    pub const fn new(
        tokens: &'tok [Token],
        source: &'tok str,
        interner: &'tok mut Interner,
    ) -> Self {
        Self {
            tokens,
            source,
            pos: 0,
            ast: AstData::new(),
            interner,
            errors: Vec::new(),
        }
    }

    pub fn finish(self) -> (AstData, Vec<ParseError>) {
        (self.ast, self.errors)
    }

    // ── Token navigation ──────────────────────────────────────────

    pub fn peek(&self) -> &Token {
        self.tokens
            .get(self.pos)
            .unwrap_or_else(|| self.tokens.last().expect("token stream has Eof"))
    }

    pub fn peek_kind(&self) -> &TokenKind {
        &self.peek().kind
    }

    pub fn advance(&mut self) -> &Token {
        let idx = self.pos;
        if !self.at_eof() {
            self.pos += 1;
        }
        self.tokens
            .get(idx)
            .unwrap_or_else(|| self.tokens.last().expect("token stream has Eof"))
    }

    pub fn eat(&mut self, kind: &TokenKind) -> bool {
        if self.at(kind) {
            let _ = self.advance();
            true
        } else {
            false
        }
    }

    pub fn expect(&mut self, kind: &TokenKind, expected: &'static str) -> ParseResult<Span> {
        if self.at(kind) {
            Ok(self.advance().span)
        } else if self.at_eof() {
            Err(ParseError {
                kind: ParseErrorKind::UnexpectedEof { expected },
                span: self.span(),
                context: None,
            })
        } else {
            Err(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected,
                    found: describe_token(self.peek_kind()),
                },
                span: self.span(),
                context: None,
            })
        }
    }

    pub fn expect_closing(
        &mut self,
        close: &TokenKind,
        open_label: &'static str,
        open_span: Span,
        ctx: &'static str,
    ) -> ParseResult<Span> {
        if self.at(close) {
            Ok(self.advance().span)
        } else if self.at_eof() {
            Err(ParseError {
                kind: ParseErrorKind::UnclosedDelimiter {
                    open: open_label,
                    open_span,
                },
                span: self.span(),
                context: Some(ctx),
            })
        } else {
            Err(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected: describe_token(close),
                    found: describe_token(self.peek_kind()),
                },
                span: self.span(),
                context: Some(ctx),
            })
        }
    }

    pub fn at(&self, kind: &TokenKind) -> bool {
        mem::discriminant(self.peek_kind()) == mem::discriminant(kind)
    }

    pub fn at_eof(&self) -> bool {
        matches!(self.peek_kind(), TokenKind::Eof)
    }

    pub fn span(&self) -> Span {
        self.peek().span
    }

    pub fn prev_span(&self) -> Span {
        if self.pos == 0 {
            return Span::DUMMY;
        }
        self.tokens
            .get(self.pos - 1)
            .map_or(Span::DUMMY, |t| t.span)
    }

    // ── Source text access ─────────────────────────────────────────

    pub fn text_at(&self, span: Span) -> &str {
        let start = usize::try_from(span.start).expect("span start fits usize");
        let end = usize::try_from(span.end).expect("span end fits usize");
        self.source.get(start..end).expect("valid span range")
    }

    pub fn token_text(&self, token: &Token) -> &str {
        self.text_at(token.span)
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        self.interner.intern(s)
    }

    /// Advance over an `Ident` token, intern its text, return an `Ident`.
    pub fn advance_ident(&mut self) -> Ident {
        let span = self.peek().span;
        let name = self.intern_span(span);
        let _ = self.advance();
        Ident::new(name, span)
    }

    /// Advance over an `EscapedIdent` token, trim backticks, intern, return `Ident`.
    pub fn advance_ident_trimmed(&mut self) -> Ident {
        let span = self.peek().span;
        let start = usize::try_from(span.start).expect("span start fits usize") + 1;
        let end = usize::try_from(span.end)
            .expect("span end fits usize")
            .saturating_sub(1);
        let trimmed = self
            .source
            .get(start..end)
            .expect("escaped ident has backticks");
        let name = self.interner.intern(trimmed);
        let _ = self.advance();
        Ident::new(name, span)
    }

    /// Intern the text at `span` and return an `Ident` without advancing.
    pub fn make_ident_from_span(&mut self, span: Span) -> Ident {
        let name = self.intern_span(span);
        Ident::new(name, span)
    }

    fn intern_span(&mut self, span: Span) -> Symbol {
        let start = usize::try_from(span.start).expect("span start fits usize");
        let end = usize::try_from(span.end).expect("span end fits usize");
        let text = self.source.get(start..end).expect("valid span range");
        self.interner.intern(text)
    }

    // ── Alloc helpers ─────────────────────────────────────────────

    pub fn alloc_expr(&mut self, kind: ExprKind, span: Span) -> ExprId {
        self.ast.exprs.alloc(Spanned::new(kind, span))
    }

    pub fn alloc_pat(&mut self, kind: PatKind, span: Span) -> PatId {
        self.ast.pats.alloc(Spanned::new(kind, span))
    }

    pub fn alloc_ty(&mut self, kind: TyKind, span: Span) -> TyId {
        self.ast.types.alloc(Spanned::new(kind, span))
    }

    pub fn alloc_attr(&mut self, attr: Attr, span: Span) -> AttrId {
        self.ast.attrs.alloc(Spanned::new(attr, span))
    }

    pub fn eat_wildcard(&mut self) -> bool {
        if matches!(self.peek_kind(), TokenKind::Ident) && self.token_text(self.peek()) == "_" {
            let _ = self.advance();
            return true;
        }
        false
    }

    // ── Error recovery ────────────────────────────────────────────

    pub fn error(&mut self, err: ParseError) {
        self.errors.push(err);
    }

    pub fn synchronize(&mut self) {
        while !self.at_eof() {
            if self.at(&TokenKind::Semi) {
                let _ = self.advance();
                return;
            }
            if matches!(
                self.peek_kind(),
                TokenKind::RParen | TokenKind::RBracket | TokenKind::RBrace
            ) {
                return;
            }
            let _ = self.advance();
        }
    }

    // ── Root + statement parsing ──────────────────────────────────

    pub fn parse_root(&mut self) {
        while !self.at_eof() {
            let before = self.pos;
            match self.parse_stmt() {
                Ok(expr_id) => self.ast.root.push(expr_id),
                Err(e) => {
                    self.error(e);
                    self.synchronize();
                    if self.pos == before {
                        let _ = self.advance();
                    }
                }
            }
        }
    }

    fn parse_stmt(&mut self) -> ParseResult<ExprId> {
        let expr = self.parse_expr(0)?;
        if let Err(e) = self.expect(&TokenKind::Semi, "';'") {
            self.error(e);
        }
        Ok(expr)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
