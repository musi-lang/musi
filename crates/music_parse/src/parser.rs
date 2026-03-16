//! Parser state and combinators.

#[cfg(test)]
mod tests;

use core::mem;

use music_ast::expr::Expr;
use music_ast::pat::Pat;
use music_ast::ty::Ty;
use music_ast::{AstArenas, ExprIdx, ParsedModule, PatIdx, Stmt, TyIdx};
use music_lex::token::{Token, TokenKind};
use music_shared::{DiagnosticBag, FileId, Interner, Span, Symbol};

use crate::can_start_expr;
use crate::error::ParseError;

pub struct Parser<'a> {
    tokens: &'a [Token],
    pos: usize,
    pub(crate) file_id: FileId,
    pub(crate) diags: &'a mut DiagnosticBag,
    pub(crate) interner: &'a mut Interner,
    pub(crate) arenas: AstArenas,
}

impl<'a> Parser<'a> {
    pub(crate) fn new(
        tokens: &'a [Token],
        file_id: FileId,
        diags: &'a mut DiagnosticBag,
        interner: &'a mut Interner,
    ) -> Self {
        Self {
            tokens,
            pos: 0,
            file_id,
            diags,
            interner,
            arenas: AstArenas::new(),
        }
    }

    #[must_use]
    pub(crate) fn peek(&self) -> &Token {
        &self.tokens[self.pos]
    }

    #[must_use]
    pub(crate) fn peek_kind(&self) -> TokenKind {
        self.peek().kind
    }

    #[must_use]
    pub(crate) fn peek2(&self) -> TokenKind {
        if self.pos + 1 < self.tokens.len() {
            self.tokens[self.pos + 1].kind
        } else {
            TokenKind::Eof
        }
    }

    pub(crate) fn at(&self, kind: TokenKind) -> bool {
        self.peek_kind() == kind
    }

    pub(crate) fn bump(&mut self) -> &Token {
        let tok = &self.tokens[self.pos];
        if tok.kind != TokenKind::Eof {
            self.pos += 1;
        }
        tok
    }

    pub(crate) fn eat(&mut self, kind: TokenKind) -> bool {
        self.at(kind).then(|| self.bump()).is_some()
    }

    pub(crate) fn expect(&mut self, kind: TokenKind) -> Span {
        if self.at(kind) {
            self.bump().span
        } else {
            let span = self.peek().span;
            let err = ParseError::expected_token(kind, self.peek_kind());
            let _diag = self.diags.report(&err, span, self.file_id);
            span
        }
    }

    pub(crate) fn expect_symbol(&mut self) -> Symbol {
        let tok = self.bump();
        let span = tok.span;
        if let Some(sym) = tok.symbol {
            return sym;
        }
        if tok.kind != TokenKind::Underscore {
            let _diag = self
                .diags
                .report(&ParseError::ExpectedIdent, span, self.file_id);
        }
        Symbol(u32::MAX)
    }

    pub(crate) fn start_span(&self) -> u32 {
        self.peek().span.start
    }

    #[must_use]
    pub(crate) fn finish_span(&self, start: u32) -> Span {
        let prev = if self.pos > 0 {
            &self.tokens[self.pos - 1]
        } else {
            self.peek()
        };
        let end = prev.span.end();
        if end >= start {
            Span::new(start, end - start)
        } else {
            Span::new(start, 0)
        }
    }

    pub(crate) fn recover_to(&mut self, sync: &[TokenKind]) {
        loop {
            let k = self.peek_kind();
            if sync.contains(&k) || k == TokenKind::Eof {
                break;
            }
            match k {
                TokenKind::Semi | TokenKind::RParen | TokenKind::RBrace | TokenKind::RBracket => {
                    break;
                }
                _ => {
                    let _tok = self.bump();
                }
            }
        }
    }

    pub(crate) fn alloc_expr(&mut self, e: Expr) -> ExprIdx {
        self.arenas.exprs.alloc(e)
    }

    pub(crate) fn alloc_ty(&mut self, t: Ty) -> TyIdx {
        self.arenas.tys.alloc(t)
    }

    pub(crate) fn alloc_pat(&mut self, p: Pat) -> PatIdx {
        self.arenas.pats.alloc(p)
    }

    pub(crate) fn error_expr(&mut self, err: &ParseError) -> Expr {
        let span = self.peek().span;
        let _diag = self.diags.report(err, span, self.file_id);
        self.recover_to(&[]);
        Expr::Error {
            span: self.finish_span(span.start),
        }
    }

    pub(crate) fn error_ty(&mut self, err: &ParseError) -> Ty {
        let span = self.peek().span;
        let _diag = self.diags.report(err, span, self.file_id);
        Ty::Error {
            span: self.finish_span(span.start),
        }
    }

    pub(crate) fn error_pat(&mut self, err: &ParseError) -> Pat {
        let span = self.peek().span;
        let _diag = self.diags.report(err, span, self.file_id);
        self.recover_to(&[]);
        Pat::Error {
            span: self.finish_span(span.start),
        }
    }

    pub(crate) fn parse_alloc_expr(&mut self) -> ExprIdx {
        let e = self.parse_expr();
        self.alloc_expr(e)
    }

    pub(crate) fn parse_alloc_expr_no_in(&mut self) -> ExprIdx {
        let e = self.parse_expr_no_in();
        self.alloc_expr(e)
    }

    pub(crate) fn parse_alloc_ty(&mut self) -> TyIdx {
        let t = self.parse_ty();
        self.alloc_ty(t)
    }

    pub(crate) fn parse_alloc_pat(&mut self) -> PatIdx {
        let p = self.parse_pat();
        self.alloc_pat(p)
    }

    pub(crate) fn parse_opt_expr(&mut self) -> Option<ExprIdx> {
        if can_start_expr(self.peek_kind()) {
            Some(self.parse_alloc_expr())
        } else {
            None
        }
    }

    pub(crate) fn resolve(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym)
    }

    /// Get display text for the current token: resolved symbol if available, else kind display.
    pub(crate) fn peek_text(&self) -> Box<str> {
        let tok = self.peek();
        if let Some(sym) = tok.symbol {
            return self.interner.resolve(sym).into();
        }
        tok.kind.to_string().into()
    }

    pub(crate) fn sep_by<T>(
        &mut self,
        sep: TokenKind,
        close: TokenKind,
        mut f: impl FnMut(&mut Self) -> T,
    ) -> Vec<T> {
        let mut items = vec![];
        if self.at(close) {
            return items;
        }
        loop {
            items.push(f(self));
            if !self.eat(sep) {
                break;
            }
            if self.at(close) {
                break;
            }
        }
        items
    }

    pub(crate) fn comma_sep<T>(
        &mut self,
        close: TokenKind,
        f: impl FnMut(&mut Self) -> T,
    ) -> Vec<T> {
        self.sep_by(TokenKind::Comma, close, f)
    }

    pub(crate) fn pipe_sep<T>(
        &mut self,
        close: TokenKind,
        f: impl FnMut(&mut Self) -> T,
    ) -> Vec<T> {
        self.sep_by(TokenKind::Pipe, close, f)
    }

    pub(crate) fn delimited<T>(
        &mut self,
        open: TokenKind,
        close: TokenKind,
        f: impl FnMut(&mut Self) -> T,
    ) -> Vec<T> {
        let _open = self.expect(open);
        let items = self.comma_sep(close, f);
        let _close = self.expect(close);
        items
    }

    pub(crate) fn parse_program(&mut self) -> ParsedModule {
        let start = self.start_span();
        let mut stmts = vec![];
        while !self.at(TokenKind::Eof) {
            let pos_before = self.pos;
            let stmt_start = self.start_span();
            let expr_idx = self.parse_alloc_expr();
            let _semi = self.expect(TokenKind::Semi);
            let stmt_span = self.finish_span(stmt_start);
            stmts.push(Stmt {
                expr: expr_idx,
                span: stmt_span,
            });

            if self.pos == pos_before {
                let span = self.peek().span;
                let text = self.peek_text();
                let err = ParseError::unexpected_kind(self.peek_kind(), text);
                let _d = self.diags.report(&err, span, self.file_id);
                let _tok = self.bump();
            }
        }
        let span = self.finish_span(start);
        let arenas = mem::take(&mut self.arenas);
        ParsedModule {
            stmts,
            arenas,
            span,
        }
    }
}
