//! Parser utility methods: list parsers, block helpers, symbol/literal helpers.

use musi_lex::token::TokenKind;
use musi_shared::{Idx, Slice, Symbol};

use crate::ast::{Attr, BindKind, Expr, LitValue, PostfixOp};

use super::{can_start_expr, parse_char_lit, parse_int_lit, Parser};

impl<'a> Parser<'a> {
    pub(super) fn parse_separated_list<T, F>(&mut self, closing: TokenKind, mut f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let mut items = Vec::new();
        if self.at(closing) {
            return items;
        }
        loop {
            items.push(f(self));
            if !self.eat(TokenKind::Comma) {
                break;
            }
            if self.at(closing) {
                break;
            }
        }
        items
    }

    pub(super) fn parse_delimited<T, F>(&mut self, open: TokenKind, close: TokenKind, f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let _open = self.expect(open);
        let items = self.parse_separated_list(close, f);
        let _close = self.expect(close);
        items
    }

    pub(super) fn parse_and_alloc_expr(&mut self) -> Idx<Expr> {
        let e = self.parse_expr();
        self.alloc_expr(e)
    }

    pub(super) fn parse_alloc_block(&mut self) -> Idx<Expr> {
        let b = self.parse_block();
        self.alloc_expr(b)
    }

    pub(super) fn parse_block_tail(&mut self, mut stmts: Vec<Idx<Expr>>, start: u32) -> Expr {
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let e = self.parse_expr();
            if self.eat(TokenKind::Semi) {
                let idx = self.alloc_expr(e);
                stmts.push(idx);
            } else {
                let tail = self.alloc_expr(e);
                let _rp = self.expect(TokenKind::RParen);
                let stmts: Slice<_> = self.ctx.expr_lists.alloc_slice(stmts);
                return Expr::Block {
                    stmts,
                    tail: Some(tail),
                    span: self.finish_span(start),
                };
            }
        }
        let _rp = self.expect(TokenKind::RParen);
        let stmts: Slice<_> = self.ctx.expr_lists.alloc_slice(stmts);
        Expr::Block {
            stmts,
            tail: None,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_opt_expr(&mut self) -> Option<Idx<Expr>> {
        if can_start_expr(self.peek_kind()) {
            Some(self.parse_and_alloc_expr())
        } else {
            None
        }
    }

    pub(super) fn parse_option<T, F>(&mut self, trigger: TokenKind, mut f: F) -> Option<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        if self.eat(trigger) {
            Some(f(self))
        } else {
            None
        }
    }

    pub(super) fn parse_opt_guard(&mut self) -> Option<Idx<Expr>> {
        self.parse_option(TokenKind::If, Parser::parse_and_alloc_expr)
    }

    pub(super) fn parse_pipe_separated<T, F>(&mut self, closing: TokenKind, mut f: F) -> Vec<T>
    where
        F: FnMut(&mut Self) -> T,
    {
        let mut items = Vec::new();
        if self.at(closing) {
            return items;
        }
        let _ = self.eat(TokenKind::Pipe);
        loop {
            items.push(f(self));
            if !self.eat(TokenKind::Pipe) {
                break;
            }
            if self.at(closing) {
                break;
            }
        }
        items
    }

    /// Wraps `base` in a [`Expr::Postfix`] node, computing the outer span from
    /// `start`.  The caller is responsible for computing any per-op inner span
    /// before passing `op`.
    pub(super) fn wrap_postfix(&self, base: Idx<Expr>, op: PostfixOp, start: u32) -> Expr {
        Expr::Postfix {
            base,
            op,
            span: self.finish_span(start),
        }
    }

    /// Parses `const` or `var`, returning the corresponding [`BindKind`].
    pub(super) fn parse_bind_kind(&mut self) -> BindKind {
        if self.eat(TokenKind::Const) {
            BindKind::Const
        } else {
            let _var = self.expect(TokenKind::Var);
            BindKind::Var
        }
    }

    pub(super) fn optional_ident(&mut self) -> Option<Symbol> {
        if self.at(TokenKind::Ident) {
            Some(self.expect_symbol())
        } else {
            None
        }
    }

    pub(super) fn parse_field_header(&mut self) -> (u32, Vec<Attr>, bool, Symbol) {
        let start = self.start_span();
        let attrs = self.parse_opt_attrs();
        let mutable = self.eat(TokenKind::Var);
        let name = self.expect_symbol();
        (start, attrs, mutable, name)
    }

    pub(super) fn expect_symbol(&mut self) -> Symbol {
        let tok = self.advance().clone();
        if let Some(sym) = tok.symbol {
            return sym;
        }
        if tok.kind != TokenKind::Underscore {
            let _diag = self
                .diags
                .error("expected identifier", tok.span, self.file_id);
        }
        Symbol(u32::MAX)
    }

    pub(super) fn parse_lit_value(&mut self) -> LitValue {
        let tok = self.advance().clone();
        let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
        match tok.kind {
            TokenKind::IntLit => LitValue::Int(parse_int_lit(self.resolve(sym))),
            TokenKind::FloatLit => LitValue::Float(
                self.resolve(sym)
                    .replace('_', "")
                    .parse::<f64>()
                    .unwrap_or(0.0),
            ),
            TokenKind::StringLit => LitValue::Str(sym),
            TokenKind::CharLit => LitValue::Char(parse_char_lit(self.resolve(sym))),
            _ => {
                let _diag = self.diags.error("expected literal", tok.span, self.file_id);
                LitValue::Int(0)
            }
        }
    }

    #[must_use]
    pub(super) fn resolve(&self, sym: Symbol) -> &str {
        self.interner.resolve(sym)
    }

    pub(super) fn parse_expr_list(&mut self, closing: TokenKind) -> Slice<Idx<Expr>> {
        let v = self.parse_separated_list(closing, Parser::parse_and_alloc_expr);
        self.ctx.expr_lists.alloc_slice(v)
    }

    /// Parses `args_list close`, allocates `lhs` as the base, and wraps in a postfix node.
    pub(super) fn parse_list_postfix<F>(
        &mut self, lhs: Expr, start: u32, close: TokenKind, make_op: F,
    ) -> Expr
    where F: FnOnce(Slice<Idx<Expr>>, musi_shared::Span) -> crate::ast::PostfixOp {
        let args = self.parse_expr_list(close);
        let _close = self.expect(close);
        let base = self.alloc_expr(lhs);
        self.wrap_postfix(base, make_op(args, self.finish_span(start)), start)
    }
}
