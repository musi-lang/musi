//! Postfix chain parsing: calls, indexing, field access, update.

use music_ast::expr::{Arg, Expr, FieldKey};
use music_lex::token::TokenKind;
use music_shared::Symbol;

use crate::parser::Parser;

impl Parser<'_> {
    pub(super) fn parse_expr_postfix_chain(&mut self, mut lhs: Expr, start: u32) -> Expr {
        loop {
            lhs = match self.peek_kind() {
                TokenKind::LParen => self.parse_expr_call(lhs, start),
                TokenKind::DotLBracket => self.parse_expr_index(lhs, start),
                TokenKind::DotLBrace => self.parse_expr_update(lhs, start),
                TokenKind::Dot => self.parse_expr_field(lhs, start, false),
                TokenKind::QuestionDot => self.parse_expr_field(lhs, start, true),
                _ => break,
            };
        }
        lhs
    }

    fn parse_expr_call(&mut self, lhs: Expr, start: u32) -> Expr {
        let _lp = self.bump();
        let args = self.comma_sep(TokenKind::RParen, Self::parse_arg);
        let _rp = self.expect(TokenKind::RParen);
        let callee = self.alloc_expr(lhs);
        Expr::Call {
            callee,
            args,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_index(&mut self, lhs: Expr, start: u32) -> Expr {
        let _dlb = self.bump();
        let index = self.parse_alloc_expr();
        let _rb = self.expect(TokenKind::RBracket);
        let object = self.alloc_expr(lhs);
        Expr::Index {
            object,
            index,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_update(&mut self, lhs: Expr, start: u32) -> Expr {
        let _dlb = self.bump();
        let fields = self.comma_sep(TokenKind::RBrace, Self::parse_rec_field);
        let _rb = self.expect(TokenKind::RBrace);
        let base = self.alloc_expr(lhs);
        Expr::Update {
            base,
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_field(&mut self, lhs: Expr, start: u32, safe: bool) -> Expr {
        let _tok = self.bump();
        let field = self.parse_field_key();
        let object = self.alloc_expr(lhs);
        Expr::Field {
            object,
            field,
            safe,
            span: self.finish_span(start),
        }
    }

    fn parse_field_key(&mut self) -> FieldKey {
        let start = self.start_span();
        if self.at(TokenKind::IntLit) {
            let tok = self.bump();
            let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
            let text = self.resolve(sym);
            let index = text.parse::<u32>().unwrap_or(0);
            FieldKey::Pos {
                index,
                span: self.finish_span(start),
            }
        } else {
            let name = self.expect_symbol();
            FieldKey::Name {
                name,
                span: self.finish_span(start),
            }
        }
    }

    fn parse_arg(&mut self) -> Arg {
        let start = self.start_span();
        // Hole: ...
        if self.eat(TokenKind::DotDotDot) {
            return Arg::Hole {
                span: self.finish_span(start),
            };
        }
        let expr = self.parse_alloc_expr();
        Arg::Pos {
            expr,
            span: self.finish_span(start),
        }
    }
}
