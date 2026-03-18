//! Postfix chain parsing: calls, indexing, field access, update.

use msc_ast::expr::{Arg, Expr, FieldKey, TypeCheckKind, UnaryOp};
use msc_lex::token::TokenKind;
use msc_shared::Symbol;

use crate::error::ParseError;
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
                TokenKind::BangDot => self.parse_expr_force_field(lhs, start),
                TokenKind::Bang => self.parse_expr_force_unwrap(lhs, start),
                TokenKind::Question => self.parse_expr_propagate(lhs, start),
                TokenKind::ColonQuestion => self.parse_expr_type_test(lhs, start),
                TokenKind::ColonQuestionGt => self.parse_expr_type_cast(lhs, start),
                _ => break,
            };
        }
        lhs
    }

    pub(super) fn parse_expr_postfix_chain_match(&mut self, mut lhs: Expr, start: u32) -> Expr {
        loop {
            lhs = match self.peek_kind() {
                TokenKind::LParen if self.lookahead_has_arrow_in_parens() => break,
                TokenKind::LParen => self.parse_expr_call(lhs, start),
                TokenKind::DotLBracket => self.parse_expr_index(lhs, start),
                TokenKind::DotLBrace => self.parse_expr_update(lhs, start),
                TokenKind::Dot => self.parse_expr_field(lhs, start, false),
                TokenKind::QuestionDot => self.parse_expr_field(lhs, start, true),
                TokenKind::BangDot => self.parse_expr_force_field(lhs, start),
                TokenKind::Bang => self.parse_expr_force_unwrap(lhs, start),
                TokenKind::Question => self.parse_expr_propagate(lhs, start),
                TokenKind::ColonQuestion => self.parse_expr_type_test(lhs, start),
                TokenKind::ColonQuestionGt => self.parse_expr_type_cast(lhs, start),
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
            let span = self.finish_span(start);
            let index = text.parse::<u32>().unwrap_or_else(|_| {
                let _diag = self.diags.error(
                    ParseError::InvalidIntLiteral.to_string(),
                    span,
                    self.file_id,
                );
                0
            });
            FieldKey::Pos { index, span }
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
        // Spread: ...expr
        if self.eat(TokenKind::DotDotDot) {
            let expr = self.parse_alloc_expr();
            return Arg::Spread {
                expr,
                span: self.finish_span(start),
            };
        }
        let first = self.parse_expr();
        if self.at(TokenKind::KwIf) {
            let first_idx = self.alloc_expr(first);
            let pw = self.parse_piecewise_arms(first_idx, start);
            let expr = self.alloc_expr(pw);
            return Arg::Pos {
                expr,
                span: self.finish_span(start),
            };
        }
        let expr = self.alloc_expr(first);
        Arg::Pos {
            expr,
            span: self.finish_span(start),
        }
    }

    /// Parses `expr!` - force unwrap.
    fn parse_expr_force_unwrap(&mut self, lhs: Expr, start: u32) -> Expr {
        let _bang = self.bump();
        let operand = self.alloc_expr(lhs);
        Expr::UnaryOp {
            op: UnaryOp::ForceUnwrap,
            operand,
            span: self.finish_span(start),
        }
    }

    /// Parses `expr?` - error propagation.
    fn parse_expr_propagate(&mut self, lhs: Expr, start: u32) -> Expr {
        let _q = self.bump();
        let operand = self.alloc_expr(lhs);
        Expr::UnaryOp {
            op: UnaryOp::Propagate,
            operand,
            span: self.finish_span(start),
        }
    }

    /// Parses `expr!.field` - forced unwrap field access.
    ///
    /// Desugared to a force-unwrap followed by a field access.
    fn parse_expr_force_field(&mut self, lhs: Expr, start: u32) -> Expr {
        let _bang_dot = self.bump();
        let field = self.parse_field_key();
        let operand = self.alloc_expr(lhs);
        let unwrapped = Expr::UnaryOp {
            op: UnaryOp::ForceUnwrap,
            operand,
            span: self.finish_span(start),
        };
        let unwrapped_idx = self.alloc_expr(unwrapped);
        Expr::Field {
            object: unwrapped_idx,
            field,
            safe: false,
            span: self.finish_span(start),
        }
    }

    /// Parses `expr :? Type` or `expr :? Type as ident` - type test.
    fn parse_expr_type_test(&mut self, lhs: Expr, start: u32) -> Expr {
        let _cq = self.bump();
        let ty = self.parse_alloc_ty();
        let binding = if self.eat(TokenKind::KwAs) {
            Some(self.expect_symbol())
        } else {
            None
        };
        let operand = self.alloc_expr(lhs);
        Expr::TypeCheck {
            kind: TypeCheckKind::Test,
            operand,
            ty,
            binding,
            span: self.finish_span(start),
        }
    }

    /// Parses `expr :?> Type` - type cast.
    fn parse_expr_type_cast(&mut self, lhs: Expr, start: u32) -> Expr {
        let _cqg = self.bump();
        let ty = self.parse_alloc_ty();
        let operand = self.alloc_expr(lhs);
        Expr::TypeCheck {
            kind: TypeCheckKind::Cast,
            operand,
            ty,
            binding: None,
            span: self.finish_span(start),
        }
    }
}
