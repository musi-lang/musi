//! Expression parsing: Pratt, postfix chains, literals, and record/array literals.

use musi_lex::token::TokenKind;
use musi_shared::Symbol;

use crate::ast::{ArrayItem, BinOp, Expr, FieldInit, PostfixOp, PrefixOp};

use super::{InfixKind, Parser};

impl Parser<'_> {
    pub(super) fn parse_expr(&mut self) -> Expr {
        self.parse_pratt(0)
    }

    pub(super) fn parse_pratt(&mut self, min_bp: u8) -> Expr {
        let start = self.start_span();

        let mut lhs = self.parse_unary_expr();
        lhs = self.parse_postfix_chain(lhs, start);

        loop {
            let Some((l_bp, r_bp, op_kind)) = self.infix_info() else {
                break;
            };
            if l_bp < min_bp {
                break;
            }
            let _op_tok = self.advance();
            let rhs_expr = self.parse_pratt(r_bp);
            let lhs_idx = self.alloc_expr(lhs);
            let rhs_idx = self.alloc_expr(rhs_expr);
            let span = self.finish_span(start);

            lhs = match op_kind {
                InfixKind::Binary(op) => Expr::Binary {
                    op,
                    lhs: lhs_idx,
                    rhs: rhs_idx,
                    span,
                },
                InfixKind::Assign => Expr::Assign {
                    target: lhs_idx,
                    value: rhs_idx,
                    span,
                },
            };
        }

        lhs
    }

    /// Returns `(left_bp, right_bp, kind)` for the current token if it is an
    /// infix operator, or `None` otherwise.
    #[must_use]
    pub(super) fn infix_info(&self) -> Option<(u8, u8, InfixKind)> {
        use BinOp as B;
        use InfixKind::{Assign, Binary};
        use TokenKind as T;

        let kind = self.peek_kind();
        let (l, r, op) = match kind {
            // BP 10 -- assign (right-assoc)
            T::LtMinus => (10, 9, Assign),
            // BP 15 -- nil coalescing (right-assoc)
            T::QuestionQuestion => (15, 14, Binary(B::NilCoalesce)),
            // BP 20 -- or / xor (left-assoc)
            T::Or => (20, 21, Binary(B::Or)),
            T::Xor => (20, 21, Binary(B::Xor)),
            // BP 30 -- and (left-assoc)
            T::And => (30, 31, Binary(B::And)),
            // BP 40 -- equality (non-assoc)
            T::Eq => (40, 41, Binary(B::Eq)),
            T::SlashEq => (40, 41, Binary(B::NotEq)),
            // BP 50 -- comparison (non-assoc)
            T::Lt => (50, 51, Binary(B::Lt)),
            T::Gt => (50, 51, Binary(B::Gt)),
            T::LtEq => (50, 51, Binary(B::LtEq)),
            T::GtEq => (50, 51, Binary(B::GtEq)),
            T::In => (50, 51, Binary(B::In)),
            // BP 60 -- range (non-assoc)
            T::DotDot => (60, 61, Binary(B::Range)),
            T::DotDotLt => (60, 61, Binary(B::RangeExcl)),
            // BP 70 -- cons (left-assoc)
            T::ColonColon => (70, 71, Binary(B::Cons)),
            // BP 80 -- bitwise or / xor (left-assoc)
            T::Pipe => (80, 81, Binary(B::BitOr)),
            T::Caret => (80, 81, Binary(B::BitXor)),
            // BP 90 -- bitwise and (left-assoc)
            T::Amp => (90, 91, Binary(B::BitAnd)),
            // BP 100 -- shift (left-assoc)
            T::Shl => (100, 101, Binary(B::Shl)),
            T::Shr => (100, 101, Binary(B::Shr)),
            // BP 110 -- additive (left-assoc)
            T::Plus => (110, 111, Binary(B::Add)),
            T::Minus => (110, 111, Binary(B::Sub)),
            // BP 120 -- multiplicative (left-assoc)
            T::Star => (120, 121, Binary(B::Mul)),
            T::Slash => (120, 121, Binary(B::Div)),
            T::Percent => (120, 121, Binary(B::Rem)),
            _ => return None,
        };
        Some((l, r, op))
    }

    pub(super) fn parse_postfix_chain(&mut self, mut lhs: Expr, start: u32) -> Expr {
        loop {
            match self.peek_kind() {
                // Call: f(args)
                TokenKind::LParen => {
                    let _ = self.advance();
                    lhs = self.parse_list_postfix(lhs, start, TokenKind::RParen, |args, span| {
                        PostfixOp::Call { args, span }
                    });
                }
                // Index: e.[args]
                TokenKind::DotLBracket => {
                    let _ = self.advance();
                    lhs = self.parse_list_postfix(lhs, start, TokenKind::RBracket, |args, span| {
                        PostfixOp::Index { args, span }
                    });
                }
                // RecDot: e.{ fields }
                TokenKind::DotLBrace => {
                    let _dlb = self.advance();
                    let fields =
                        self.parse_separated_list(TokenKind::RBrace, Parser::parse_rec_lit_field);
                    let _rb = self.expect(TokenKind::RBrace);
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(
                        base,
                        PostfixOp::RecDot {
                            fields,
                            span: self.finish_span(start),
                        },
                        start,
                    );
                }
                // Field: e.name or e.0
                TokenKind::Dot => {
                    let _dot = self.advance();
                    let field_start = self.start_span();
                    match self.peek_kind() {
                        TokenKind::Ident | TokenKind::IntLit => {
                            let name = self.expect_symbol();
                            let f_span = self.finish_span(field_start);
                            let base = self.alloc_expr(lhs);
                            lhs = self.wrap_postfix(
                                base,
                                PostfixOp::Field { name, span: f_span },
                                start,
                            );
                        }
                        _ => {
                            let _err = self.diags.error(
                                "expected field name",
                                self.peek().span,
                                self.file_id,
                            );
                            break;
                        }
                    }
                }
                // OptField: e?.field
                TokenKind::QuestionDot => {
                    let _qd = self.advance();
                    let field_start = self.start_span();
                    let name = self.expect_symbol();
                    let f_span = self.finish_span(field_start);
                    let base = self.alloc_expr(lhs);
                    lhs =
                        self.wrap_postfix(base, PostfixOp::OptField { name, span: f_span }, start);
                }
                // As cast: e as T
                TokenKind::As => {
                    let _as_kw = self.advance();
                    let ty = self.parse_ty();
                    let base = self.alloc_expr(lhs);
                    lhs = self.wrap_postfix(
                        base,
                        PostfixOp::As {
                            ty,
                            span: self.finish_span(start),
                        },
                        start,
                    );
                }
                _ => break,
            }
        }
        lhs
    }

    fn parse_prefix(&mut self, op: PrefixOp) -> Expr {
        let start = self.start_span();
        let _tok = self.advance();
        let inner = self.parse_pratt(130);
        let operand = self.alloc_expr(inner);
        Expr::Prefix {
            op,
            operand,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_unary_expr(&mut self) -> Expr {
        match self.peek_kind() {
            TokenKind::Minus => self.parse_prefix(PrefixOp::Neg),
            TokenKind::Not => self.parse_prefix(PrefixOp::Not),
            TokenKind::Bang => self.parse_prefix(PrefixOp::Deref),
            TokenKind::At => self.parse_prefix(PrefixOp::AddrOf),
            TokenKind::Tilde => self.parse_prefix(PrefixOp::BitNot),

            TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit | TokenKind::CharLit => {
                self.parse_lit()
            }

            TokenKind::Ident => {
                let start = self.start_span();
                let tok = self.advance().clone();
                let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
                Expr::Ident {
                    name: sym,
                    span: self.finish_span(start),
                }
            }

            TokenKind::LParen => self.parse_expr_paren(),

            TokenKind::LBracket => self.parse_array_lit(),

            TokenKind::DotLBrace => self.parse_anon_rec_lit(),

            // Dot-prefix: .Name or .Name(args)  -- constructor/variant shorthand
            TokenKind::Dot => self.parse_dot_prefix(),

            TokenKind::If => self.parse_if(),
            TokenKind::Match => self.parse_match(),
            TokenKind::Return => self.parse_return(),
            TokenKind::Break => self.parse_break(),
            TokenKind::Cycle => self.parse_cycle(),
            TokenKind::Defer => self.parse_defer(),
            TokenKind::Import => self.parse_import(),

            // export { ... } from "path" -- re-export; export fn/record/... -- modifier
            TokenKind::Export => {
                if self.peek2() == TokenKind::LBrace {
                    self.parse_export()
                } else {
                    self.parse_expr_after_attrs()
                }
            }

            TokenKind::Using => self.parse_using(),

            TokenKind::Hash | TokenKind::Opaque | TokenKind::Extrin => {
                self.parse_expr_after_attrs()
            }
            TokenKind::Fn => { let s = self.start_span(); self.parse_fn_expr(Vec::new(), Vec::new(), s) }
            TokenKind::Record => { let s = self.start_span(); self.parse_record(Vec::new(), Vec::new(), s) }
            TokenKind::Choice => { let s = self.start_span(); self.parse_choice(Vec::new(), Vec::new(), s) }
            TokenKind::Const | TokenKind::Var => { let s = self.start_span(); self.parse_bind(Vec::new(), Vec::new(), s) }

            TokenKind::While => self.parse_while(),
            TokenKind::Loop => self.parse_loop(),
            TokenKind::For => self.parse_for(),
            TokenKind::Class => self.parse_class_def(),
            TokenKind::Given => self.parse_given_def(),

            _ => self.error_expr("unexpected token"),
        }
    }

    pub(super) fn parse_lit(&mut self) -> Expr {
        let start = self.start_span();
        let value = self.parse_lit_value();
        Expr::Lit {
            value,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_paren(&mut self) -> Expr {
        let start = self.start_span();
        let _lp = self.expect(TokenKind::LParen);

        // Unit: ()
        if self.eat(TokenKind::RParen) {
            return Expr::Unit {
                span: self.finish_span(start),
            };
        }

        // Parse first expression
        let first = self.parse_expr();

        match self.peek_kind() {
            // Tuple: (e, ...)
            TokenKind::Comma => {
                let _comma = self.advance();
                let mut raw = vec![self.alloc_expr(first)];
                raw.extend(
                    self.parse_separated_list(TokenKind::RParen, Parser::parse_and_alloc_expr),
                );
                let _rp = self.expect(TokenKind::RParen);
                let elements = self.ctx.expr_lists.alloc_slice(raw);
                Expr::Tuple {
                    elements,
                    span: self.finish_span(start),
                }
            }
            // Block: (e; ... [tail])
            TokenKind::Semi => {
                let _semi = self.advance();
                let first_idx = self.alloc_expr(first);
                self.parse_block_tail(vec![first_idx], start)
            }
            _ => {
                // Single: (e), or unexpected token with error recovery; RParen expected
                let _rp = self.expect(TokenKind::RParen);
                let inner = self.alloc_expr(first);
                Expr::Paren {
                    inner,
                    span: self.finish_span(start),
                }
            }
        }
    }

    fn parse_array_lit(&mut self) -> Expr {
        let start = self.start_span();
        let _lb = self.expect(TokenKind::LBracket);
        let mut items = Vec::new();

        if !self.at(TokenKind::RBracket) {
            loop {
                if self.eat(TokenKind::LtDotDot) {
                    items.push(ArrayItem::Spread(self.parse_and_alloc_expr()));
                } else {
                    items.push(ArrayItem::Single(self.parse_and_alloc_expr()));
                }
                if !self.eat(TokenKind::Comma) {
                    break;
                }
                if self.at(TokenKind::RBracket) {
                    break;
                }
            }
        }
        let _rb = self.expect(TokenKind::RBracket);
        Expr::Array {
            items,
            span: self.finish_span(start),
        }
    }

    fn parse_anon_rec_lit(&mut self) -> Expr {
        let start = self.start_span();
        let _dlb = self.expect(TokenKind::DotLBrace);
        let fields = self.parse_separated_list(TokenKind::RBrace, Parser::parse_rec_lit_field);
        let _rb = self.expect(TokenKind::RBrace);
        Expr::AnonRec {
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_dot_prefix(&mut self) -> Expr {
        let start = self.start_span();
        let _dot = self.expect(TokenKind::Dot);
        let name = self.expect_symbol();
        let args = if self.at(TokenKind::LParen) {
            let _lp = self.advance();
            let args = self.parse_expr_list(TokenKind::RParen);
            let _rp = self.expect(TokenKind::RParen);
            args
        } else {
            self.ctx.expr_lists.alloc_slice([])
        };
        Expr::DotPrefix {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_block(&mut self) -> Expr {
        let start = self.start_span();
        let _lp = self.expect(TokenKind::LParen);
        self.parse_block_tail(Vec::new(), start)
    }

    pub(super) fn parse_rec_lit_field(&mut self) -> FieldInit {
        let start = self.start_span();

        // Spread: <.. expr
        if self.eat(TokenKind::LtDotDot) {
            let idx = self.parse_and_alloc_expr();
            return FieldInit::Spread {
                expr: idx,
                span: self.finish_span(start),
            };
        }

        // field_base: [attrs] [var] name [:= expr]
        // Shorthand (no :=): { x } desugars to { x := x }
        let attrs = self.parse_opt_attrs();
        let mutable = self.eat(TokenKind::Var);
        let name = self.expect_symbol();
        let value = if self.eat(TokenKind::ColonEq) {
            self.parse_and_alloc_expr()
        } else {
            // Shorthand: synthesize Expr::Ident with same name
            let ident_span = self.finish_span(start);
            self.alloc_expr(Expr::Ident {
                name,
                span: ident_span,
            })
        };
        FieldInit::Named {
            attrs,
            mutable,
            name,
            value,
            span: self.finish_span(start),
        }
    }
}
