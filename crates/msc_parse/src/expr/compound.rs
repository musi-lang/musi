//! Compound expression parsing: paren groups, tuples, blocks, piecewise, match, fn literals.

use msc_ast::expr::{Expr, MatchArm, PwArm, PwGuard};
use msc_ast::lit::Lit;
use msc_ast::ExprIdx;
use msc_lex::token::TokenKind;

use crate::parser::Parser;

impl Parser<'_> {
    pub(super) fn parse_expr_paren(&mut self) -> Expr {
        let start = self.start_span();
        let _lp = self.bump();

        // () -> unit
        if self.eat(TokenKind::RParen) {
            return self.maybe_expr_lit_fn(vec![], start);
        }

        // (,) -> empty tuple
        if self.at(TokenKind::Comma) && self.peek2() == TokenKind::RParen {
            let _comma = self.bump();
            let _rp = self.bump();
            return Expr::Tuple {
                elems: vec![],
                span: self.finish_span(start),
            };
        }

        // (;) -> empty block
        if self.at(TokenKind::Semi) && self.peek2() == TokenKind::RParen {
            let _semi = self.bump();
            let _rp = self.bump();
            return Expr::Block {
                stmts: vec![],
                tail: None,
                span: self.finish_span(start),
            };
        }

        // (mut ...) or (ident : ...) can only be a lambda — parse as param list directly
        if self.at(TokenKind::KwMut)
            || (self.at(TokenKind::Ident) && self.peek_at(1).kind == TokenKind::Colon)
            || (self.at(TokenKind::Underscore) && self.peek_at(1).kind == TokenKind::Colon)
        {
            return self.parse_forced_fn_literal(start);
        }

        let first = self.parse_expr();

        match self.peek_kind() {
            TokenKind::Comma => self.parse_expr_tuple_tail(first, start),
            TokenKind::Semi => {
                let _semi = self.bump();
                let first_idx = self.alloc_expr(first);
                self.parse_expr_block_tail(vec![first_idx], start)
            }
            TokenKind::KwIf => {
                let first_idx = self.alloc_expr(first);
                self.parse_expr_piecewise_tail(first_idx, start)
            }
            TokenKind::RParen => self.parse_expr_paren_close(first, start),
            _ => {
                let _rp = self.expect(TokenKind::RParen);
                let inner = self.alloc_expr(first);
                Expr::Paren {
                    inner,
                    span: self.finish_span(start),
                }
            }
        }
    }

    /// Parses `(a, b, ...)` tuple or multi-param fn literal after the first element and comma.
    fn parse_expr_tuple_tail(&mut self, first: Expr, start: u32) -> Expr {
        let _comma = self.bump();
        let mut raw_elems = vec![first];
        if !self.at(TokenKind::RParen) {
            loop {
                if self.at(TokenKind::KwMut)
                    || (self.at(TokenKind::Ident) && self.peek_at(1).kind == TokenKind::Colon)
                {
                    let mut params = self.reinterpret_as_params(&raw_elems);
                    params.push(self.parse_param());
                    while self.eat(TokenKind::Comma) {
                        if self.at(TokenKind::RParen) {
                            break;
                        }
                        params.push(self.parse_param());
                    }
                    let _rp = self.expect(TokenKind::RParen);
                    let ret_ty = self.parse_opt_ty_annot();
                    let _arrow = self.expect(TokenKind::EqGt);
                    let body = self.parse_alloc_expr();
                    return Expr::Fn {
                        params,
                        ret_ty,
                        body,
                        span: self.finish_span(start),
                    };
                }
                raw_elems.push(self.parse_expr());
                if !self.eat(TokenKind::Comma) {
                    break;
                }
                if self.at(TokenKind::RParen) {
                    break;
                }
            }
        }
        let _rp = self.expect(TokenKind::RParen);

        if self.at(TokenKind::EqGt) || self.at(TokenKind::Colon) {
            return self.parse_expr_fn_after_paren(&raw_elems, start);
        }

        let elems = raw_elems.into_iter().map(|e| self.alloc_expr(e)).collect();
        Expr::Tuple {
            elems,
            span: self.finish_span(start),
        }
    }

    /// Handles `)` after a single expression: either fn literal or parenthesised expr.
    fn parse_expr_paren_close(&mut self, first: Expr, start: u32) -> Expr {
        let _rp = self.bump();

        if self.at(TokenKind::EqGt) || self.at(TokenKind::Colon) {
            return self.parse_expr_fn_after_paren(&[first], start);
        }

        let inner = self.alloc_expr(first);
        Expr::Paren {
            inner,
            span: self.finish_span(start),
        }
    }

    fn maybe_expr_lit_fn(&mut self, _exprs: Vec<Expr>, start: u32) -> Expr {
        // () followed by => or : means fn literal
        if self.at(TokenKind::EqGt) || self.at(TokenKind::Colon) {
            return self.parse_expr_fn_after_paren(&[], start);
        }
        Expr::Lit {
            lit: Lit::Unit {
                span: self.finish_span(start),
            },
            span: self.finish_span(start),
        }
    }

    /// Parse `(mut param, ...) => body` - called when `(` was consumed and `mut` is next.
    fn parse_forced_fn_literal(&mut self, start: u32) -> Expr {
        let mut params = vec![self.parse_param()];
        while self.eat(TokenKind::Comma) {
            if self.at(TokenKind::RParen) {
                break;
            }
            params.push(self.parse_param());
        }
        let _rp = self.expect(TokenKind::RParen);
        let ret_ty = self.parse_opt_ty_annot();
        let _arrow = self.expect(TokenKind::EqGt);
        let body = self.parse_alloc_expr();
        Expr::Fn {
            params,
            ret_ty,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_fn_after_paren(&mut self, paren_exprs: &[Expr], start: u32) -> Expr {
        let params = self.reinterpret_as_params(paren_exprs);
        let ret_ty = self.parse_opt_ty_annot();
        let _arrow = self.expect(TokenKind::EqGt);
        let body = self.parse_alloc_expr();
        Expr::Fn {
            params,
            ret_ty,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_block_tail(&mut self, mut stmts: Vec<ExprIdx>, start: u32) -> Expr {
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            let e = self.parse_expr();
            if self.eat(TokenKind::Semi) {
                stmts.push(self.alloc_expr(e));
            } else if self.at(TokenKind::KwIf) {
                let first_idx = self.alloc_expr(e);
                let pw = self.parse_piecewise_arms(first_idx, start);
                let tail = self.alloc_expr(pw);
                let _rp = self.expect(TokenKind::RParen);
                return Expr::Block {
                    stmts,
                    tail: Some(tail),
                    span: self.finish_span(start),
                };
            } else {
                let tail = self.alloc_expr(e);
                let _rp = self.expect(TokenKind::RParen);
                return Expr::Block {
                    stmts,
                    tail: Some(tail),
                    span: self.finish_span(start),
                };
            }
        }
        let _rp = self.expect(TokenKind::RParen);
        Expr::Block {
            stmts,
            tail: None,
            span: self.finish_span(start),
        }
    }

    pub(crate) fn parse_piecewise_arms(&mut self, first_result: ExprIdx, start: u32) -> Expr {
        let _if = self.expect(TokenKind::KwIf);
        let first_guard = self.parse_pw_guard();
        let first_span = self.finish_span(start);
        let mut arms = vec![PwArm {
            result: first_result,
            guard: first_guard,
            span: first_span,
        }];

        while self.eat(TokenKind::Pipe) {
            let arm_start = self.start_span();
            let result = self.parse_alloc_expr();
            let _if = self.expect(TokenKind::KwIf);
            let guard = self.parse_pw_guard();
            arms.push(PwArm {
                result,
                guard,
                span: self.finish_span(arm_start),
            });
        }

        Expr::Piecewise {
            arms,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_piecewise_tail(&mut self, first_result: ExprIdx, start: u32) -> Expr {
        let pw = self.parse_piecewise_arms(first_result, start);
        let _rp = self.expect(TokenKind::RParen);
        pw
    }

    fn parse_pw_guard(&mut self) -> PwGuard {
        if self.at(TokenKind::Underscore) {
            let start = self.start_span();
            let _us = self.bump();
            PwGuard::Any {
                span: self.finish_span(start),
            }
        } else {
            let expr = self.parse_alloc_expr();
            PwGuard::When {
                expr,
                span: self.peek().span,
            }
        }
    }

    pub(super) fn parse_expr_match(&mut self) -> Expr {
        let start = self.start_span();
        let _match = self.bump();
        let scrut_start = self.start_span();
        let scrut_expr = self.parse_expr_nud_chain();
        let scrut_expr = self.parse_expr_postfix_chain_match(scrut_expr, scrut_start);
        let scrutinee = self.alloc_expr(scrut_expr);
        let _lp = self.expect(TokenKind::LParen);
        let arms = self.pipe_sep(TokenKind::RParen, Self::parse_match_arm);
        let _rp = self.expect(TokenKind::RParen);
        Expr::Match {
            scrutinee,
            arms,
            span: self.finish_span(start),
        }
    }

    fn parse_match_arm(&mut self) -> MatchArm {
        let arm_start = self.start_span();
        let attrs = self.parse_attrs();
        let pat = self.parse_alloc_pat();
        let guard = if self.eat(TokenKind::KwIf) {
            Some(self.parse_alloc_expr())
        } else {
            None
        };
        let _arrow = self.expect(TokenKind::EqGt);
        let result_expr = self.parse_arm_body();
        let result = self.alloc_expr(result_expr);
        MatchArm {
            attrs,
            pat,
            guard,
            result,
            span: self.finish_span(arm_start),
        }
    }
}
