//! Control flow expression parsing: if, match, loops, jumps, and imports.

use musi_lex::token::TokenKind;
use musi_shared::Idx;

use crate::ast::{Cond, ElifBranch, ExportItem, Expr, ImportClause, ImportItem, MatchArm};

use super::{InfixKind, Parser};

impl<'a> Parser<'a> {
    pub(super) fn parse_if(&mut self) -> Expr {
        let start = self.start_span();
        let _if = self.expect(TokenKind::If);

        let cond = Box::new(self.parse_cond());
        let _then = self.expect(TokenKind::Then);
        let then_body = self.parse_and_alloc_expr();

        let mut elif_chains = Vec::new();
        while self.at(TokenKind::Elif) {
            let elif_start = self.start_span();
            let _elif = self.advance();
            let elif_cond = Box::new(self.parse_cond());
            let elif_guard = self.parse_opt_guard();
            let _then = self.expect(TokenKind::Then);
            let elif_body = self.parse_and_alloc_expr();
            elif_chains.push(ElifBranch {
                cond: elif_cond,
                guard: elif_guard,
                body: elif_body,
                span: self.finish_span(elif_start),
            });
        }

        let else_body = self.parse_option(TokenKind::Else, Parser::parse_and_alloc_expr);

        Expr::If {
            cond,
            then_body,
            elif_chains,
            else_body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_cond(&mut self) -> Cond {
        if self.at(TokenKind::Case) {
            let start = self.start_span();
            let _case = self.advance();
            let pat = self.parse_pat();
            let _bind = self.expect(TokenKind::ColonEq);
            let init = self.parse_and_alloc_expr();
            Cond::Case {
                pat,
                init,
                span: self.finish_span(start),
            }
        } else {
            Cond::Expr(self.parse_and_alloc_expr())
        }
    }

    fn parse_match_arm(&mut self) -> MatchArm {
        let arm_start = self.start_span();
        let attrs = self.parse_opt_attrs();
        let pat = self.parse_pat();
        let guard = self.parse_opt_guard();
        let _arrow = self.expect(TokenKind::EqGt);
        let body = self.parse_alloc_arm_body();
        MatchArm {
            attrs,
            pat,
            guard,
            body,
            span: self.finish_span(arm_start),
        }
    }

    pub(super) fn parse_match(&mut self) -> Expr {
        let start = self.start_span();
        let _match = self.expect(TokenKind::Match);
        let scrutinee = self.parse_and_alloc_expr();
        let _with = self.expect(TokenKind::With);
        let _lp = self.expect(TokenKind::LParen);

        let mut arms = vec![self.parse_match_arm()];
        while self.eat(TokenKind::Pipe) {
            arms.push(self.parse_match_arm());
        }

        let _rp = self.expect(TokenKind::RParen);
        Expr::Match {
            scrutinee,
            arms,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_alloc_arm_body(&mut self) -> Idx<Expr> {
        let expr = self.parse_arm_body();
        self.alloc_expr(expr)
    }

    /// Like `parse_pratt(0)` but stops before a top-level `|` (Pipe),
    /// which serves as the arm separator in match expressions.
    fn parse_arm_body(&mut self) -> Expr {
        let start = self.start_span();
        let mut lhs = self.parse_unary_expr();
        lhs = self.parse_postfix_chain(lhs, start);
        loop {
            if self.at(TokenKind::Pipe) {
                break;
            }
            let Some((_l_bp, r_bp, op_kind)) = self.infix_info() else {
                break;
            };
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

    pub(super) fn parse_return(&mut self) -> Expr {
        let start = self.start_span();
        let _ret = self.expect(TokenKind::Return);
        let value = self.parse_opt_expr();
        Expr::Return {
            value,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_break(&mut self) -> Expr {
        let start = self.start_span();
        let _brk = self.expect(TokenKind::Break);
        let value = self.parse_opt_expr();
        Expr::Break {
            label: None,
            value,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_cycle(&mut self) -> Expr {
        let start = self.start_span();
        let _cyc = self.expect(TokenKind::Cycle);
        let label = self.optional_ident();
        let guard = self.parse_opt_guard();
        Expr::Cycle {
            label,
            guard,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_defer(&mut self) -> Expr {
        let start = self.start_span();
        let _def = self.expect(TokenKind::Defer);
        let body = self.parse_and_alloc_expr();
        Expr::Defer {
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_import(&mut self) -> Expr {
        let start = self.start_span();
        let _imp = self.expect(TokenKind::Import);

        let items = if self.eat(TokenKind::Star) {
            if self.eat(TokenKind::As) {
                let name = self.expect_symbol();
                ImportClause::GlobAs(name)
            } else {
                ImportClause::Glob
            }
        } else {
            let list = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
                let item_start = p.start_span();
                let name = p.expect_symbol();
                let alias = p.parse_option(TokenKind::As, Parser::expect_symbol);
                ImportItem {
                    name,
                    alias,
                    span: p.finish_span(item_start),
                }
            });
            ImportClause::Items(list)
        };

        let _from = self.expect(TokenKind::From);
        let path = self.expect_symbol();
        Expr::Import {
            items,
            path,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_export(&mut self) -> Expr {
        let start = self.start_span();
        let _export = self.expect(TokenKind::Export);
        let items = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
            let item_start = p.start_span();
            let name = p.expect_symbol();
            let alias = p.parse_option(TokenKind::As, Parser::expect_symbol);
            ExportItem {
                name,
                alias,
                span: p.finish_span(item_start),
            }
        });
        let _from = self.expect(TokenKind::From);
        let path = self.expect_symbol();
        Expr::Export {
            items,
            path,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_using(&mut self) -> Expr {
        let start = self.start_span();
        let _using = self.expect(TokenKind::Using);
        let name = self.expect_symbol();
        let _assign = self.expect(TokenKind::ColonEq);
        let init = self.parse_and_alloc_expr();
        let body = self.parse_alloc_block();
        Expr::Using {
            name,
            init,
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_while(&mut self) -> Expr {
        let start = self.start_span();
        let _while = self.expect(TokenKind::While);
        let cond = Box::new(self.parse_cond());
        let guard = self.parse_opt_guard();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        Expr::While {
            cond,
            guard,
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_loop(&mut self) -> Expr {
        let start = self.start_span();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        let post_cond = self.parse_option(TokenKind::While, |p| Box::new(p.parse_cond()));
        Expr::Loop {
            body,
            post_cond,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_for(&mut self) -> Expr {
        let start = self.start_span();
        let _for = self.expect(TokenKind::For);
        let pat = self.parse_pat();
        let _in = self.expect(TokenKind::In);
        let iter = self.parse_and_alloc_expr();
        let guard = self.parse_opt_guard();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        Expr::For {
            pat,
            iter,
            guard,
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_label(&mut self) -> Expr {
        let start = self.start_span();
        let _label = self.expect(TokenKind::Label);
        let name = self.expect_symbol();
        let body = self.parse_alloc_block();
        Expr::Label {
            name,
            body,
            span: self.finish_span(start),
        }
    }
}
