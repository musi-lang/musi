use music_ast::ExprId;
use music_ast::common::TyRef;
use music_ast::expr::{
    CaseArm, CaseData, ExprKind, HandleData, HandlerClause, QuoteKind, SpliceKind,
};
use music_lex::TokenKind;

use crate::errors::{ParseError, ParseErrorKind, ParseResult};
use crate::parser::Parser;

use super::POSTFIX_BP;

impl Parser<'_> {
    pub(super) fn parse_case(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwCase, "'case'")?;
        let scrutinee = self.parse_expr_no_call(0)?;
        let _ = self.expect(&TokenKind::KwOf, "'of'")?;
        let open_span = self.expect(&TokenKind::LParen, "'('")?;
        let _ = self.eat(&TokenKind::Pipe);
        let mut arms = vec![self.parse_case_arm()?];
        while self.eat(&TokenKind::Pipe) {
            if self.at(&TokenKind::RParen) {
                break;
            }
            arms.push(self.parse_case_arm()?);
        }
        let end =
            self.expect_closing(&TokenKind::RParen, "'('", open_span, "in case expression")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::Case(Box::new(CaseData { scrutinee, arms })), span))
    }

    pub(super) fn parse_case_arm(&mut self) -> ParseResult<CaseArm> {
        let attrs = self.parse_attrs()?;
        let pat = self.parse_pat()?;
        let guard = if self.eat(&TokenKind::KwIf) {
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        let _ = self.expect(&TokenKind::EqGt, "'=>'")?;
        let body = self.parse_expr(0)?;
        Ok(CaseArm {
            attrs,
            pat,
            guard,
            body,
        })
    }

    pub(super) fn parse_return(&mut self) -> ParseResult<ExprId> {
        self.parse_keyword_expr(&TokenKind::KwReturn, "'return'", ExprKind::Return)
    }

    pub(super) fn parse_resume(&mut self) -> ParseResult<ExprId> {
        self.parse_keyword_expr(&TokenKind::KwResume, "'resume'", ExprKind::Resume)
    }

    pub(super) fn parse_keyword_expr(
        &mut self,
        kw: &TokenKind,
        kw_str: &'static str,
        make: fn(Option<ExprId>) -> ExprKind,
    ) -> ParseResult<ExprId> {
        let start = self.expect(kw, kw_str)?;
        let value = if self.can_start_expr() {
            Some(self.parse_expr(0)?)
        } else {
            None
        };
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(make(value), span))
    }

    pub(super) fn parse_perform(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwPerform, "'perform'")?;
        let expr = self.parse_expr(POSTFIX_BP)?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::Perform(expr), span))
    }

    pub(super) fn can_start_expr(&self) -> bool {
        !matches!(
            self.peek_kind(),
            TokenKind::Semi
                | TokenKind::RParen
                | TokenKind::RBracket
                | TokenKind::RBrace
                | TokenKind::Pipe
                | TokenKind::Eof
        )
    }

    pub(super) fn parse_handle(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwHandle, "'handle'")?;
        let body = self.parse_expr(0)?;
        let _ = self.expect(&TokenKind::KwWith, "'with'")?;
        let name = self.expect_ident()?;
        let args = if self.at(&TokenKind::LBracket) {
            self.parse_ty_bracket_args()?
        } else {
            Vec::new()
        };
        let effect = TyRef { name, args };
        let _ = self.expect(&TokenKind::KwOf, "'of'")?;
        let open = self.expect(&TokenKind::LParen, "'('")?;
        let mut clauses = Vec::new();
        let _ = self.eat(&TokenKind::Pipe);
        while !self.at(&TokenKind::RParen) && !self.at_eof() {
            clauses.push(self.parse_handle_clause()?);
            if !self.eat(&TokenKind::Pipe) {
                break;
            }
        }
        let end = self.expect_closing(&TokenKind::RParen, "'('", open, "in handle expression")?;
        let span = start.to(end);
        Ok(self.alloc_expr(
            ExprKind::Handle(Box::new(HandleData {
                effect,
                body,
                clauses,
            })),
            span,
        ))
    }

    pub(super) fn parse_handle_clause(&mut self) -> ParseResult<HandlerClause> {
        if self.at(&TokenKind::KwReturn) {
            let _ = self.advance();
            let binder = self.expect_ident()?;
            let _ = self.expect(&TokenKind::EqGt, "'=>'")?;
            let body = self.parse_expr(0)?;
            return Ok(HandlerClause::Return { binder, body });
        }

        let name = self.expect_ident_or_kw_op()?;
        let _ = self.expect(&TokenKind::LParen, "'('")?;
        let mut binders = Vec::new();
        if !self.at(&TokenKind::RParen) {
            binders.push(self.expect_ident()?);
            while self.eat(&TokenKind::Comma) {
                binders.push(self.expect_ident()?);
            }
        }
        let _ = self.expect(&TokenKind::RParen, "')'")?;
        let _ = self.expect(&TokenKind::EqGt, "'=>'")?;
        let body = self.parse_expr(0)?;
        let Some(cont) = binders.pop() else {
            return Err(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected: "handler continuation binder",
                    found: "')'",
                },
                span: name.span,
                context: Some("handler operation clauses require at least a continuation binder"),
            });
        };
        Ok(HandlerClause::Op {
            name,
            args: binders,
            cont,
            body,
        })
    }

    pub(super) fn parse_quote(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwQuote, "'quote'")?;
        if self.at(&TokenKind::LParen) {
            let _ = self.advance();
            let expr = self.parse_expr(0)?;
            let end = self.expect(&TokenKind::RParen, "')'")?;
            let span = start.to(end);
            return Ok(self.alloc_expr(ExprKind::Quote(QuoteKind::Expr(expr)), span));
        }
        let _ = self.expect(&TokenKind::LBrace, "'{'")?;
        let mut stmts = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at_eof() {
            let expr = self.parse_expr(0)?;
            let _ = self.expect(&TokenKind::Semi, "';'")?;
            stmts.push(expr);
        }
        let end = self.expect(&TokenKind::RBrace, "'}'")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::Quote(QuoteKind::Block(stmts)), span))
    }

    pub(super) fn parse_splice(&mut self) -> ParseResult<ExprId> {
        let start = self.span();
        match self.peek_kind().clone() {
            TokenKind::Hash => {
                let _ = self.advance();
                let ident = self.expect_ident()?;
                let span = start.to(ident.span);
                Ok(self.alloc_expr(ExprKind::Splice(SpliceKind::Ident(ident)), span))
            }
            TokenKind::HashLParen => {
                let _ = self.advance();
                let expr = self.parse_expr(0)?;
                let end = self.expect(&TokenKind::RParen, "')'")?;
                let span = start.to(end);
                Ok(self.alloc_expr(ExprKind::Splice(SpliceKind::Expr(expr)), span))
            }
            TokenKind::HashLBracket => {
                let _ = self.advance();
                let exprs = self.parse_expr_list(&TokenKind::RBracket)?;
                let end = self.expect(&TokenKind::RBracket, "']'")?;
                let span = start.to(end);
                Ok(self.alloc_expr(ExprKind::Splice(SpliceKind::Array(exprs)), span))
            }
            _ => Err(self.err_expected_expr()),
        }
    }
}
