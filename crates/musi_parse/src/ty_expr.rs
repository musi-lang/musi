use musi_ast::{Idents, TyExpr, TyExprKind, TyExprList};
use musi_basic::{
    error::{IntoMusiError, MusiResult},
    span::Span,
};
use musi_lex::token::TokenKind;

use crate::{Parser, error::ParseErrorKind};

// ============================================================================
// TYPE EXPRESSION PARSING
// ============================================================================

impl Parser<'_> {
    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_ty_expr(&mut self) -> MusiResult<TyExpr> {
        let start = self.curr_span();
        let ty = self.parse_ty_expr_prefix()?;
        self.parse_ty_expr_fn(ty, start)
    }

    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_ty_expr_params(&mut self) -> MusiResult<Idents> {
        self.opt_delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
            p.separated(TokenKind::Comma, Self::expect_ident)
        })
    }

    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_ty_expr_args(&mut self) -> MusiResult<TyExprList> {
        self.opt_delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
            p.separated(TokenKind::Comma, Self::parse_ty_expr)
        })
    }

    fn parse_ty_expr_fn(&mut self, param: TyExpr, start: Span) -> MusiResult<TyExpr> {
        if self.bump_if(TokenKind::MinusGt) {
            let ret = Box::new(self.parse_ty_expr()?);
            Ok(TyExpr::new(
                TyExprKind::Fn {
                    param: Box::new(param),
                    ret,
                },
                start.merge(self.prev_span()),
            ))
        } else {
            Ok(param)
        }
    }

    fn parse_ty_expr_prefix(&mut self) -> MusiResult<TyExpr> {
        match self.peek_kind() {
            Some(TokenKind::Question) => self.parse_ty_expr_optional(),
            Some(TokenKind::Caret) => self.parse_ty_expr_ptr(),
            Some(TokenKind::LBrack) => self.parse_ty_expr_array(),
            _ => self.parse_ty_expr_primary(),
        }
    }

    fn parse_ty_expr_optional(&mut self) -> MusiResult<TyExpr> {
        let start = self.curr_span();
        let _ = self.advance();
        let inner = Box::new(self.parse_ty_expr_prefix()?);
        Ok(TyExpr::new(
            TyExprKind::Optional(inner),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_ty_expr_ptr(&mut self) -> MusiResult<TyExpr> {
        let start = self.curr_span();
        let _ = self.advance();
        let inner = Box::new(self.parse_ty_expr_prefix()?);
        Ok(TyExpr::new(
            TyExprKind::Ptr(inner),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_ty_expr_array(&mut self) -> MusiResult<TyExpr> {
        let start = self.curr_span();
        let _ = self.advance();
        let size = if let Some(TokenKind::LitInt(id)) = self.peek_kind() {
            let _ = self.advance();
            Some(i64::from(id))
        } else {
            None
        };
        let _ = self.expect(TokenKind::RBrack)?;
        let elem = Box::new(self.parse_ty_expr_prefix()?);
        Ok(TyExpr::new(
            TyExprKind::Array { size, elem },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_ty_expr_primary(&mut self) -> MusiResult<TyExpr> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Ident(id)) => self.parse_ty_expr_ident(id),
            Some(TokenKind::LParen) => self.parse_ty_expr_paren(),
            Some(kind) => Err(ParseErrorKind::UnexpectedToken(kind).into_musi_error(start)),
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_ty_expr_ident(&mut self, id: u32) -> MusiResult<TyExpr> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.at(TokenKind::LBrack) {
            let args = self.delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
                p.separated(TokenKind::Comma, Self::parse_ty_expr)
            })?;
            Ok(TyExpr::new(
                TyExprKind::App { base: id, args },
                start.merge(self.prev_span()),
            ))
        } else {
            Ok(TyExpr::new(TyExprKind::Ident(id), start))
        }
    }

    fn parse_ty_expr_paren(&mut self) -> MusiResult<TyExpr> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.bump_if(TokenKind::RParen) {
            return Ok(self.make_empty_ty_expr_tuple(start));
        }
        let first = self.parse_ty_expr()?;
        if self.bump_if(TokenKind::Comma) {
            self.parse_ty_expr_tuple(first, start)
        } else {
            self.parse_ty_expr_grouped(first, start)
        }
    }

    fn parse_ty_expr_tuple(&mut self, first: TyExpr, start: Span) -> MusiResult<TyExpr> {
        let mut elems = vec![first];
        if !self.at(TokenKind::RParen) {
            elems.extend(self.separated(TokenKind::Comma, Self::parse_ty_expr)?);
        }
        let _ = self.expect(TokenKind::RParen)?;
        Ok(TyExpr::new(
            TyExprKind::Tuple(elems),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_ty_expr_grouped(&mut self, inner: TyExpr, start: Span) -> MusiResult<TyExpr> {
        let _ = self.expect(TokenKind::RParen)?;
        Ok(TyExpr::new(inner.kind, start.merge(self.prev_span())))
    }
}

// ============================================================================
// TYPE EXPRESSION HELPERS
// ============================================================================

impl Parser<'_> {
    fn make_empty_ty_expr_tuple(&self, start: Span) -> TyExpr {
        TyExpr::new(TyExprKind::Tuple(vec![]), start.merge(self.prev_span()))
    }
}
