use musi_ast::{TyExprId, TyExprKind};
use musi_core::{MusiResult, Span, Name};
use musi_lex::token::TokenKind;

use crate::{Parser, errors};

impl Parser<'_> {
    /// # Errors
    /// Returns error on syntax failure.
    pub fn parse_ty_expr(&mut self) -> MusiResult<TyExprId> {
        let start = self.curr_span();
        let ty_id = self.parse_ty_expr_prefix()?;
        self.parse_ty_expr_fn(ty_id, start)
    }

    /// # Errors
    /// Returns error on syntax failure.
    pub fn parse_ty_expr_params(&mut self) -> MusiResult<Vec<Name>> {
        self.opt_delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
            p.separated(TokenKind::Comma, Self::expect_ident)
        })
    }

    /// # Errors
    /// Returns error on syntax failure.
    pub fn parse_ty_expr_args(&mut self) -> MusiResult<Vec<TyExprId>> {
        self.opt_delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
            p.separated(TokenKind::Comma, Self::parse_ty_expr)
        })
    }

    fn parse_ty_expr_fn(&mut self, param_id: TyExprId, start: Span) -> MusiResult<TyExprId> {
        if self.bump_if(TokenKind::MinusGt) {
            let ret_id = self.parse_ty_expr()?;
            let span = start.merge(self.prev_span());
            Ok(self.arena.alloc_ty_expr(
                TyExprKind::Fn {
                    param: param_id,
                    ret: ret_id,
                },
                span,
            ))
        } else {
            Ok(param_id)
        }
    }

    fn parse_ty_expr_prefix(&mut self) -> MusiResult<TyExprId> {
        match self.peek_kind() {
            Some(TokenKind::Question) => self.parse_ty_expr_optional(),
            Some(TokenKind::Caret) => self.parse_ty_expr_ptr(),
            Some(TokenKind::LBrack) => self.parse_ty_expr_array(),
            _ => self.parse_ty_expr_primary(),
        }
    }

    fn parse_ty_expr_optional(&mut self) -> MusiResult<TyExprId> {
        let start = self.curr_span();
        let _ = self.advance();
        let inner_id = self.parse_ty_expr_prefix()?;
        let span = start.merge(self.prev_span());
        Ok(self
            .arena
            .alloc_ty_expr(TyExprKind::Optional(inner_id), span))
    }

    fn parse_ty_expr_ptr(&mut self) -> MusiResult<TyExprId> {
        let start = self.curr_span();
        let _ = self.advance();
        let inner_id = self.parse_ty_expr_prefix()?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_ty_expr(TyExprKind::Ptr(inner_id), span))
    }

    fn parse_ty_expr_array(&mut self) -> MusiResult<TyExprId> {
        let start = self.curr_span();
        let _ = self.advance();
        let size = if let Some(TokenKind::LitInt {
            raw: v,
            base,
            suffix: _,
        }) = self.peek_kind()
        {
            let _ = self.advance();
            Some(self.parse_int(v, base)?)
        } else {
            None
        };
        let _ = self.expect(TokenKind::RBrack)?;
        let elem_id = self.parse_ty_expr_prefix()?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_ty_expr(
            TyExprKind::Array {
                size,
                elem: elem_id,
            },
            span,
        ))
    }

    fn parse_ty_expr_primary(&mut self) -> MusiResult<TyExprId> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Ident(ident)) => self.parse_ty_expr_ident(ident),
            Some(TokenKind::LParen) => self.parse_ty_expr_paren(),
            Some(kind) => Err(errors::unexpected_token(kind, start)),
            None => Err(errors::unexpected_eof(start)),
        }
    }

    fn parse_ty_expr_ident(&mut self, ident: Name) -> MusiResult<TyExprId> {
        let start = ident.span;
        let _ = self.advance();
        if self.at(TokenKind::LBrack) {
            let args = self.delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
                p.separated(TokenKind::Comma, Self::parse_ty_expr)
            })?;
            let span = start.merge(self.prev_span());
            Ok(self
                .arena
                .alloc_ty_expr(TyExprKind::App { base: ident, args }, span))
        } else {
            Ok(self.arena.alloc_ty_expr(TyExprKind::Ident(ident), start))
        }
    }

    fn parse_ty_expr_paren(&mut self) -> MusiResult<TyExprId> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.bump_if(TokenKind::RParen) {
            let span = start.merge(self.prev_span());
            return Ok(self.arena.alloc_ty_expr(TyExprKind::Tuple(vec![]), span));
        }
        let first_id = self.parse_ty_expr()?;
        if self.bump_if(TokenKind::Comma) {
            self.parse_ty_expr_tuple(first_id, start)
        } else {
            self.parse_ty_expr_grouped(first_id, start)
        }
    }

    fn parse_ty_expr_tuple(&mut self, first_id: TyExprId, start: Span) -> MusiResult<TyExprId> {
        let mut elems = vec![first_id];
        if !self.at(TokenKind::RParen) {
            elems.extend(self.separated(TokenKind::Comma, Self::parse_ty_expr)?);
        }
        let _ = self.expect(TokenKind::RParen)?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_ty_expr(TyExprKind::Tuple(elems), span))
    }

    fn parse_ty_expr_grouped(&mut self, inner_id: TyExprId, start: Span) -> MusiResult<TyExprId> {
        let _ = self.expect(TokenKind::RParen)?;
        let inner = self.arena.ty_exprs.get(inner_id);
        let kind = inner.kind.clone();
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_ty_expr(kind, span))
    }
}

#[cfg(test)]
mod tests;
