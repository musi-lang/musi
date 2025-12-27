use musi_ast::{ExprId, ExprKind, LitKind, PatId, PatIds, PatKind, TyExprIds};
use musi_basic::{
    error::{IntoMusiError, MusiResult},
    span::Span,
};
use musi_lex::token::TokenKind;

use crate::{Parser, error::ParseErrorKind};

impl Parser<'_> {
    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_pat(&mut self) -> MusiResult<PatId> {
        self.parse_pat_or()
    }

    fn parse_pat_or(&mut self) -> MusiResult<PatId> {
        let start = self.curr_span();
        let first_id = self.parse_pat_cons()?;
        if !self.at(TokenKind::Bar) {
            return Ok(first_id);
        }
        let mut alts = vec![first_id];
        while self.bump_if(TokenKind::Bar) {
            alts.push(self.parse_pat_cons()?);
        }
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_pat(PatKind::Or(alts), span))
    }

    fn parse_pat_cons(&mut self) -> MusiResult<PatId> {
        let start = self.curr_span();
        let first_id = self.parse_pat_primary()?;
        if !self.at(TokenKind::ColonColon) {
            return Ok(first_id);
        }
        let mut parts = vec![first_id];
        while self.bump_if(TokenKind::ColonColon) {
            parts.push(self.parse_pat_primary()?);
        }
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_pat(PatKind::Cons(parts), span))
    }

    fn parse_pat_primary(&mut self) -> MusiResult<PatId> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Underscore) => {
                let _ = self.advance();
                Ok(self.arena.alloc_pat(PatKind::Wild, start))
            }
            Some(
                TokenKind::LitInt(_)
                | TokenKind::LitReal(_)
                | TokenKind::LitString(_)
                | TokenKind::LitRune(_)
                | TokenKind::KwTrue
                | TokenKind::KwFalse,
            ) => self.parse_pat_lit(),
            Some(TokenKind::Ident(ident)) => {
                let _ = self.advance();
                self.parse_pat_after_ident(ident, ident.span)
            }
            Some(TokenKind::Dot) if self.peek_nth(1) == Some(TokenKind::LBrace) => {
                self.parse_pat_record_anon()
            }
            Some(TokenKind::LParen) => self.parse_pat_paren(),
            Some(TokenKind::LBrack) => self.parse_pat_array(),
            Some(kind) => Err(ParseErrorKind::UnexpectedToken(kind).into_musi_error(start)),
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_pat_lit(&mut self) -> MusiResult<PatId> {
        let start = self.curr_span();
        let kind = match self.peek_kind() {
            Some(TokenKind::LitInt(v)) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::Int(v))
            }
            Some(TokenKind::LitReal(v)) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::Real(v))
            }
            Some(TokenKind::LitString(ident)) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::String(ident))
            }
            Some(TokenKind::LitRune(c)) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::Rune(c))
            }
            Some(TokenKind::KwTrue) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::Bool(true))
            }
            Some(TokenKind::KwFalse) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::Bool(false))
            }
            _ => {
                return Err(ParseErrorKind::ExpectedLit.into_musi_error(start));
            }
        };
        Ok(self.arena.alloc_pat(kind, start))
    }

    fn parse_pat_after_ident(&mut self, ident: musi_ast::Ident, start: Span) -> MusiResult<PatId> {
        if self.at(TokenKind::Dot) && self.peek_nth(1) == Some(TokenKind::LBrace) {
            let ty_expr_id = self.arena.alloc_expr(ExprKind::Ident(ident), start);
            return self.parse_pat_record(Some(ty_expr_id), start);
        }
        let ty_args = self.parse_ty_expr_args()?;
        if self.at(TokenKind::LParen) {
            let args = self.delimited(TokenKind::LParen, TokenKind::RParen, |p| {
                p.separated(TokenKind::Comma, Self::parse_pat)
            })?;
            return Ok(self.make_pat_variant(ident, ty_args, args, start));
        }
        if !ty_args.is_empty() {
            return Ok(self.make_pat_variant(ident, ty_args, vec![], start));
        }
        Ok(self.arena.alloc_pat(PatKind::Ident(ident), start))
    }

    fn parse_pat_record(&mut self, base: Option<ExprId>, start: Span) -> MusiResult<PatId> {
        self.advance_by(2); // consume `.` and `{`
        let fields = self.separated(TokenKind::Comma, Self::expect_ident)?;
        let _ = self.expect(TokenKind::RBrace)?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_pat(PatKind::Record { base, fields }, span))
    }

    fn parse_pat_record_anon(&mut self) -> MusiResult<PatId> {
        self.parse_pat_record(None, self.curr_span())
    }

    fn parse_pat_paren(&mut self) -> MusiResult<PatId> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.bump_if(TokenKind::RParen) {
            return Ok(self.make_empty_tuple_pat(start));
        }
        let first_id = self.parse_pat()?;
        if self.bump_if(TokenKind::Comma) {
            self.parse_pat_tuple(first_id, start)
        } else {
            self.parse_pat_grouped(first_id, start)
        }
    }

    fn parse_pat_tuple(&mut self, first_id: PatId, start: Span) -> MusiResult<PatId> {
        let mut elems = vec![first_id];
        if !self.at(TokenKind::RParen) {
            elems.extend(self.separated(TokenKind::Comma, Self::parse_pat)?);
        }
        let _ = self.expect(TokenKind::RParen)?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_pat(PatKind::Tuple(elems), span))
    }

    fn parse_pat_grouped(&mut self, inner_id: PatId, start: Span) -> MusiResult<PatId> {
        let _ = self.expect(TokenKind::RParen)?;
        let inner = self.arena.pats.get(inner_id);
        let kind = inner.kind.clone();
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_pat(kind, span))
    }

    fn parse_pat_array(&mut self) -> MusiResult<PatId> {
        let start = self.curr_span();
        let pats = self.delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
            p.separated(TokenKind::Comma, Self::parse_pat)
        })?;
        let span = start.merge(self.prev_span());
        Ok(self.arena.alloc_pat(PatKind::Array(pats), span))
    }
}

impl Parser<'_> {
    fn make_pat_variant(
        &mut self,
        name: musi_ast::Ident,
        ty_args: TyExprIds,
        args: PatIds,
        start: Span,
    ) -> PatId {
        let span = start.merge(self.prev_span());
        self.arena.alloc_pat(
            PatKind::Choice {
                name,
                ty_args,
                args,
            },
            span,
        )
    }

    fn make_empty_tuple_pat(&mut self, start: Span) -> PatId {
        let span = start.merge(self.prev_span());
        self.arena.alloc_pat(PatKind::Tuple(vec![]), span)
    }
}

#[cfg(test)]
mod tests;
