use musi_ast::{Expr, ExprKind, LitKind, OptExprPtr, Pat, PatKind, PatList, TypList};
use musi_basic::{
    error::{IntoMusiError, MusiResult},
    span::Span,
};
use musi_lex::token::TokenKind;

use crate::{Parser, error::ParseErrorKind};

impl Parser<'_> {
    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_pat(&mut self) -> MusiResult<Pat> {
        self.parse_pat_or()
    }

    fn parse_pat_or(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        let first = self.parse_pat_cons()?;
        if !self.at(TokenKind::Bar) {
            return Ok(first);
        }
        let mut alts = vec![first];
        while self.bump_if(TokenKind::Bar) {
            alts.push(self.parse_pat_cons()?);
        }
        Ok(Pat::new(PatKind::Or(alts), start.merge(self.prev_span())))
    }

    fn parse_pat_cons(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        let first = self.parse_pat_primary()?;
        if !self.at(TokenKind::ColonColon) {
            return Ok(first);
        }
        let mut parts = vec![first];
        while self.bump_if(TokenKind::ColonColon) {
            parts.push(self.parse_pat_primary()?);
        }
        Ok(Pat::new(
            PatKind::Cons(parts),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_pat_primary(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Underscore) => {
                let _ = self.advance();
                Ok(Pat::new(PatKind::Wild, start))
            }
            Some(
                TokenKind::LitInt(_)
                | TokenKind::LitReal(_)
                | TokenKind::LitString(_)
                | TokenKind::LitRune(_)
                | TokenKind::KwTrue
                | TokenKind::KwFalse,
            ) => self.parse_pat_lit(),
            Some(TokenKind::Ident(id)) => {
                let _ = self.advance();
                self.parse_pat_after_ident(id, start)
            }
            Some(TokenKind::Dot) if self.peek_nth(1) == Some(TokenKind::LBrace) => {
                self.parse_pat_record_anon()
            }
            Some(TokenKind::LParen) => self.parse_pat_paren(),
            Some(TokenKind::LBrack) => self.parse_pat_array(),
            Some(kind) => {
                Err(ParseErrorKind::Unexpected(kind.as_str().into()).into_musi_error(start))
            }
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_pat_lit(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        let kind = match self.peek_kind() {
            Some(TokenKind::LitInt(id)) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::Int(i64::from(id)))
            }
            Some(TokenKind::LitReal(id)) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::Real(f64::from(id)))
            }
            Some(TokenKind::LitString(id)) => {
                let _ = self.advance();
                PatKind::Lit(LitKind::String(id))
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
            _ => return Err(ParseErrorKind::Expected("literal pattern").into_musi_error(start)),
        };
        Ok(Pat::new(kind, start))
    }

    fn parse_pat_after_ident(&mut self, id: u32, start: Span) -> MusiResult<Pat> {
        if self.at(TokenKind::Dot) && self.peek_nth(1) == Some(TokenKind::LBrace) {
            let ty_expr = Expr::new(ExprKind::Ident(id), start);
            return self.parse_pat_record(Some(Box::new(ty_expr)), start);
        }
        let ty_args = self.parse_typ_args()?;
        if self.at(TokenKind::LParen) {
            let args = self.delimited(TokenKind::LParen, TokenKind::RParen, |p| {
                p.separated(TokenKind::Comma, Self::parse_pat)
            })?;
            return Ok(self.make_pat_variant(id, ty_args, args, start));
        }
        if !ty_args.is_empty() {
            return Ok(self.make_pat_variant(id, ty_args, vec![], start));
        }
        Ok(Pat::new(PatKind::Ident(id), start))
    }

    fn parse_pat_record(&mut self, ty: OptExprPtr, start: Span) -> MusiResult<Pat> {
        self.advance_by(2); // consume `.` and `{`
        let fields = self.separated(TokenKind::Comma, Self::expect_ident)?;
        let _ = self.expect(TokenKind::RBrace)?;
        Ok(Pat::new(
            PatKind::Record { ty, fields },
            start.merge(self.prev_span()),
        ))
    }

    fn parse_pat_record_anon(&mut self) -> MusiResult<Pat> {
        self.parse_pat_record(None, self.curr_span())
    }

    fn parse_pat_paren(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.bump_if(TokenKind::RParen) {
            return Ok(self.make_empty_tuple_pat(start));
        }
        let first = self.parse_pat()?;
        if self.bump_if(TokenKind::Comma) {
            self.parse_pat_tuple(first, start)
        } else {
            self.parse_pat_grouped(first, start)
        }
    }

    fn parse_pat_tuple(&mut self, first: Pat, start: Span) -> MusiResult<Pat> {
        let mut elems = vec![first];
        if !self.at(TokenKind::RParen) {
            elems.extend(self.separated(TokenKind::Comma, Self::parse_pat)?);
        }
        let _ = self.expect(TokenKind::RParen)?;
        Ok(Pat::new(
            PatKind::Tuple(elems),
            start.merge(self.prev_span()),
        ))
    }

    fn parse_pat_grouped(&mut self, inner: Pat, start: Span) -> MusiResult<Pat> {
        let _ = self.expect(TokenKind::RParen)?;
        Ok(Pat::new(inner.kind, start.merge(self.prev_span())))
    }

    fn parse_pat_array(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        let pats = self.delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
            p.separated(TokenKind::Comma, Self::parse_pat)
        })?;
        Ok(Pat::new(
            PatKind::Array(pats),
            start.merge(self.prev_span()),
        ))
    }
}

impl Parser<'_> {
    fn make_pat_variant(&self, name: u32, ty_args: TypList, args: PatList, start: Span) -> Pat {
        Pat::new(
            PatKind::Variant {
                name,
                ty_args,
                args,
            },
            start.merge(self.prev_span()),
        )
    }

    fn make_empty_tuple_pat(&self, start: Span) -> Pat {
        Pat::new(PatKind::Tuple(vec![]), start.merge(self.prev_span()))
    }
}
