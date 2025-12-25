use musi_ast::{Idents, Typ, TypKind, TypList};
use musi_basic::error::{IntoMusiError, MusiResult};
use musi_lex::token::TokenKind;

use crate::{Parser, error::ParseErrorKind};

impl Parser<'_> {
    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_typ(&mut self) -> MusiResult<Typ> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Question) => self.parse_typ_optional(),
            Some(TokenKind::Caret) => self.parse_typ_ptr(),
            Some(TokenKind::LBrack) => self.parse_typ_array(),
            _ => {
                let mut ty = self.parse_typ_primary()?;
                if self.bump_if(TokenKind::MinusGt) {
                    let ret = Box::new(self.parse_typ()?);
                    ty = Typ {
                        kind: TypKind::Fn {
                            param: Box::new(ty),
                            ret,
                        },
                        span: start.merge(self.prev_span()),
                    };
                }
                Ok(ty)
            }
        }
    }

    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_typ_params(&mut self) -> MusiResult<Idents> {
        self.opt_delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
            p.separated(TokenKind::Comma, Self::expect_ident)
        })
    }

    /// # Errors
    /// Returns `ParseErrorKind` on syntax error.
    pub fn parse_typ_args(&mut self) -> MusiResult<TypList> {
        self.opt_delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
            p.separated(TokenKind::Comma, Self::parse_typ)
        })
    }

    fn parse_typ_optional(&mut self) -> MusiResult<Typ> {
        let start = self.curr_span();
        let _ = self.advance();
        let inner = Box::new(self.parse_typ()?);
        Ok(Typ {
            kind: TypKind::Optional(inner),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_typ_ptr(&mut self) -> MusiResult<Typ> {
        let start = self.curr_span();
        let _ = self.advance();
        let inner = Box::new(self.parse_typ()?);
        Ok(Typ {
            kind: TypKind::Ptr(inner),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_typ_array(&mut self) -> MusiResult<Typ> {
        let start = self.curr_span();
        let _ = self.advance();
        let size = if let Some(TokenKind::LitInt(id)) = self.peek_kind() {
            let _ = self.advance();
            Some(i64::from(id))
        } else {
            None
        };
        let _ = self.expect(TokenKind::RBrack)?;
        let elem = Box::new(self.parse_typ()?);
        Ok(Typ {
            kind: TypKind::Array { size, elem },
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_typ_primary(&mut self) -> MusiResult<Typ> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Ident(id)) => self.parse_typ_ident(id),
            Some(TokenKind::LParen) => self.parse_typ_paren(),
            Some(kind) => {
                Err(ParseErrorKind::Unexpected(kind.as_str().into()).into_musi_error(start))
            }
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_typ_ident(&mut self, id: u32) -> MusiResult<Typ> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.at(TokenKind::LBrack) {
            let args = self.delimited(TokenKind::LBrack, TokenKind::RBrack, |p| {
                p.separated(TokenKind::Comma, Self::parse_typ)
            })?;
            Ok(Typ {
                kind: TypKind::App { base: id, args },
                span: start.merge(self.prev_span()),
            })
        } else {
            Ok(Typ {
                kind: TypKind::Ident(id),
                span: start,
            })
        }
    }

    fn parse_typ_paren(&mut self) -> MusiResult<Typ> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.bump_if(TokenKind::RParen) {
            return Ok(Typ {
                kind: TypKind::Tuple(vec![]),
                span: start.merge(self.prev_span()),
            });
        }
        let first = self.parse_typ()?;
        if self.bump_if(TokenKind::Comma) {
            let mut elems = vec![first];
            if !self.at(TokenKind::RParen) {
                elems.extend(self.separated(TokenKind::Comma, Self::parse_typ)?);
            }
            let _ = self.expect(TokenKind::RParen)?;
            Ok(Typ {
                kind: TypKind::Tuple(elems),
                span: start.merge(self.prev_span()),
            })
        } else {
            let _ = self.expect(TokenKind::RParen)?;
            Ok(Typ {
                kind: first.kind,
                span: start.merge(self.prev_span()),
            })
        }
    }
}
