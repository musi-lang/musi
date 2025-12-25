use musi_ast::{Idents, LitKind, Pat, PatKind, PatList};
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
        while self.at(TokenKind::Bar) {
            let _ = self.advance();
            alts.push(self.parse_pat_cons()?);
        }
        Ok(Pat {
            kind: PatKind::Or(alts),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_pat_cons(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        let first = self.parse_pat_primary()?;
        if !self.at(TokenKind::ColonColon) {
            return Ok(first);
        }
        let mut parts = vec![first];
        while self.at(TokenKind::ColonColon) {
            let _ = self.advance();
            parts.push(self.parse_pat_primary()?);
        }
        Ok(Pat {
            kind: PatKind::Cons(parts),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_pat_primary(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Underscore) => {
                let _ = self.advance();
                Ok(Pat {
                    kind: PatKind::Wild,
                    span: start,
                })
            }
            Some(
                TokenKind::LitInt(_)
                | TokenKind::LitReal(_)
                | TokenKind::LitString(_)
                | TokenKind::LitRune(_)
                | TokenKind::KwTrue
                | TokenKind::KwFalse,
            ) => self.parse_pat_lit(),
            Some(TokenKind::Ident(id)) => self.parse_pat_ident(id, start),
            Some(TokenKind::Dot) if self.peek_nth(1) == Some(TokenKind::LBrace) => {
                self.advance_by(2);
                let fields = self.parse_pat_fields()?;
                let _ = self.expect(TokenKind::RBrace)?;
                Ok(Pat {
                    kind: PatKind::Record { ty: None, fields },
                    span: start.merge(self.prev_span()),
                })
            }
            Some(TokenKind::LParen) => self.parse_pat_tuple_or_grouped(),
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
        Ok(Pat { kind, span: start })
    }

    fn parse_pat_ident(&mut self, id: u32, start: Span) -> MusiResult<Pat> {
        let _ = self.advance();
        if self.at(TokenKind::Dot) && self.peek_nth(1) == Some(TokenKind::LBrace) {
            self.advance_by(2);
            let fields = self.parse_pat_fields()?;
            let _ = self.expect(TokenKind::RBrace)?;
            return Ok(Pat {
                kind: PatKind::Record {
                    ty: Some(id),
                    fields,
                },
                span: start.merge(self.prev_span()),
            });
        }
        let ty_args = self.parse_typ_args()?;
        if self.at(TokenKind::LParen) {
            let _ = self.advance();
            let args = self.parse_pat_list()?;
            let _ = self.expect(TokenKind::RParen)?;
            return Ok(Pat {
                kind: PatKind::Variant {
                    name: id,
                    ty_args,
                    args,
                },
                span: start.merge(self.prev_span()),
            });
        }
        if !ty_args.is_empty() {
            return Ok(Pat {
                kind: PatKind::Variant {
                    name: id,
                    ty_args,
                    args: vec![],
                },
                span: start.merge(self.prev_span()),
            });
        }
        Ok(Pat {
            kind: PatKind::Ident(id),
            span: start,
        })
    }

    fn parse_pat_tuple_or_grouped(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        let _ = self.advance();
        if self.at(TokenKind::RParen) {
            let _ = self.advance();
            return Ok(Pat {
                kind: PatKind::Tuple(vec![]),
                span: start.merge(self.prev_span()),
            });
        }
        let first = self.parse_pat()?;
        if self.at(TokenKind::Comma) {
            let _ = self.advance();
            let mut pats = vec![first];
            if !self.at(TokenKind::RParen) {
                pats.extend(self.parse_pat_list()?);
            }
            let _ = self.expect(TokenKind::RParen)?;
            Ok(Pat {
                kind: PatKind::Tuple(pats),
                span: start.merge(self.prev_span()),
            })
        } else {
            let _ = self.expect(TokenKind::RParen)?;
            Ok(Pat {
                kind: first.kind,
                span: start.merge(self.prev_span()),
            })
        }
    }

    fn parse_pat_array(&mut self) -> MusiResult<Pat> {
        let start = self.curr_span();
        let _ = self.advance();
        let pats = if self.at(TokenKind::RBrack) {
            vec![]
        } else {
            self.parse_pat_list()?
        };
        let _ = self.expect(TokenKind::RBrack)?;
        Ok(Pat {
            kind: PatKind::Array(pats),
            span: start.merge(self.prev_span()),
        })
    }

    fn parse_pat_list(&mut self) -> MusiResult<PatList> {
        let mut pats = vec![self.parse_pat()?];
        while self.at(TokenKind::Comma) {
            let _ = self.advance();
            if self.at(TokenKind::RParen)
                || self.at(TokenKind::RBrack)
                || self.at(TokenKind::RBrace)
            {
                break;
            }
            pats.push(self.parse_pat()?);
        }
        Ok(pats)
    }

    fn parse_pat_fields(&mut self) -> MusiResult<Idents> {
        let mut fields = vec![];
        while !self.at(TokenKind::RBrace) && !self.is_eof() {
            fields.push(self.expect_ident()?);
            if !self.at(TokenKind::Comma) {
                break;
            }
            let _ = self.advance();
        }
        Ok(fields)
    }
}
