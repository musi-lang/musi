use musi_ast::{Idents, LitKind, Pat, PatKind, PatList};
use musi_basic::error::{IntoMusiError, MusiResult};
use musi_lex::token::TokenKind;

use crate::{Parser, error::ParseErrorKind};

impl Parser<'_> {
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
            Some(TokenKind::LitInt(id)) => {
                let _ = self.advance();
                Ok(Pat {
                    kind: PatKind::Lit(LitKind::Int(i64::from(id))),
                    span: start,
                })
            }
            Some(TokenKind::LitReal(id)) => {
                let _ = self.advance();
                Ok(Pat {
                    kind: PatKind::Lit(LitKind::Real(f64::from(id))),
                    span: start,
                })
            }
            Some(TokenKind::LitString(id)) => {
                let _ = self.advance();
                Ok(Pat {
                    kind: PatKind::Lit(LitKind::String(id)),
                    span: start,
                })
            }
            Some(TokenKind::LitRune(c)) => {
                let _ = self.advance();
                Ok(Pat {
                    kind: PatKind::Lit(LitKind::Rune(c)),
                    span: start,
                })
            }
            Some(TokenKind::KwTrue) => {
                let _ = self.advance();
                Ok(Pat {
                    kind: PatKind::Lit(LitKind::Bool(true)),
                    span: start,
                })
            }
            Some(TokenKind::KwFalse) => {
                let _ = self.advance();
                Ok(Pat {
                    kind: PatKind::Lit(LitKind::Bool(false)),
                    span: start,
                })
            }

            Some(TokenKind::Ident(id)) => {
                let _ = self.advance();
                if self.at(TokenKind::Dot) && self.peek_nth(1) == Some(TokenKind::LBrace) {
                    let _ = self.advance();
                    let _ = self.advance();
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

            Some(TokenKind::Dot) if self.peek_nth(1) == Some(TokenKind::LBrace) => {
                let _ = self.advance();
                let _ = self.advance();
                let fields = self.parse_pat_fields()?;
                let _ = self.expect(TokenKind::RBrace)?;
                Ok(Pat {
                    kind: PatKind::Record { ty: None, fields },
                    span: start.merge(self.prev_span()),
                })
            }

            Some(TokenKind::LParen) => {
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

            Some(TokenKind::LBrack) => {
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

            Some(kind) => {
                Err(ParseErrorKind::Unexpected(kind.as_str().into()).into_musi_error(start))
            }
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
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
