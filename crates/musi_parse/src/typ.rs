use musi_ast::{Idents, Typ, TypKind, TypList};
use musi_basic::error::{IntoMusiError, MusiResult};
use musi_lex::token::TokenKind;

use crate::{Parser, error::ParseErrorKind};

impl Parser<'_> {
    pub fn parse_typ(&mut self) -> MusiResult<Typ> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Question) => {
                let _ = self.advance();
                let inner = Box::new(self.parse_typ()?);
                Ok(Typ {
                    kind: TypKind::Optional(inner),
                    span: start.merge(self.prev_span()),
                })
            }
            Some(TokenKind::Caret) => {
                let _ = self.advance();
                let inner = Box::new(self.parse_typ()?);
                Ok(Typ {
                    kind: TypKind::Ptr(inner),
                    span: start.merge(self.prev_span()),
                })
            }
            Some(TokenKind::LBrack) => {
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
            _ => {
                let mut ty = self.parse_typ_primary()?;
                if self.at(TokenKind::MinusGt) {
                    let _ = self.advance();
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

    fn parse_typ_primary(&mut self) -> MusiResult<Typ> {
        let start = self.curr_span();
        match self.peek_kind() {
            Some(TokenKind::Ident(id)) => {
                let _ = self.advance();
                if self.at(TokenKind::LBrack) {
                    let _ = self.advance();
                    let args = self.parse_typ_list()?;
                    let _ = self.expect(TokenKind::RBrack)?;
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
            Some(TokenKind::LParen) => {
                let _ = self.advance();
                if self.at(TokenKind::RParen) {
                    let _ = self.advance();
                    return Ok(Typ {
                        kind: TypKind::Tuple(vec![]),
                        span: start.merge(self.prev_span()),
                    });
                }
                let first = self.parse_typ()?;
                if self.at(TokenKind::Comma) {
                    let _ = self.advance();
                    let mut elems = vec![first];
                    if !self.at(TokenKind::RParen) {
                        elems.extend(self.parse_typ_list()?);
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
            Some(kind) => {
                Err(ParseErrorKind::Unexpected(kind.as_str().into()).into_musi_error(start))
            }
            None => Err(ParseErrorKind::UnexpectedEof.into_musi_error(start)),
        }
    }

    fn parse_typ_list(&mut self) -> MusiResult<TypList> {
        let mut types = vec![self.parse_typ()?];
        while self.at(TokenKind::Comma) {
            let _ = self.advance();
            if self.at(TokenKind::RBrack) || self.at(TokenKind::RParen) {
                break;
            }
            types.push(self.parse_typ()?);
        }
        Ok(types)
    }

    pub fn parse_typ_params(&mut self) -> MusiResult<Idents> {
        if !self.at(TokenKind::LBrack) {
            return Ok(vec![]);
        }
        let _ = self.advance();
        let mut params = vec![];
        while !self.at(TokenKind::RBrack) && !self.is_eof() {
            params.push(self.expect_ident()?);
            if !self.at(TokenKind::Comma) {
                break;
            }
            let _ = self.advance();
        }
        let _ = self.expect(TokenKind::RBrack)?;
        Ok(params)
    }

    pub fn parse_typ_args(&mut self) -> MusiResult<TypList> {
        if !self.at(TokenKind::LBrack) {
            return Ok(vec![]);
        }
        let _ = self.advance();
        let types = if self.at(TokenKind::RBrack) {
            vec![]
        } else {
            self.parse_typ_list()?
        };
        let _ = self.expect(TokenKind::RBrack)?;
        Ok(types)
    }
}
