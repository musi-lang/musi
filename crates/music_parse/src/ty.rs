use music_ast::TyId;
use music_ast::ty::{Dim, TyKind};
use music_lex::TokenKind;
use music_shared::Span;

use crate::errors::ParseResult;
use crate::parser::Parser;

impl Parser<'_> {
    pub(crate) fn parse_ty(&mut self) -> ParseResult<TyId> {
        self.parse_ty_arrow()
    }

    fn parse_ty_arrow(&mut self) -> ParseResult<TyId> {
        let left = self.parse_ty_sum()?;
        self.parse_ty_arrow_rest(left)
    }

    fn parse_ty_arrow_rest(&mut self, left: TyId) -> ParseResult<TyId> {
        let is_pure = self.at(&TokenKind::MinusGt);
        let is_effect = self.at(&TokenKind::TildeGt);
        if is_pure || is_effect {
            let _ = self.advance();
            let right = self.parse_ty_arrow()?;
            let span = self.merge_ty_spans(left, right);
            let kind = if is_pure {
                TyKind::Arrow {
                    from: left,
                    to: right,
                }
            } else {
                TyKind::EffectArrow {
                    from: left,
                    to: right,
                }
            };
            return Ok(self.alloc_ty(kind, span));
        }
        Ok(left)
    }

    fn parse_ty_sum(&mut self) -> ParseResult<TyId> {
        let first = self.parse_ty_product()?;
        if !self.at(&TokenKind::Plus) {
            return Ok(first);
        }
        let mut members = vec![first];
        while self.eat(&TokenKind::Plus) {
            members.push(self.parse_ty_product()?);
        }
        let start = self.ast.types.get(first).span;
        let end = self
            .ast
            .types
            .get(*members.last().expect("at least two members"))
            .span;
        let span = start.to(end);
        Ok(self.alloc_ty(TyKind::Sum(members), span))
    }

    fn parse_ty_product(&mut self) -> ParseResult<TyId> {
        let first = self.parse_ty_base()?;
        if !self.at(&TokenKind::Star) {
            return Ok(first);
        }
        let mut members = vec![first];
        while self.eat(&TokenKind::Star) {
            members.push(self.parse_ty_base()?);
        }
        let start = self.ast.types.get(first).span;
        let end = self
            .ast
            .types
            .get(*members.last().expect("at least two members"))
            .span;
        let span = start.to(end);
        Ok(self.alloc_ty(TyKind::Product(members), span))
    }

    fn parse_ty_base(&mut self) -> ParseResult<TyId> {
        match self.peek_kind() {
            TokenKind::KwMut => self.parse_ty_prefix(TyKind::Mut),
            TokenKind::Question => self.parse_ty_prefix(TyKind::Option),
            TokenKind::Ident | TokenKind::EscapedIdent => self.parse_ty_named(),
            TokenKind::LParen => self.parse_ty_paren(),
            TokenKind::LBracket => self.parse_ty_array(),
            _ => Err(self.err_expected_type()),
        }
    }

    fn parse_ty_prefix(&mut self, wrap: fn(TyId) -> TyKind) -> ParseResult<TyId> {
        let start = self.advance().span;
        let inner = self.parse_ty_base()?;
        let span = start.to(self.ast.types.get(inner).span);
        Ok(self.alloc_ty(wrap(inner), span))
    }

    pub(crate) fn parse_ty_named_only(&mut self) -> ParseResult<TyId> {
        self.parse_ty_named()
    }

    fn parse_ty_named(&mut self) -> ParseResult<TyId> {
        let start = self.span();
        let name = self.expect_ident()?;
        let args = if self.eat(&TokenKind::KwOf) {
            self.parse_ty_of_args()?
        } else {
            Vec::new()
        };
        let span = start.to(self.prev_span());
        Ok(self.alloc_ty(TyKind::Named { name, args }, span))
    }

    fn parse_ty_of_args(&mut self) -> ParseResult<Vec<TyId>> {
        let mut args = vec![self.parse_ty()?];
        while self.eat(&TokenKind::Comma) {
            if !matches!(
                self.peek_kind(),
                TokenKind::KwMut
                    | TokenKind::Question
                    | TokenKind::LParen
                    | TokenKind::LBracket
                    | TokenKind::Ident
                    | TokenKind::EscapedIdent
            ) {
                break;
            }
            args.push(self.parse_ty()?);
        }
        Ok(args)
    }

    fn parse_ty_paren(&mut self) -> ParseResult<TyId> {
        let start = self.expect(&TokenKind::LParen, "'('")?;
        // Check for pi type: (name : Ty) -> Ty
        if matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
            let saved_pos = self.pos;
            let name = self.expect_ident()?;
            if self.eat(&TokenKind::Colon) {
                let param_ty = self.parse_ty()?;
                let _ = self.expect(&TokenKind::RParen, "')'")?;
                if self.at(&TokenKind::MinusGt) {
                    let _ = self.advance();
                    let ret_ty = self.parse_ty()?;
                    let span = start.to(self.ast.types.get(ret_ty).span);
                    return Ok(self.alloc_ty(
                        TyKind::Pi {
                            name,
                            param_ty,
                            ret_ty,
                        },
                        span,
                    ));
                }
                return Ok(param_ty);
            }
            // Backtrack: not a pi type
            self.pos = saved_pos;
        }
        self.parse_ty_tuple_or_grouped(start)
    }

    fn parse_ty_tuple_or_grouped(&mut self, start: Span) -> ParseResult<TyId> {
        if self.at(&TokenKind::RParen) {
            let end = self.expect(&TokenKind::RParen, "')'")?;
            let span = start.to(end);
            return Ok(self.alloc_ty(TyKind::Tuple(Vec::new()), span));
        }
        let first = self.parse_ty()?;
        if self.at(&TokenKind::RParen) {
            let _ = self.expect(&TokenKind::RParen, "')'")?;
            return Ok(first);
        }
        let mut items = vec![first];
        while self.eat(&TokenKind::Comma) {
            if self.at(&TokenKind::RParen) {
                break;
            }
            items.push(self.parse_ty()?);
        }
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        Ok(self.alloc_ty(TyKind::Tuple(items), span))
    }

    fn parse_ty_array(&mut self) -> ParseResult<TyId> {
        let start = self.expect(&TokenKind::LBracket, "'['")?;
        let dims = self.parse_dim_list()?;
        let _ = self.expect(&TokenKind::RBracket, "']'")?;
        let elem = self.parse_ty()?;
        let span = start.to(self.ast.types.get(elem).span);
        Ok(self.alloc_ty(TyKind::Array { dims, elem }, span))
    }

    fn parse_dim_list(&mut self) -> ParseResult<Vec<Dim>> {
        let mut dims = Vec::new();
        while !self.at(&TokenKind::RBracket) && !self.at_eof() {
            dims.push(self.parse_dim()?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(dims)
    }

    fn parse_dim(&mut self) -> ParseResult<Dim> {
        match self.peek_kind() {
            TokenKind::Int(n) => {
                let val = *n;
                let _ = self.advance();
                Ok(Dim::Lit(val))
            }
            TokenKind::Ident => {
                let tok = self.peek();
                let text = self.token_text(tok);
                if text == "_" {
                    let _ = self.advance();
                    return Ok(Dim::Inferred);
                }
                let ident = self.expect_ident()?;
                Ok(Dim::Var(ident))
            }
            TokenKind::EscapedIdent => {
                let ident = self.expect_ident()?;
                Ok(Dim::Var(ident))
            }
            _ => Err(self.err_expected_token("dimension (integer, identifier, or '_')")),
        }
    }

    fn merge_ty_spans(&self, left: TyId, right: TyId) -> Span {
        self.ast
            .types
            .get(left)
            .span
            .to(self.ast.types.get(right).span)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
