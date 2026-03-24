use music_ast::PatId;
use music_ast::pat::{PatKind, RecordPatField};
use music_found::Literal;
use music_lex::TokenKind;

use crate::errors::{ParseError, ParseErrorKind, ParseResult, describe_token};
use crate::parser::Parser;

impl Parser<'_> {
    pub(crate) fn parse_pat(&mut self) -> ParseResult<PatId> {
        let first = self.parse_pat_primary()?;
        if !self.at(&TokenKind::KwOr) {
            return Ok(first);
        }
        let mut alts = vec![first];
        while self.eat(&TokenKind::KwOr) {
            alts.push(self.parse_pat_primary()?);
        }
        let start = self.ast.pats.get(first).span;
        let end = self
            .ast
            .pats
            .get(*alts.last().expect("at least two alternatives"))
            .span;
        let span = start.to(end);
        Ok(self.alloc_pat(PatKind::Or(alts), span))
    }

    fn parse_pat_primary(&mut self) -> ParseResult<PatId> {
        match self.peek_kind() {
            TokenKind::Int(_) | TokenKind::Float(_) | TokenKind::Str(_) | TokenKind::Rune(_) => {
                self.parse_pat_lit()
            }
            TokenKind::Ident | TokenKind::EscapedIdent => self.parse_pat_ident(),
            TokenKind::Dot => self.parse_pat_variant(),
            TokenKind::DotLBrace => self.parse_pat_record(),
            TokenKind::LParen => self.parse_pat_delimited(
                &TokenKind::LParen,
                "'('",
                &TokenKind::RParen,
                "')'",
                PatKind::Tuple,
            ),
            TokenKind::LBracket => self.parse_pat_delimited(
                &TokenKind::LBracket,
                "'['",
                &TokenKind::RBracket,
                "']'",
                PatKind::Array,
            ),
            _ => Err(self.err_expected_pat()),
        }
    }

    fn parse_pat_lit(&mut self) -> ParseResult<PatId> {
        let token = self.advance();
        let span = token.span;
        let lit = match &token.kind {
            TokenKind::Int(n) => Literal::Int(*n),
            TokenKind::Float(f) => Literal::Float(*f),
            TokenKind::Str(s) => Literal::Str(s.clone()),
            TokenKind::Rune(c) => Literal::Rune(*c),
            _ => {
                return Err(ParseError {
                    kind: ParseErrorKind::ExpectedPat {
                        found: describe_token(&token.kind),
                    },
                    span,
                    context: None,
                });
            }
        };
        Ok(self.alloc_pat(PatKind::Lit(lit), span))
    }

    fn parse_pat_ident(&mut self) -> ParseResult<PatId> {
        let span = self.peek().span;
        if self.eat_wildcard() {
            return Ok(self.alloc_pat(PatKind::Wildcard, span));
        }
        let ident = self.expect_ident()?;
        let span = ident.span;
        if self.eat(&TokenKind::KwAs) {
            let sub = self.parse_pat_primary()?;
            let end = self.ast.pats.get(sub).span;
            let full_span = span.to(end);
            return Ok(self.alloc_pat(
                PatKind::As {
                    name: ident,
                    pat: sub,
                },
                full_span,
            ));
        }
        Ok(self.alloc_pat(PatKind::Bind(ident), span))
    }

    fn parse_pat_variant(&mut self) -> ParseResult<PatId> {
        let start = self.expect(&TokenKind::Dot, "'.'")?;
        let tag = self.expect_ident()?;
        let fields = if self.eat(&TokenKind::LParen) {
            let pats = self.parse_pat_list(&TokenKind::RParen)?;
            let _ = self.expect(&TokenKind::RParen, "')'")?;
            pats
        } else {
            Vec::new()
        };
        let span = start.to(self.prev_span());
        Ok(self.alloc_pat(PatKind::Variant { tag, fields }, span))
    }

    fn parse_pat_record(&mut self) -> ParseResult<PatId> {
        let start = self.expect(&TokenKind::DotLBrace, "'.{'")?;
        let mut fields = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at_eof() {
            fields.push(self.parse_rec_pat_field()?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        let end = self.expect(&TokenKind::RBrace, "'}'")?;
        let span = start.to(end);
        Ok(self.alloc_pat(PatKind::Record(fields), span))
    }

    fn parse_rec_pat_field(&mut self) -> ParseResult<RecordPatField> {
        let mutable = self.eat(&TokenKind::KwMut);
        let name = self.expect_ident()?;
        let pat = if self.eat(&TokenKind::Colon) {
            Some(self.parse_pat()?)
        } else {
            None
        };
        Ok(RecordPatField { mutable, name, pat })
    }

    fn parse_pat_delimited(
        &mut self,
        open: &TokenKind,
        open_str: &'static str,
        close: &TokenKind,
        close_str: &'static str,
        make: fn(Vec<PatId>) -> PatKind,
    ) -> ParseResult<PatId> {
        let start = self.expect(open, open_str)?;
        let pats = self.parse_pat_list(close)?;
        let end = self.expect(close, close_str)?;
        let span = start.to(end);
        Ok(self.alloc_pat(make(pats), span))
    }

    fn parse_pat_list(&mut self, terminator: &TokenKind) -> ParseResult<Vec<PatId>> {
        let mut pats = Vec::new();
        while !self.at(terminator) && !self.at_eof() {
            pats.push(self.parse_pat()?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(pats)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
