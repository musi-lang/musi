//! Attribute parsing.

use msc_ast::attr::{Attr, AttrField, AttrValue};
use msc_lex::token::TokenKind;

use crate::parser::Parser;

impl Parser<'_> {
    /// Parses `{ '#[' attr_body *(',' attr_body) ']' }`.
    pub(crate) fn parse_attrs(&mut self) -> Vec<Attr> {
        let mut attrs = vec![];
        while self.at(TokenKind::HashLBracket) {
            let _hlb = self.bump();
            loop {
                let start = self.start_span();
                let name = self.expect_symbol();
                let value = if self.eat(TokenKind::ColonEq) {
                    Some(self.parse_attr_value())
                } else if self.at(TokenKind::LParen) {
                    Some(self.parse_attr_params())
                } else {
                    None
                };
                attrs.push(Attr {
                    name,
                    value,
                    span: self.finish_span(start),
                });
                if !self.eat(TokenKind::Comma) {
                    break;
                }
                // A RBracket after a comma means the bracket is closing (trailing
                // comma inside `#[a, b,]` is not valid per the grammar, but we stop
                // gracefully instead of emitting a spurious error).
                if self.at(TokenKind::RBracket) {
                    break;
                }
            }
            let _rb = self.expect(TokenKind::RBracket);
        }
        attrs
    }

    fn parse_attr_value(&mut self) -> AttrValue {
        let start = self.start_span();
        if self.at(TokenKind::LParen) {
            let _lp = self.bump();
            let lits = self.comma_sep(TokenKind::RParen, Self::parse_lit_value);
            let _rp = self.expect(TokenKind::RParen);
            return AttrValue::Tuple {
                lits,
                span: self.finish_span(start),
            };
        }
        let lit = self.parse_lit_value();
        AttrValue::Lit {
            lit,
            span: self.finish_span(start),
        }
    }

    /// Parses `'(' attr-params ')'` where params are either positional literals
    /// or named `ident ':=' lit` fields. Detection is by lookahead:
    /// - `Ident` followed by `ColonEq` → named fields.
    /// - literal token or `RParen` → positional.
    fn parse_attr_params(&mut self) -> AttrValue {
        let start = self.start_span();
        let _lp = self.bump(); // consume '('

        // Empty parens → positional with no args.
        if self.at(TokenKind::RParen) {
            let _rp = self.expect(TokenKind::RParen);
            return AttrValue::Tuple {
                lits: vec![],
                span: self.finish_span(start),
            };
        }

        // Named if: current token is an identifier AND next is ':='.
        let is_named = self.at(TokenKind::Ident) && self.peek2() == TokenKind::ColonEq;

        if is_named {
            let fields = self.comma_sep(TokenKind::RParen, Self::parse_attr_named_field);
            let _rp = self.expect(TokenKind::RParen);
            AttrValue::Named {
                fields,
                span: self.finish_span(start),
            }
        } else {
            let lits = self.comma_sep(TokenKind::RParen, Self::parse_lit_value);
            let _rp = self.expect(TokenKind::RParen);
            AttrValue::Tuple {
                lits,
                span: self.finish_span(start),
            }
        }
    }

    fn parse_attr_named_field(&mut self) -> AttrField {
        let start = self.start_span();
        let name = self.expect_symbol();
        let _ceq = self.expect(TokenKind::ColonEq);
        let value = self.parse_lit_value();
        AttrField {
            name,
            value,
            span: self.finish_span(start),
        }
    }
}
