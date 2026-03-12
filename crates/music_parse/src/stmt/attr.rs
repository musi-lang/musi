//! Attribute parsing.

use music_ast::attr::{Attr, AttrField, AttrValue};
use music_lex::token::TokenKind;

use crate::parser::Parser;

impl Parser<'_> {
    /// Parses `{ '#[' attr_body ']' }`.
    pub(crate) fn parse_attrs(&mut self) -> Vec<Attr> {
        let mut attrs = vec![];
        while self.at(TokenKind::HashLBracket) {
            let start = self.start_span();
            let _hlb = self.bump();
            let name = self.expect_symbol();
            let value = if self.eat(TokenKind::ColonEq) {
                Some(self.parse_attr_value())
            } else if self.at(TokenKind::LParen) {
                Some(self.parse_attr_named_params())
            } else {
                None
            };
            let _rb = self.expect(TokenKind::RBracket);
            attrs.push(Attr {
                name,
                value,
                span: self.finish_span(start),
            });
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

    fn parse_attr_named_params(&mut self) -> AttrValue {
        let start = self.start_span();
        let _lp = self.bump();
        let fields = self.comma_sep(TokenKind::RParen, Self::parse_attr_named_field);
        let _rp = self.expect(TokenKind::RParen);
        AttrValue::Named {
            fields,
            span: self.finish_span(start),
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
