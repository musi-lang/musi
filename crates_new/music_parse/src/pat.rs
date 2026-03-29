use music_ast::{SyntaxElementId, SyntaxNodeId, SyntaxNodeKind};
use music_lex::TokenKind;

use crate::errors::ParseResult;
use crate::parser::Parser;

impl Parser<'_, '_> {
    pub(crate) fn parse_pat(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut left = self.parse_pat_primary()?;
        while let Some(or_kw) = self.eat(&TokenKind::KwOr) {
            let right = self.parse_pat_primary()?;
            left = self.builder.push_node_from_children(
                SyntaxNodeKind::OrPat,
                [
                    SyntaxElementId::Node(left),
                    or_kw,
                    SyntaxElementId::Node(right),
                ],
            );
        }
        Ok(left)
    }

    fn parse_pat_primary(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::Ident | TokenKind::EscapedIdent => self.parse_bind_pat(),
            TokenKind::IntLit
            | TokenKind::FloatLit
            | TokenKind::StringLit
            | TokenKind::FStringLit(_)
            | TokenKind::RuneLit => Ok(self.parse_literal_pat()),
            TokenKind::Dot => self.parse_variant_pat(),
            TokenKind::DotLBrace => self.parse_record_pat(),
            TokenKind::LParen => self.parse_tuple_pat(),
            TokenKind::LBracket => self.parse_array_pat(),
            _ => Err(self.expected_pattern()),
        }
    }

    fn parse_bind_pat(&mut self) -> ParseResult<SyntaxNodeId> {
        let is_wildcard = self.lexed.token_text(self.pos) == "_";
        let ident = self.expect_ident_element()?;
        if is_wildcard {
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::WildcardPat, [ident]));
        }

        let mut children = vec![ident];
        if let Some(as_kw) = self.eat(&TokenKind::KwAs) {
            children.push(as_kw);
            let pat = self.parse_pat_primary()?;
            children.push(SyntaxElementId::Node(pat));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::BindPat, children))
    }

    fn parse_literal_pat(&mut self) -> SyntaxNodeId {
        let token = self.advance_element();
        self.builder
            .push_node_from_children(SyntaxNodeKind::LiteralPat, [token])
    }

    fn parse_variant_pat(&mut self) -> ParseResult<SyntaxNodeId> {
        let dot = self.expect_token(&TokenKind::Dot)?;
        let ident = self.expect_ident_element()?;
        let mut children = vec![dot, ident];
        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            if !self.at(&TokenKind::RParen) {
                children.push(SyntaxElementId::Node(self.parse_pat()?));
                while let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                    if self.at(&TokenKind::RParen) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_pat()?));
                }
            }
            let close = self.expect_token(&TokenKind::RParen)?;
            children.push(close);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::VariantPat, children))
    }

    fn parse_record_pat(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::DotLBrace)?;
        let mut children = vec![open];
        if !self.at(&TokenKind::RBrace) {
            loop {
                if let Some(mut_kw) = self.eat(&TokenKind::KwMut) {
                    children.push(mut_kw);
                }
                children.push(self.expect_ident_element()?);
                if let Some(colon) = self.eat(&TokenKind::Colon) {
                    children.push(colon);
                    children.push(SyntaxElementId::Node(self.parse_pat()?));
                }
                if let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                    if self.at(&TokenKind::RBrace) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordPat, children))
    }

    fn parse_tuple_pat(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LParen)?;
        let mut children = vec![open];
        if !self.at(&TokenKind::RParen) {
            children.push(SyntaxElementId::Node(self.parse_pat()?));
            while let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                if self.at(&TokenKind::RParen) {
                    break;
                }
                children.push(SyntaxElementId::Node(self.parse_pat()?));
            }
        }
        let close = self.expect_token(&TokenKind::RParen)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::TuplePat, children))
    }

    fn parse_array_pat(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LBracket)?;
        let mut children = vec![open];
        if !self.at(&TokenKind::RBracket) {
            children.push(SyntaxElementId::Node(self.parse_pat()?));
            while let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                if self.at(&TokenKind::RBracket) {
                    break;
                }
                children.push(SyntaxElementId::Node(self.parse_pat()?));
            }
        }
        let close = self.expect_token(&TokenKind::RBracket)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ArrayPat, children))
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
