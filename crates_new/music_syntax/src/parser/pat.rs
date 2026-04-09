use super::*;

impl Parser<'_> {
    pub(super) fn parse_pattern(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut left = self.parse_pattern_as()?;
        while let Some(or_kw) = self.eat(TokenKind::KwOr) {
            let right = self.parse_pattern_as()?;
            left = self.builder.push_node_from_children(
                SyntaxNodeKind::OrPat,
                vec![
                    SyntaxElementId::Node(left),
                    or_kw,
                    SyntaxElementId::Node(right),
                ],
            );
        }
        Ok(left)
    }

    fn parse_pattern_as(&mut self) -> ParseResult<SyntaxNodeId> {
        let primary = self.parse_pattern_primary()?;
        if let Some(as_kw) = self.eat(TokenKind::KwAs) {
            let ident = self.expect_ident_element()?;
            return Ok(self.builder.push_node_from_children(
                SyntaxNodeKind::AsPat,
                vec![SyntaxElementId::Node(primary), as_kw, ident],
            ));
        }
        Ok(primary)
    }

    fn parse_pattern_primary(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::Underscore => {
                let token = self.advance_element();
                Ok(self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::WildcardPat, vec![token]))
            }
            TokenKind::Int
            | TokenKind::Float
            | TokenKind::String
            | TokenKind::Rune
            | TokenKind::TemplateNoSubst => {
                let token = self.advance_element();
                Ok(self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::LiteralPat, vec![token]))
            }
            TokenKind::Ident => {
                let ident = self.advance_element();
                Ok(self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::BindPat, vec![ident]))
            }
            TokenKind::Dot => self.parse_variant_pattern(),
            TokenKind::LBrace => self.parse_record_pattern(),
            TokenKind::LParen => self.parse_tuple_pattern(),
            TokenKind::LBracket => self.parse_array_pattern(),
            _ => Err(self.expected_pattern()),
        }
    }

    fn parse_variant_pattern(&mut self) -> ParseResult<SyntaxNodeId> {
        let dot = self.expect_token(TokenKind::Dot)?;
        let ident = self.expect_ident_element()?;
        let mut children = vec![dot, ident];
        if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            children.extend(self.parse_separated_nodes(
                TokenKind::Comma,
                TokenKind::RParen,
                Parser::parse_pattern,
            )?);
            children.push(self.expect_token(TokenKind::RParen)?);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::VariantPat, children))
    }

    fn parse_record_pattern(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LBrace)?;
        let mut children = vec![open];
        if !self.at(TokenKind::RBrace) {
            loop {
                children.push(self.expect_ident_element()?);
                if let Some(colon) = self.eat(TokenKind::Colon) {
                    children.push(colon);
                    children.push(SyntaxElementId::Node(self.parse_pattern()?));
                }
                if let Some(comma) = self.eat(TokenKind::Comma) {
                    children.push(comma);
                    if self.at(TokenKind::RBrace) {
                        break;
                    }
                    continue;
                }
                break;
            }
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordPat, children))
    }

    fn parse_tuple_pattern(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_wrapped_nodes(
            SyntaxNodeKind::TuplePat,
            TokenKind::LParen,
            TokenKind::Comma,
            TokenKind::RParen,
            Parser::parse_pattern,
        )
    }

    fn parse_array_pattern(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_wrapped_nodes(
            SyntaxNodeKind::ArrayPat,
            TokenKind::LBracket,
            TokenKind::Comma,
            TokenKind::RBracket,
            Parser::parse_pattern,
        )
    }
}
