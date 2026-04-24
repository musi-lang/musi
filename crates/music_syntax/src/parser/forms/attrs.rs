use super::*;

impl Parser<'_> {
    pub(crate) fn parse_attrs(&mut self) -> ParseResult<SyntaxElementList> {
        let mut children = Vec::new();
        while self.at(TokenKind::At) {
            children.push(SyntaxElementId::Node(self.parse_attr()?));
        }
        Ok(children)
    }

    pub(crate) fn parse_attr(&mut self) -> ParseResult<SyntaxNodeId> {
        let at = self.expect_token(TokenKind::At)?;
        let mut children = vec![at, self.expect_attr_name_element()?];
        while let Some(dot) = self.eat(TokenKind::Dot) {
            children.push(dot);
            children.push(self.expect_attr_name_element()?);
        }
        if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            if !self.at(TokenKind::RParen) {
                children.push(SyntaxElementId::Node(self.parse_attr_arg()?));
                while let Some(comma) = self.eat(TokenKind::Comma) {
                    children.push(comma);
                    if self.at(TokenKind::RParen) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_attr_arg()?));
                }
            }
            children.push(self.expect_token(TokenKind::RParen)?);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Attr, children))
    }

    fn expect_attr_name_element(&mut self) -> ParseResult<SyntaxElementId> {
        if matches!(self.peek_kind(), TokenKind::Ident | TokenKind::KwKnown) {
            Ok(self.advance_element())
        } else {
            self.expect_ident_element()
        }
    }

    fn parse_attr_arg(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        if matches!(self.peek_kind(), TokenKind::Ident) && self.nth_kind(1) == TokenKind::ColonEq {
            children.push(self.advance_element());
            children.push(self.advance_element());
        }
        children.push(SyntaxElementId::Node(self.parse_attr_value()?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::AttrArg, children))
    }

    pub(crate) fn parse_attr_value(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::String | TokenKind::Int | TokenKind::Rune => Ok(self.parse_literal_expr()),
            TokenKind::Dot => self.parse_attr_variant(),
            TokenKind::LBracket => self.parse_attr_array(),
            TokenKind::LBrace => self.parse_attr_record(),
            _ => Err(self.expected_attr_value()),
        }
    }

    fn parse_attr_variant(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_variant_like(SyntaxNodeKind::VariantExpr, Parser::parse_attr_value_node)
    }

    fn parse_attr_array(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_wrapped_nodes(
            SyntaxNodeKind::ArrayExpr,
            TokenKind::LBracket,
            TokenKind::Comma,
            TokenKind::RBracket,
            Parser::parse_attr_value_node,
        )
    }

    fn parse_attr_record(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LBrace)?;
        let mut children = vec![open];
        if !self.at(TokenKind::RBrace) {
            children.push(SyntaxElementId::Node(self.parse_attr_record_field()?));
            while let Some(comma) = self.eat(TokenKind::Comma) {
                children.push(comma);
                while let Some(extra_comma) = self.eat(TokenKind::Comma) {
                    children.push(extra_comma);
                }
                if self.at(TokenKind::RBrace) {
                    break;
                }
                children.push(SyntaxElementId::Node(self.parse_attr_record_field()?));
            }
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(self.node(SyntaxNodeKind::RecordExpr, children))
    }

    fn parse_attr_record_field(&mut self) -> ParseResult<SyntaxNodeId> {
        let ident = self.expect_ident_element()?;
        let bind = self.expect_token(TokenKind::ColonEq)?;
        let value = self.parse_attr_value()?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::RecordItem,
            vec![ident, bind, SyntaxElementId::Node(value)],
        ))
    }

    pub(crate) fn parse_param_list_contents(
        &mut self,
        close: TokenKind,
    ) -> ParseResult<SyntaxElementList> {
        self.parse_separated_nodes(TokenKind::Comma, close, Parser::parse_param)
    }

    fn parse_param(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        if let Some(comptime) = self.eat(TokenKind::KwComptime) {
            children.push(comptime);
        }
        children.push(self.expect_ident_element()?);
        self.parse_optional_typed_expr(&mut children)?;
        self.parse_optional_bound_expr(&mut children)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Param, children))
    }

    pub(crate) fn parse_type_param_list_contents(
        &mut self,
        close: TokenKind,
    ) -> ParseResult<SyntaxElementList> {
        self.parse_separated_nodes(TokenKind::Comma, close, Parser::parse_type_param)
    }

    pub(crate) fn parse_constraint_list(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![SyntaxElementId::Node(self.parse_constraint()?)];
        while let Some(comma) = self.eat(TokenKind::Comma) {
            children.push(comma);
            if self.at_any(&[TokenKind::LBrace, TokenKind::RBrace]) {
                break;
            }
            children.push(SyntaxElementId::Node(self.parse_constraint()?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ConstraintList, children))
    }

    fn parse_constraint(&mut self) -> ParseResult<SyntaxNodeId> {
        let ident = self.expect_ident_element()?;
        let op = if self.at_any(&[TokenKind::LtColon, TokenKind::Colon, TokenKind::TildeEq]) {
            self.advance_element()
        } else {
            return Err(self.expected_constraint_operator());
        };
        let expr = self.parse_type_expr(0)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::Constraint,
            vec![ident, op, SyntaxElementId::Node(expr)],
        ))
    }

    pub(crate) fn parse_effect_set(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LBrace)?;
        let mut children = vec![open];
        while let Some(comma) = self.eat(TokenKind::Comma) {
            children.push(comma);
        }
        if !self.at(TokenKind::RBrace) {
            if self.at(TokenKind::DotDotDot) {
                children.push(self.advance_element());
                children.push(self.expect_ident_element()?);
            } else {
                children.push(SyntaxElementId::Node(self.parse_effect_item()?));
                while let Some(comma) = self.eat(TokenKind::Comma) {
                    children.push(comma);
                    if self.at(TokenKind::DotDotDot) {
                        children.push(self.advance_element());
                        children.push(self.expect_ident_element()?);
                        break;
                    }
                    if self.at(TokenKind::RBrace) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_effect_item()?));
                }
            }
        }
        while let Some(comma) = self.eat(TokenKind::Comma) {
            children.push(comma);
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::EffectSet, children))
    }

    fn parse_effect_item(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![self.expect_ident_element()?];
        if self.at(TokenKind::LBracket) {
            let open = self.advance_element();
            children.push(open);
            children.push(SyntaxElementId::Node(self.parse_type_expr(0)?));
            children.push(self.expect_token(TokenKind::RBracket)?);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::EffectItem, children))
    }

    pub(crate) fn parse_expr_list(&mut self, close: TokenKind) -> ParseResult<SyntaxElementList> {
        self.parse_separated_nodes(TokenKind::Comma, close, Parser::parse_expr_node)
    }

    pub(crate) fn parse_ident_list_opt(&mut self, close: TokenKind) -> SyntaxElementList {
        let mut children = Vec::new();
        while let Some(comma) = self.eat(TokenKind::Comma) {
            children.push(comma);
        }
        if self.at(close) {
            return children;
        }
        loop {
            if self.peek_kind() == TokenKind::Ident {
                children.push(self.advance_element());
            } else {
                self.error(ParseError::new(
                    ParseErrorKind::ExpectedIdentifier {
                        found: self.found_token(),
                    },
                    self.span(),
                ));
                break;
            }
            let mut saw_separator = false;
            while let Some(comma) = self.eat(TokenKind::Comma) {
                saw_separator = true;
                children.push(comma);
            }
            if !saw_separator || self.at(close) {
                break;
            }
        }
        children
    }

    pub(crate) fn parse_op_or_ident_name(&mut self) -> ParseResult<SyntaxElementList> {
        match self.peek_kind() {
            TokenKind::Ident | TokenKind::OpIdent => Ok(vec![self.advance_element()]),
            _ => Err(self.expected_operator_member_name()),
        }
    }

    pub(crate) fn parse_type_param(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![self.expect_ident_element()?];
        self.parse_optional_typed_expr(&mut children)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::TypeParam, children))
    }
}
