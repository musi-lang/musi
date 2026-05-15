use super::*;

impl Parser<'_> {
    pub(crate) fn parse_literal_expr(&mut self) -> SyntaxNodeId {
        let literal = self.advance_element();
        self.node1(SyntaxNodeKind::LiteralExpr, literal)
    }

    pub(crate) fn parse_template_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        match self.peek_kind() {
            TokenKind::TemplateNoSubst => {
                children.push(self.advance_element());
                Ok(self.node(SyntaxNodeKind::TemplateExpr, children))
            }
            TokenKind::TemplateHead => {
                children.push(self.advance_element());
                loop {
                    children.push(SyntaxElementId::Node(self.parse_expr(0)?));
                    match self.peek_kind() {
                        TokenKind::TemplateMiddle => children.push(self.advance_element()),
                        TokenKind::TemplateTail => {
                            children.push(self.advance_element());
                            break;
                        }
                        _ => return Err(self.expected_expression()),
                    }
                }
                Ok(self.node(SyntaxNodeKind::TemplateExpr, children))
            }
            _ => Err(self.expected_expression()),
        }
    }

    pub(crate) fn parse_name_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let name = self.expect_name_element()?;
        Ok(self.node1(SyntaxNodeKind::NameExpr, name))
    }

    pub(crate) fn parse_pi_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LParen)?;
        let ident = self.expect_ident_element()?;
        let colon = self.expect_token(TokenKind::Colon)?;
        let param = self.parse_expr(0)?;
        let close = self.expect_token(TokenKind::RParen)?;
        let arrow = if self.at(TokenKind::MinusGt) {
            self.advance_element()
        } else {
            return Err(self.expected_token(TokenKind::MinusGt));
        };
        let ret = self.parse_expr(ARROW_BP)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::PiExpr,
            vec![
                open,
                ident,
                colon,
                SyntaxElementId::Node(param),
                close,
                arrow,
                SyntaxElementId::Node(ret),
            ],
        ))
    }

    pub(crate) fn parse_lambda_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let backslash = self.expect_token(TokenKind::Backslash)?;
        let open = self.expect_token(TokenKind::LParen)?;
        let params = self.parse_param_list_contents(TokenKind::RParen)?;
        let close = self.expect_token(TokenKind::RParen)?;
        let param_list = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
        let mut children = vec![backslash, SyntaxElementId::Node(param_list)];
        if let Some(colon) = self.eat(TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        children.push(self.expect_token(TokenKind::EqGt)?);
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::LambdaExpr, children))
    }

    pub(crate) fn parse_paren_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LParen)?;
        if self.at(TokenKind::RParen) {
            let close = self.advance_element();
            return Ok(self.node2(SyntaxNodeKind::TupleExpr, open, close));
        }
        if self.at(TokenKind::Comma) {
            let comma = self.advance_element();
            let close = self.expect_token(TokenKind::RParen)?;
            return Ok(self.node3(SyntaxNodeKind::TupleExpr, open, comma, close));
        }
        if self.at(TokenKind::Semicolon) {
            let semi = self.advance_element();
            let close = self.expect_token(TokenKind::RParen)?;
            return Ok(self.node3(SyntaxNodeKind::SequenceExpr, open, semi, close));
        }

        let first = self.parse_expr(0)?;
        if self.at(TokenKind::Comma) {
            let mut children = vec![open, SyntaxElementId::Node(first)];
            while let Some(comma) = self.eat(TokenKind::Comma) {
                children.push(comma);
                if self.at(TokenKind::RParen) {
                    break;
                }
                children.push(SyntaxElementId::Node(self.parse_expr(0)?));
            }
            children.push(self.expect_token(TokenKind::RParen)?);
            return Ok(self.node(SyntaxNodeKind::TupleExpr, children));
        }
        if self.at(TokenKind::Semicolon) {
            let mut children = vec![open, SyntaxElementId::Node(first)];
            while let Some(semi) = self.eat(TokenKind::Semicolon) {
                children.push(semi);
                if self.at(TokenKind::RParen) {
                    break;
                }
                children.push(SyntaxElementId::Node(self.parse_expr(0)?));
            }
            children.push(self.expect_token(TokenKind::RParen)?);
            return Ok(self.node(SyntaxNodeKind::SequenceExpr, children));
        }
        let close = self.expect_token(TokenKind::RParen)?;
        Ok(self.rewrap_node(first, vec![open, SyntaxElementId::Node(first), close]))
    }

    pub(crate) fn parse_array_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_punctuated_wrapped_nodes(
            SyntaxNodeKind::ArrayExpr,
            TokenKind::LBracket,
            TokenKind::Comma,
            TokenKind::RBracket,
            Parser::parse_array_item,
        )
    }

    pub(crate) fn parse_stack_effect_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LBracket)?;
        let mut children = vec![open];
        if !self.at(TokenKind::Semicolon) {
            children.extend(self.parse_separated_nodes(
                TokenKind::Comma,
                TokenKind::Semicolon,
                Parser::parse_type_expr_node,
            )?);
        }
        children.push(self.expect_token(TokenKind::Semicolon)?);
        if !self.at(TokenKind::RBracket) {
            children.extend(self.parse_separated_nodes(
                TokenKind::Comma,
                TokenKind::RBracket,
                Parser::parse_type_expr_node,
            )?);
        }
        children.push(self.expect_token(TokenKind::RBracket)?);
        Ok(self.node(SyntaxNodeKind::StackEffectExpr, children))
    }

    fn parse_array_item(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        if let Some(spread) = self.eat(TokenKind::DotDotDot) {
            children.push(spread);
        }
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self.node(SyntaxNodeKind::ArrayItem, children))
    }

    pub(super) fn parse_record_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_punctuated_wrapped_nodes(
            SyntaxNodeKind::RecordExpr,
            TokenKind::LBrace,
            TokenKind::Comma,
            TokenKind::RBrace,
            Parser::parse_record_item,
        )
    }

    pub(crate) fn parse_dot_prefix_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_variant_expr_like(SyntaxNodeKind::VariantExpr)
    }

    pub(crate) fn parse_match_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let match_kw = self.expect_token(TokenKind::KwMatch)?;
        let scrutinee = self.parse_expr(0)?;
        let open = self.expect_token(TokenKind::LParen)?;
        let mut children = vec![match_kw, SyntaxElementId::Node(scrutinee), open];
        children.extend(self.parse_piped_nodes(TokenKind::RParen, Parser::parse_match_arm)?);
        children.push(self.expect_token(TokenKind::RParen)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::MatchExpr, children))
    }

    pub(crate) fn parse_if_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let if_kw = self.expect_token(TokenKind::KwIf)?;
        let condition = self.parse_expr(0)?;
        let then_kw = self.expect_token(TokenKind::KwThen)?;
        let then_expr = self.parse_expr(0)?;
        let else_kw = self.expect_token(TokenKind::KwElse)?;
        let else_expr = self.parse_expr(0)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::IfExpr,
            vec![
                if_kw,
                SyntaxElementId::Node(condition),
                then_kw,
                SyntaxElementId::Node(then_expr),
                else_kw,
                SyntaxElementId::Node(else_expr),
            ],
        ))
    }

    fn parse_match_arm(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        children.push(SyntaxElementId::Node(self.parse_pattern()?));
        if let Some(where_kw) = self.eat(TokenKind::KwWhere) {
            children.push(where_kw);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        children.push(self.expect_token(TokenKind::EqGt)?);
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::MatchArm, children))
    }

    pub(crate) fn parse_yield_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let yield_kw = self.expect_token(TokenKind::KwYield)?;
        let expr = self.parse_expr(PREFIX_BP)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::YieldExpr,
            vec![yield_kw, SyntaxElementId::Node(expr)],
        ))
    }

    pub(crate) fn parse_defer_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let defer_kw = self.expect_token(TokenKind::KwDefer)?;
        let expr = self.parse_expr(0)?;
        let mut children = vec![defer_kw, SyntaxElementId::Node(expr)];
        if let Some(where_kw) = self.eat(TokenKind::KwWhere) {
            children.push(where_kw);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::DeferExpr, children))
    }

    pub(crate) fn parse_import_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let import = self.expect_token(TokenKind::KwImport)?;
        let expr = self.parse_expr(PREFIX_BP)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::ImportExpr,
            vec![import, SyntaxElementId::Node(expr)],
        ))
    }

    pub(crate) fn parse_unsafe_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let unsafe_kw = self.expect_token(TokenKind::KwUnsafe)?;
        let body = self.parse_paren_expr()?;
        let children = vec![unsafe_kw, SyntaxElementId::Node(body)];
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::UnsafeExpr, children))
    }

    pub(crate) fn parse_pin_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let pin_kw = self.expect_token(TokenKind::KwPin)?;
        let pinned_expr = self.parse_expr(0)?;
        let as_kw = self.expect_token(TokenKind::KwAs)?;
        let name = self.expect_ident_element()?;
        let in_kw = self.expect_token(TokenKind::KwIn)?;
        let body = self.parse_expr(0)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::PinExpr,
            vec![
                pin_kw,
                SyntaxElementId::Node(pinned_expr),
                as_kw,
                name,
                in_kw,
                SyntaxElementId::Node(body),
            ],
        ))
    }
}
