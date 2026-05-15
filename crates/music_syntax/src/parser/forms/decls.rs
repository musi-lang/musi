use super::*;

const fn is_receiver_ident(kind: TokenKind) -> bool {
    matches!(kind, TokenKind::Ident)
}

impl Parser<'_> {
    pub(crate) fn parse_let_expr(
        &mut self,
        mut attrs: SyntaxElementList,
    ) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(TokenKind::KwLet)?);
        if self.at(TokenKind::KwRecur) {
            attrs.push(self.advance_element());
        }
        if self.at_receiver_method_head() {
            attrs.push(SyntaxElementId::Node(self.parse_receiver_method_head()?));
        } else {
            attrs.push(SyntaxElementId::Node(self.parse_pattern()?));
        }
        self.parse_optional_type_params_clause(&mut attrs)?;
        self.parse_optional_param_clause(&mut attrs)?;
        self.parse_optional_typed_expr(&mut attrs)?;
        self.parse_optional_constraints_clause(&mut attrs)?;
        if let Some(bind) = self.eat(TokenKind::ColonEq) {
            attrs.push(bind);
            attrs.push(SyntaxElementId::Node(self.parse_expr(0)?));
            if let Some(else_kw) = self.eat(TokenKind::KwElse) {
                attrs.push(else_kw);
                attrs.push(SyntaxElementId::Node(self.parse_expr(0)?));
            }
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::LetExpr, attrs))
    }
}

impl Parser<'_> {
    fn at_receiver_method_head(&self) -> bool {
        if self.peek_kind() != TokenKind::LParen || !is_receiver_ident(self.nth_kind(1)) {
            return false;
        }
        if self.nth_kind(2) != TokenKind::Colon {
            return false;
        }
        let mut depth = 0usize;
        let mut offset = 0usize;
        loop {
            match self.nth_kind(offset) {
                TokenKind::LParen => depth += 1,
                TokenKind::RParen => {
                    depth = depth.saturating_sub(1);
                    if depth == 0 {
                        return self.nth_kind(offset + 1) == TokenKind::Dot
                            && is_receiver_ident(self.nth_kind(offset + 2));
                    }
                }
                TokenKind::Eof => return false,
                _ => {}
            }
            offset += 1;
        }
    }

    fn parse_receiver_method_head(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![self.expect_token(TokenKind::LParen)?];
        children.push(self.expect_ident_element()?);
        children.push(self.expect_token(TokenKind::Colon)?);
        children.push(SyntaxElementId::Node(self.parse_type_expr(0)?));
        children.push(self.expect_token(TokenKind::RParen)?);
        children.push(self.expect_token(TokenKind::Dot)?);
        children.push(self.expect_ident_element()?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ReceiverMethodHead, children))
    }

    pub(crate) fn parse_data_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let data_kw = self.expect_token(TokenKind::KwData)?;
        let open = self.expect_token(TokenKind::LBrace)?;
        let mut children = vec![data_kw, open];
        if self.at(TokenKind::Pipe) {
            children.push(self.advance_element());
            if !self.at(TokenKind::RBrace) {
                children.push(SyntaxElementId::Node(self.parse_variant_list()?));
            }
        } else if self.at(TokenKind::Semicolon) {
            children.push(self.advance_element());
            if !self.at(TokenKind::RBrace) {
                children.push(SyntaxElementId::Node(self.parse_field_list()?));
            }
        } else if !self.at(TokenKind::RBrace) {
            children.push(SyntaxElementId::Node(self.parse_field_def()?));
            while let Some(semi) = self.eat(TokenKind::Semicolon) {
                children.push(semi);
                if self.at(TokenKind::RBrace) {
                    break;
                }
                children.push(SyntaxElementId::Node(self.parse_field_def()?));
            }
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::DataExpr, children))
    }

    fn parse_variant_list(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_node_list(
            SyntaxNodeKind::VariantList,
            TokenKind::Pipe,
            TokenKind::RBrace,
            Parser::parse_variant_def,
        )
    }

    fn parse_variant_def(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        children.push(self.expect_ident_element()?);
        if self.at(TokenKind::LParen) {
            children.push(SyntaxElementId::Node(
                self.parse_variant_payload_list(Parser::parse_variant_payload_def_item)?,
            ));
        }
        if let Some(arrow) = self.eat(TokenKind::MinusGt) {
            children.push(arrow);
            children.push(SyntaxElementId::Node(self.parse_type_expr(0)?));
        }
        self.parse_optional_bound_expr(&mut children)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Variant, children))
    }

    fn parse_variant_payload_def_item(&mut self) -> ParseResult<SyntaxNodeId> {
        if self.peek_kind() == TokenKind::Ident && self.nth_kind(1) == TokenKind::Colon {
            let ident = self.expect_ident_element()?;
            let mut children = vec![ident];
            self.parse_required_typed_expr(&mut children)?;
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::VariantFieldDef, children));
        }
        self.parse_expr_node()
    }

    fn parse_field_list(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_node_list(
            SyntaxNodeKind::FieldList,
            TokenKind::Semicolon,
            TokenKind::RBrace,
            Parser::parse_field_def,
        )
    }

    fn parse_field_def(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        children.push(self.expect_token(TokenKind::KwLet)?);
        let ident = self.expect_ident_element()?;
        children.push(ident);
        self.parse_required_typed_expr(&mut children)?;
        self.parse_optional_bound_expr(&mut children)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Field, children))
    }
}

impl Parser<'_> {
    pub(crate) fn parse_shape_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let shape = self.expect_token(TokenKind::KwShape)?;
        let mut children = vec![shape];
        if self.at(TokenKind::KwWhere) {
            children.push(self.advance_element());
            children.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        self.parse_member_body(&mut children)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ShapeExpr, children))
    }

    pub(crate) fn parse_with_mods_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        let mut has_visibility_mod = false;
        while self.at(TokenKind::At) || self.at(TokenKind::KwExport) || self.at(TokenKind::KwHidden)
        {
            if self.at(TokenKind::At) {
                children.push(SyntaxElementId::Node(self.parse_attr()?));
            } else if self.at(TokenKind::KwHidden) {
                has_visibility_mod = true;
                children.push(self.advance_element());
            } else {
                has_visibility_mod = true;
                children.push(SyntaxElementId::Node(self.parse_export_mod()?));
            }
        }
        if has_visibility_mod && !self.at(TokenKind::KwLet) {
            return Err(self.expected_token(TokenKind::KwLet));
        }
        let expr = match self.peek_kind() {
            TokenKind::KwLet => self.parse_let_expr(Vec::new())?,
            _ => self.parse_expr(PREFIX_BP)?,
        };
        children.push(SyntaxElementId::Node(expr));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::AttributedExpr, children))
    }

    fn parse_export_mod(&mut self) -> ParseResult<SyntaxNodeId> {
        let children = vec![self.expect_token(TokenKind::KwExport)?];
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ExportMod, children))
    }
}

impl Parser<'_> {
    pub(crate) fn parse_member(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        match self.peek_kind() {
            TokenKind::KwLet => {
                children.push(self.advance_element());
                children.extend(self.parse_op_or_ident_name()?);
                self.parse_optional_param_clause(&mut children)?;
                self.parse_optional_typed_expr(&mut children)?;
                self.parse_optional_constraints_clause(&mut children)?;
                self.parse_optional_bound_expr(&mut children)?;
            }
            _ => return Err(self.expected_member()),
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Member, children))
    }
}
