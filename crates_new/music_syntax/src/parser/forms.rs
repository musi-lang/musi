use super::*;

impl Parser<'_> {
    fn parse_punctuated_nodes<F>(
        &mut self,
        sep: TokenKind,
        end: TokenKind,
        mut parse_item: F,
    ) -> ParseResult<SyntaxElementList>
    where
        F: FnMut(&mut Self) -> ParseResult<SyntaxNodeId>,
    {
        let mut children = Vec::new();
        while let Some(sep) = self.eat(sep) {
            children.push(sep);
        }
        while !self.at(end) && !self.at(TokenKind::Eof) {
            children.push(SyntaxElementId::Node(parse_item(self)?));
            if let Some(sep) = self.eat(sep) {
                children.push(sep);
                continue;
            }
            break;
        }
        while let Some(sep) = self.eat(sep) {
            children.push(sep);
        }
        Ok(children)
    }

    pub(super) fn parse_separated_nodes<F>(
        &mut self,
        sep: TokenKind,
        end: TokenKind,
        mut parse_item: F,
    ) -> ParseResult<SyntaxElementList>
    where
        F: FnMut(&mut Self) -> ParseResult<SyntaxNodeId>,
    {
        let mut children = Vec::new();
        if self.at(end) {
            return Ok(children);
        }
        loop {
            children.push(SyntaxElementId::Node(parse_item(self)?));
            if let Some(sep) = self.eat(sep) {
                children.push(sep);
                if self.at(end) {
                    break;
                }
                continue;
            }
            break;
        }
        Ok(children)
    }

    pub(super) fn parse_wrapped_nodes<F>(
        &mut self,
        kind: SyntaxNodeKind,
        open_kind: TokenKind,
        sep: TokenKind,
        close_kind: TokenKind,
        parse_item: F,
    ) -> ParseResult<SyntaxNodeId>
    where
        F: FnMut(&mut Self) -> ParseResult<SyntaxNodeId>,
    {
        let open = self.expect_token(open_kind)?;
        let mut children = vec![open];
        children.extend(self.parse_separated_nodes(sep, close_kind, parse_item)?);
        children.push(self.expect_token(close_kind)?);
        Ok(self.node(kind, children))
    }

    fn parse_piped_nodes<F>(
        &mut self,
        close: TokenKind,
        mut parse_item: F,
    ) -> ParseResult<SyntaxElementList>
    where
        F: FnMut(&mut Self) -> ParseResult<SyntaxNodeId>,
    {
        let mut children = Vec::new();
        if let Some(pipe) = self.eat(TokenKind::Pipe) {
            children.push(pipe);
        }
        while !self.at(close) && !self.at(TokenKind::Eof) {
            children.push(SyntaxElementId::Node(parse_item(self)?));
            if let Some(pipe) = self.eat(TokenKind::Pipe) {
                children.push(pipe);
                if self.at(close) {
                    break;
                }
                continue;
            }
            break;
        }
        Ok(children)
    }

    fn parse_member_body(&mut self, children: &mut SyntaxElementList) -> ParseResult<()> {
        children.push(self.expect_token(TokenKind::LBrace)?);
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            children.push(SyntaxElementId::Node(self.parse_member()?));
            let _ = self.eat(TokenKind::Semicolon);
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(())
    }

    pub(super) fn parse_atom_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::Int | TokenKind::Float | TokenKind::String | TokenKind::Rune => {
                Ok(self.parse_literal_expr())
            }
            TokenKind::TemplateNoSubst | TokenKind::TemplateHead => self.parse_template_expr(),
            TokenKind::Ident | TokenKind::OpIdent => self.parse_name_expr(),
            TokenKind::Hash => self.parse_splice_expr(),
            TokenKind::LParen => {
                if self.is_pi_paren() {
                    self.parse_pi_expr()
                } else if self.is_lambda_paren() {
                    self.parse_lambda_expr()
                } else {
                    self.parse_paren_expr()
                }
            }
            TokenKind::LBracket => {
                if self.is_array_type_expr() {
                    self.parse_array_type_expr()
                } else {
                    self.parse_array_expr()
                }
            }
            TokenKind::LBrace => self.parse_record_expr(),
            TokenKind::Dot => self.parse_dot_prefix_expr(),
            TokenKind::KwCase => self.parse_case_expr(),
            TokenKind::KwLet => self.parse_let_expr(Vec::new()),
            TokenKind::KwResume => self.parse_resume_expr(),
            TokenKind::KwImport => self.parse_import_expr(),
            TokenKind::KwData => self.parse_data_expr(),
            TokenKind::KwEffect => self.parse_effect_expr(),
            TokenKind::KwClass => self.parse_class_expr(),
            TokenKind::KwInstance => self.parse_instance_expr(Vec::new()),
            TokenKind::KwPerform => self.parse_perform_expr(),
            TokenKind::KwHandle => self.parse_handle_expr(),
            TokenKind::KwForeign => self.parse_foreign_expr(Vec::new()),
            TokenKind::KwQuote => self.parse_quote_expr(),
            TokenKind::KwExport => self.parse_export_expr(Vec::new()),
            TokenKind::At => self.parse_with_attrs_expr(),
            _ => Err(self.expected_expression()),
        }
    }

    fn parse_literal_expr(&mut self) -> SyntaxNodeId {
        let literal = self.advance_element();
        self.node1(SyntaxNodeKind::LiteralExpr, literal)
    }

    fn parse_template_expr(&mut self) -> ParseResult<SyntaxNodeId> {
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

    fn parse_name_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let name = self.expect_name_element()?;
        Ok(self.node1(SyntaxNodeKind::NameExpr, name))
    }

    fn parse_pi_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LParen)?;
        let ident = self.expect_ident_element()?;
        let colon = self.expect_token(TokenKind::Colon)?;
        let param = self.parse_expr(0)?;
        let close = self.expect_token(TokenKind::RParen)?;
        let arrow = if self.at(TokenKind::MinusGt) || self.at(TokenKind::TildeGt) {
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

    fn parse_lambda_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LParen)?;
        let params = self.parse_param_list_contents(TokenKind::RParen)?;
        let close = self.expect_token(TokenKind::RParen)?;
        let param_list = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
        let mut children = vec![SyntaxElementId::Node(param_list)];
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

    fn parse_paren_expr(&mut self) -> ParseResult<SyntaxNodeId> {
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

    fn parse_array_type_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LBracket)?;
        let mut children = vec![open];
        if !self.at(TokenKind::RBracket) {
            children.push(self.parse_dim()?);
            while let Some(comma) = self.eat(TokenKind::Comma) {
                children.push(comma);
                if self.at(TokenKind::RBracket) {
                    break;
                }
                children.push(self.parse_dim()?);
            }
        }
        children.push(self.expect_token(TokenKind::RBracket)?);
        children.push(SyntaxElementId::Node(self.parse_expr(PREFIX_BP)?));
        Ok(self.node(SyntaxNodeKind::ArrayExpr, children))
    }

    fn parse_dim(&mut self) -> ParseResult<SyntaxElementId> {
        match self.peek_kind() {
            TokenKind::Int | TokenKind::Ident | TokenKind::Underscore => Ok(self.advance_element()),
            _ => Err(self.expected_array_dimension()),
        }
    }

    fn parse_array_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LBracket)?;
        let mut children = vec![open];
        children.extend(self.parse_punctuated_nodes(
            TokenKind::Comma,
            TokenKind::RBracket,
            Parser::parse_array_item,
        )?);
        children.push(self.expect_token(TokenKind::RBracket)?);
        Ok(self.node(SyntaxNodeKind::ArrayExpr, children))
    }

    fn parse_array_item(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        if let Some(spread) = self.eat(TokenKind::DotDotDot) {
            children.push(spread);
        }
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self.node(SyntaxNodeKind::ArrayItem, children))
    }

    fn parse_record_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LBrace)?;
        let mut children = vec![open];
        children.extend(self.parse_punctuated_nodes(
            TokenKind::Comma,
            TokenKind::RBrace,
            Parser::parse_record_item,
        )?);
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(self.node(SyntaxNodeKind::RecordExpr, children))
    }

    pub(super) fn parse_record_item(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        if let Some(spread) = self.eat(TokenKind::DotDotDot) {
            children.push(spread);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
            return Ok(self.node(SyntaxNodeKind::RecordItem, children));
        }
        children.push(self.expect_ident_element()?);
        if let Some(bind) = self.eat(TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self.node(SyntaxNodeKind::RecordItem, children))
    }

    fn parse_dot_prefix_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let dot = self.expect_token(TokenKind::Dot)?;
        let ident = self.expect_ident_element()?;
        let mut children = vec![dot, ident];
        if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            children.extend(self.parse_expr_list(TokenKind::RParen)?);
            children.push(self.expect_token(TokenKind::RParen)?);
        }
        Ok(self.node(SyntaxNodeKind::VariantExpr, children))
    }

    fn parse_case_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let case_kw = self.expect_token(TokenKind::KwCase)?;
        let scrutinee = self.parse_expr(0)?;
        let of_kw = self.expect_token(TokenKind::KwOf)?;
        let open = self.expect_token(TokenKind::LParen)?;
        let mut children = vec![case_kw, SyntaxElementId::Node(scrutinee), of_kw, open];
        children.extend(self.parse_piped_nodes(TokenKind::RParen, Parser::parse_case_arm)?);
        children.push(self.expect_token(TokenKind::RParen)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::CaseExpr, children))
    }

    fn parse_case_arm(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        children.push(SyntaxElementId::Node(self.parse_pattern()?));
        if let Some(if_kw) = self.eat(TokenKind::KwIf) {
            children.push(if_kw);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        children.push(self.expect_token(TokenKind::EqGt)?);
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::CaseArm, children))
    }

    fn parse_resume_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let resume = self.expect_token(TokenKind::KwResume)?;
        let mut children = vec![resume];
        if !self.at_any(&[
            TokenKind::Semicolon,
            TokenKind::RParen,
            TokenKind::Pipe,
            TokenKind::RBrace,
            TokenKind::Eof,
        ]) {
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ResumeExpr, children))
    }

    fn parse_import_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let import = self.expect_token(TokenKind::KwImport)?;
        let expr = self.parse_expr(PREFIX_BP)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::ImportExpr,
            vec![import, SyntaxElementId::Node(expr)],
        ))
    }

    fn parse_let_expr(&mut self, mut attrs: SyntaxElementList) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(TokenKind::KwLet)?);
        if self.at_any(&[TokenKind::KwMut, TokenKind::KwRec]) {
            attrs.push(self.advance_element());
        }
        attrs.push(SyntaxElementId::Node(self.parse_pattern()?));
        if self.at(TokenKind::LBracket) {
            let open = self.advance_element();
            let params = self.parse_type_param_list_contents(TokenKind::RBracket)?;
            let close = self.expect_token(TokenKind::RBracket)?;
            let node = self.wrap_list(SyntaxNodeKind::TypeParamList, open, params, close);
            attrs.push(SyntaxElementId::Node(node));
        }
        if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            let params = self.parse_param_list_contents(TokenKind::RParen)?;
            let close = self.expect_token(TokenKind::RParen)?;
            let node = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
            attrs.push(SyntaxElementId::Node(node));
        }
        if self.at(TokenKind::KwWhere) {
            attrs.push(self.advance_element());
            attrs.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        if self.at(TokenKind::KwWith) {
            attrs.push(self.advance_element());
            attrs.push(SyntaxElementId::Node(self.parse_effect_set()?));
        }
        if let Some(colon) = self.eat(TokenKind::Colon) {
            attrs.push(colon);
            attrs.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        attrs.push(self.expect_token(TokenKind::ColonEq)?);
        attrs.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::LetExpr, attrs))
    }

    fn parse_data_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let data = self.expect_token(TokenKind::KwData)?;
        let open = self.expect_token(TokenKind::LBrace)?;
        let mut children = vec![data, open];
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
            let first = self.parse_data_member()?;
            children.push(SyntaxElementId::Node(first));
            if self.builder.node_kind(first) == SyntaxNodeKind::Variant {
                while let Some(pipe) = self.eat(TokenKind::Pipe) {
                    children.push(pipe);
                    if self.at(TokenKind::RBrace) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_variant_def()?));
                }
            } else {
                while let Some(semi) = self.eat(TokenKind::Semicolon) {
                    children.push(semi);
                    if self.at(TokenKind::RBrace) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_field_def()?));
                }
            }
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::DataExpr, children))
    }

    fn parse_data_member(&mut self) -> ParseResult<SyntaxNodeId> {
        let attrs = self.parse_attrs()?;
        let ident = self.expect_ident_element()?;
        if self.at(TokenKind::Colon) {
            let mut children = attrs;
            children.push(ident);
            children.push(self.advance_element());
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
            if let Some(bind) = self.eat(TokenKind::ColonEq) {
                children.push(bind);
                children.push(SyntaxElementId::Node(self.parse_expr(0)?));
            }
            return Ok(self
                .builder
                .push_node_from_children(SyntaxNodeKind::Field, children));
        }
        let mut children = attrs;
        children.push(ident);
        if let Some(bind) = self.eat(TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Variant, children))
    }

    fn parse_variant_list(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![SyntaxElementId::Node(self.parse_variant_def()?)];
        while let Some(pipe) = self.eat(TokenKind::Pipe) {
            children.push(pipe);
            if self.at(TokenKind::RBrace) {
                break;
            }
            children.push(SyntaxElementId::Node(self.parse_variant_def()?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::VariantList, children))
    }

    fn parse_variant_def(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        children.push(self.expect_ident_element()?);
        if let Some(colon) = self.eat(TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        if let Some(bind) = self.eat(TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Variant, children))
    }

    fn parse_field_list(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![SyntaxElementId::Node(self.parse_field_def()?)];
        while let Some(semi) = self.eat(TokenKind::Semicolon) {
            children.push(semi);
            if self.at(TokenKind::RBrace) {
                break;
            }
            children.push(SyntaxElementId::Node(self.parse_field_def()?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::FieldList, children))
    }

    fn parse_field_def(&mut self) -> ParseResult<SyntaxNodeId> {
        let ident = self.expect_ident_element()?;
        let colon = self.expect_token(TokenKind::Colon)?;
        let mut children = vec![ident, colon, SyntaxElementId::Node(self.parse_expr(0)?)];
        if let Some(bind) = self.eat(TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Field, children))
    }

    fn parse_effect_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_member_body_expr(SyntaxNodeKind::EffectExpr, TokenKind::KwEffect)
    }

    fn parse_class_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let class = self.expect_token(TokenKind::KwClass)?;
        let mut children = vec![class];
        if self.at(TokenKind::KwWhere) {
            children.push(self.advance_element());
            children.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        self.parse_member_body(&mut children)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ClassExpr, children))
    }

    fn parse_instance_expr(&mut self, mut attrs: SyntaxElementList) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(TokenKind::KwInstance)?);
        if self.at(TokenKind::LBracket) {
            let open = self.advance_element();
            let params = self.parse_type_param_list_contents(TokenKind::RBracket)?;
            let close = self.expect_token(TokenKind::RBracket)?;
            let node = self.wrap_list(SyntaxNodeKind::TypeParamList, open, params, close);
            attrs.push(SyntaxElementId::Node(node));
        }
        if self.at(TokenKind::KwWhere) {
            attrs.push(self.advance_element());
            attrs.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        attrs.push(SyntaxElementId::Node(self.parse_expr(0)?));
        self.parse_member_body(&mut attrs)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::InstanceExpr, attrs))
    }

    fn parse_perform_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let perform = self.expect_token(TokenKind::KwPerform)?;
        let expr = self.parse_expr(PREFIX_BP)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::PerformExpr,
            vec![perform, SyntaxElementId::Node(expr)],
        ))
    }

    fn parse_handle_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let handle = self.expect_token(TokenKind::KwHandle)?;
        let expr = self.parse_expr(0)?;
        let with_kw = self.expect_token(TokenKind::KwWith)?;
        let binder = self.expect_ident_element()?;
        let of_kw = self.expect_token(TokenKind::KwOf)?;
        let open = self.expect_token(TokenKind::LParen)?;
        let mut children = vec![
            handle,
            SyntaxElementId::Node(expr),
            with_kw,
            binder,
            of_kw,
            open,
        ];
        children.extend(self.parse_piped_nodes(TokenKind::RParen, Parser::parse_handle_clause)?);
        children.push(self.expect_token(TokenKind::RParen)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::HandleExpr, children))
    }

    fn parse_handle_clause(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![self.expect_ident_element()?];
        if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            children.extend(self.parse_ident_list_opt(TokenKind::RParen));
            children.push(self.expect_token(TokenKind::RParen)?);
        }
        children.push(self.expect_token(TokenKind::EqGt)?);
        children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::HandlerClause, children))
    }

    fn parse_foreign_expr(&mut self, mut attrs: SyntaxElementList) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(TokenKind::KwForeign)?);
        if self.at(TokenKind::String) {
            attrs.push(self.advance_element());
        }
        if self.at(TokenKind::KwLet) {
            attrs.push(SyntaxElementId::Node(
                self.parse_foreign_binding_with_let()?,
            ));
        } else {
            attrs.push(SyntaxElementId::Node(self.parse_foreign_group()?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ForeignBlockExpr, attrs))
    }

    fn parse_foreign_binding_with_let(&mut self) -> ParseResult<SyntaxNodeId> {
        let let_kw = self.expect_token(TokenKind::KwLet)?;
        let binding = self.parse_foreign_binding()?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::LetExpr,
            vec![let_kw, SyntaxElementId::Node(binding)],
        ))
    }

    fn parse_foreign_group(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(TokenKind::LParen)?;
        let mut children = vec![open];
        while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
            children.push(self.expect_token(TokenKind::KwLet)?);
            children.push(SyntaxElementId::Node(self.parse_foreign_binding()?));
            children.push(self.expect_token(TokenKind::Semicolon)?);
        }
        children.push(self.expect_token(TokenKind::RParen)?);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::MemberList, children))
    }

    fn parse_foreign_binding(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        children.push(self.expect_ident_element()?);
        if self.at(TokenKind::LBracket) {
            let open = self.advance_element();
            let params = self.parse_type_param_list_contents(TokenKind::RBracket)?;
            let close = self.expect_token(TokenKind::RBracket)?;
            let node = self.wrap_list(SyntaxNodeKind::TypeParamList, open, params, close);
            children.push(SyntaxElementId::Node(node));
        }
        if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            let params = self.parse_param_list_contents(TokenKind::RParen)?;
            let close = self.expect_token(TokenKind::RParen)?;
            let node = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
            children.push(SyntaxElementId::Node(node));
        }
        if self.at(TokenKind::KwWhere) {
            children.push(self.advance_element());
            children.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        if let Some(colon) = self.eat(TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Member, children))
    }

    fn parse_quote_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let quote = self.expect_token(TokenKind::KwQuote)?;
        self.quote_depth = self.quote_depth.saturating_add(1);
        let result = if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            let expr = self.parse_expr(0)?;
            let close = self.expect_token(TokenKind::RParen)?;
            self.builder.push_node_from_children(
                SyntaxNodeKind::QuoteExpr,
                vec![quote, open, SyntaxElementId::Node(expr), close],
            )
        } else {
            let open = self.expect_token(TokenKind::LBrace)?;
            let mut children = vec![quote, open];
            while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
                children.push(SyntaxElementId::Node(self.parse_stmt()?));
            }
            children.push(self.expect_token(TokenKind::RBrace)?);
            self.builder
                .push_node_from_children(SyntaxNodeKind::QuoteExpr, children)
        };
        self.quote_depth = self.quote_depth.saturating_sub(1);
        Ok(result)
    }

    fn parse_splice_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        if self.quote_depth == 0 {
            self.error(ParseError {
                kind: ParseErrorKind::SpliceOutsideQuote,
                span: self.span(),
            });
        }
        let hash = self.expect_token(TokenKind::Hash)?;
        match self.peek_kind() {
            TokenKind::Ident => {
                let ident = self.advance_element();
                Ok(self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::SpliceExpr, vec![hash, ident]))
            }
            TokenKind::LParen => {
                let open = self.advance_element();
                let expr = self.parse_expr(0)?;
                let close = self.expect_token(TokenKind::RParen)?;
                Ok(self.builder.push_node_from_children(
                    SyntaxNodeKind::SpliceExpr,
                    vec![hash, open, SyntaxElementId::Node(expr), close],
                ))
            }
            TokenKind::LBracket => {
                let open = self.advance_element();
                let mut children = vec![hash, open];
                children.extend(self.parse_expr_list(TokenKind::RBracket)?);
                children.push(self.expect_token(TokenKind::RBracket)?);
                Ok(self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::SpliceExpr, children))
            }
            _ => Err(self.expected_splice_target()),
        }
    }

    fn parse_with_attrs_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        let expr = match self.peek_kind() {
            TokenKind::KwLet => self.parse_let_expr(Vec::new())?,
            TokenKind::KwForeign => self.parse_foreign_expr(Vec::new())?,
            TokenKind::KwInstance => self.parse_instance_expr(Vec::new())?,
            TokenKind::KwExport => self.parse_export_expr(Vec::new())?,
            _ => self.parse_expr(PREFIX_BP)?,
        };
        children.push(SyntaxElementId::Node(expr));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::AttributedExpr, children))
    }

    fn parse_export_expr(&mut self, mut attrs: SyntaxElementList) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(TokenKind::KwExport)?);
        if let Some(opaque) = self.eat(TokenKind::KwOpaque) {
            attrs.push(opaque);
        }
        if self.at(TokenKind::KwForeign) {
            attrs.push(self.advance_element());
            if self.at(TokenKind::String) {
                attrs.push(self.advance_element());
            }
        }
        match self.peek_kind() {
            TokenKind::KwLet => attrs.push(SyntaxElementId::Node(self.parse_let_expr(Vec::new())?)),
            TokenKind::KwInstance => {
                attrs.push(SyntaxElementId::Node(self.parse_instance_expr(Vec::new())?));
            }
            TokenKind::LParen => attrs.push(SyntaxElementId::Node(self.parse_foreign_group()?)),
            _ => attrs.push(SyntaxElementId::Node(self.parse_expr(0)?)),
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ExportExpr, attrs))
    }

    fn parse_member_body_expr(
        &mut self,
        kind: SyntaxNodeKind,
        keyword: TokenKind,
    ) -> ParseResult<SyntaxNodeId> {
        let head = self.expect_token(keyword)?;
        let open = self.expect_token(TokenKind::LBrace)?;
        let mut children = vec![head, open];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            children.push(SyntaxElementId::Node(self.parse_member()?));
            let _ = self.eat(TokenKind::Semicolon);
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(self.builder.push_node_from_children(kind, children))
    }

    fn parse_member(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        match self.peek_kind() {
            TokenKind::KwLet => {
                children.push(self.advance_element());
                children.extend(self.parse_op_or_ident_name()?);
                if self.at(TokenKind::LParen) {
                    let open = self.advance_element();
                    let params = self.parse_param_list_contents(TokenKind::RParen)?;
                    let close = self.expect_token(TokenKind::RParen)?;
                    let node = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
                    children.push(SyntaxElementId::Node(node));
                }
                if let Some(colon) = self.eat(TokenKind::Colon) {
                    children.push(colon);
                    children.push(SyntaxElementId::Node(self.parse_expr(0)?));
                }
                if let Some(bind) = self.eat(TokenKind::ColonEq) {
                    children.push(bind);
                    children.push(SyntaxElementId::Node(self.parse_expr(0)?));
                }
            }
            TokenKind::KwLaw => {
                children.push(self.advance_element());
                children.push(self.expect_ident_element()?);
                if self.at(TokenKind::LParen) {
                    let open = self.advance_element();
                    let params = self.parse_param_list_contents(TokenKind::RParen)?;
                    let close = self.expect_token(TokenKind::RParen)?;
                    let node = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
                    children.push(SyntaxElementId::Node(node));
                }
                children.push(self.expect_token(TokenKind::ColonEq)?);
                children.push(SyntaxElementId::Node(self.parse_expr(0)?));
            }
            _ => return Err(self.expected_member()),
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Member, children))
    }

    fn parse_attrs(&mut self) -> ParseResult<SyntaxElementList> {
        let mut children = Vec::new();
        while self.at(TokenKind::At) {
            children.push(SyntaxElementId::Node(self.parse_attr()?));
        }
        Ok(children)
    }

    fn parse_attr(&mut self) -> ParseResult<SyntaxNodeId> {
        let at = self.expect_token(TokenKind::At)?;
        let mut children = vec![at, self.expect_ident_element()?];
        while let Some(dot) = self.eat(TokenKind::Dot) {
            children.push(dot);
            children.push(self.expect_ident_element()?);
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

    fn parse_attr_value(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::String | TokenKind::Int | TokenKind::Rune => Ok(self.parse_literal_expr()),
            TokenKind::Dot => self.parse_attr_variant(),
            TokenKind::LBracket => self.parse_attr_array(),
            TokenKind::LBrace => self.parse_attr_record(),
            _ => Err(self.expected_attr_value()),
        }
    }

    fn parse_attr_variant(&mut self) -> ParseResult<SyntaxNodeId> {
        let dot = self.expect_token(TokenKind::Dot)?;
        let ident = self.expect_ident_element()?;
        let mut children = vec![dot, ident];
        if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            children.extend(self.parse_separated_nodes(
                TokenKind::Comma,
                TokenKind::RParen,
                Parser::parse_attr_value_node,
            )?);
            children.push(self.expect_token(TokenKind::RParen)?);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::VariantExpr, children))
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

    fn parse_param_list_contents(&mut self, close: TokenKind) -> ParseResult<SyntaxElementList> {
        self.parse_separated_nodes(TokenKind::Comma, close, Parser::parse_param)
    }

    fn parse_param(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = Vec::new();
        if let Some(mut_kw) = self.eat(TokenKind::KwMut) {
            children.push(mut_kw);
        }
        children.push(self.expect_ident_element()?);
        if let Some(colon) = self.eat(TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        if let Some(bind) = self.eat(TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Param, children))
    }

    fn parse_type_param_list_contents(
        &mut self,
        close: TokenKind,
    ) -> ParseResult<SyntaxElementList> {
        self.parse_separated_nodes(TokenKind::Comma, close, Parser::parse_type_param)
    }

    fn parse_constraint_list(&mut self) -> ParseResult<SyntaxNodeId> {
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
        let op = if self.at_any(&[TokenKind::LtColon, TokenKind::Colon]) {
            self.advance_element()
        } else {
            return Err(self.expected_constraint_operator());
        };
        let expr = self.parse_expr(0)?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::Constraint,
            vec![ident, op, SyntaxElementId::Node(expr)],
        ))
    }

    fn parse_effect_set(&mut self) -> ParseResult<SyntaxNodeId> {
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
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
            children.push(self.expect_token(TokenKind::RBracket)?);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::EffectItem, children))
    }

    pub(super) fn parse_expr_list(&mut self, close: TokenKind) -> ParseResult<SyntaxElementList> {
        self.parse_separated_nodes(TokenKind::Comma, close, Parser::parse_expr_node)
    }

    fn parse_ident_list_opt(&mut self, close: TokenKind) -> SyntaxElementList {
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
                self.error(ParseError {
                    kind: ParseErrorKind::ExpectedIdentifier {
                        found: self.found_token(),
                    },
                    span: self.span(),
                });
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

    fn parse_op_or_ident_name(&mut self) -> ParseResult<SyntaxElementList> {
        match self.peek_kind() {
            TokenKind::Ident | TokenKind::OpIdent => Ok(vec![self.advance_element()]),
            _ => Err(self.expected_operator_member_name()),
        }
    }

    fn parse_type_param(&mut self) -> ParseResult<SyntaxNodeId> {
        let ident = self.expect_ident_element()?;
        Ok(self.node1(SyntaxNodeKind::TypeParam, ident))
    }

    pub(super) fn parse_expr_node(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_expr(0)
    }

    fn parse_attr_value_node(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_attr_value()
    }
}
