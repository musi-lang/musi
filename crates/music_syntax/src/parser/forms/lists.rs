use super::*;

impl Parser<'_> {
    pub(crate) fn parse_punctuated_nodes<F>(
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

    pub(crate) fn parse_separated_nodes<F>(
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

    pub(crate) fn parse_wrapped_nodes<F>(
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

    pub(crate) fn parse_punctuated_wrapped_nodes<F>(
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
        children.extend(self.parse_punctuated_nodes(sep, close_kind, parse_item)?);
        children.push(self.expect_token(close_kind)?);
        Ok(self.node(kind, children))
    }

    pub(crate) fn parse_piped_nodes<F>(
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

    pub(crate) fn parse_node_list<F>(
        &mut self,
        kind: SyntaxNodeKind,
        sep: TokenKind,
        end: TokenKind,
        mut parse_item: F,
    ) -> ParseResult<SyntaxNodeId>
    where
        F: FnMut(&mut Self) -> ParseResult<SyntaxNodeId>,
    {
        let mut children = vec![SyntaxElementId::Node(parse_item(self)?)];
        while let Some(separator) = self.eat(sep) {
            children.push(separator);
            if self.at(end) {
                break;
            }
            children.push(SyntaxElementId::Node(parse_item(self)?));
        }
        Ok(self.builder.push_node_from_children(kind, children))
    }

    pub(crate) fn parse_member_body(
        &mut self,
        children: &mut SyntaxElementList,
    ) -> ParseResult<()> {
        children.push(self.expect_token(TokenKind::LBrace)?);
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            children.push(SyntaxElementId::Node(self.parse_member()?));
            let _ = self.eat(TokenKind::Semicolon);
        }
        children.push(self.expect_token(TokenKind::RBrace)?);
        Ok(())
    }

    pub(crate) fn parse_optional_wrapped_list<F>(
        &mut self,
        children: &mut SyntaxElementList,
        open_kind: TokenKind,
        close_kind: TokenKind,
        kind: SyntaxNodeKind,
        parse_contents: F,
    ) -> ParseResult<()>
    where
        F: FnOnce(&mut Self, TokenKind) -> ParseResult<SyntaxElementList>,
    {
        if !self.at(open_kind) {
            return Ok(());
        }
        let open = self.advance_element();
        let items = parse_contents(self, close_kind)?;
        let close = self.expect_token(close_kind)?;
        let node = self.wrap_list(kind, open, items, close);
        children.push(SyntaxElementId::Node(node));
        Ok(())
    }

    pub(crate) fn parse_optional_type_params_clause(
        &mut self,
        children: &mut SyntaxElementList,
    ) -> ParseResult<()> {
        self.parse_optional_wrapped_list(
            children,
            TokenKind::LBracket,
            TokenKind::RBracket,
            SyntaxNodeKind::TypeParamList,
            Parser::parse_type_param_list_contents,
        )
    }

    pub(crate) fn parse_optional_param_clause(
        &mut self,
        children: &mut SyntaxElementList,
    ) -> ParseResult<()> {
        self.parse_optional_wrapped_list(
            children,
            TokenKind::LParen,
            TokenKind::RParen,
            SyntaxNodeKind::ParamList,
            Parser::parse_param_list_contents,
        )
    }

    pub(crate) fn parse_optional_constraints_clause(
        &mut self,
        children: &mut SyntaxElementList,
    ) -> ParseResult<()> {
        if self.at(TokenKind::KwWhere) {
            children.push(self.advance_element());
            children.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        Ok(())
    }

    pub(crate) fn parse_optional_effects_clause(
        &mut self,
        children: &mut SyntaxElementList,
    ) -> ParseResult<()> {
        if self.at(TokenKind::KwRequire) {
            children.push(self.advance_element());
            children.push(SyntaxElementId::Node(self.parse_effect_set()?));
        }
        Ok(())
    }

    pub(crate) fn parse_optional_typed_expr(
        &mut self,
        children: &mut SyntaxElementList,
    ) -> ParseResult<()> {
        if let Some(colon) = self.eat(TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_type_expr(0)?));
        }
        Ok(())
    }

    pub(crate) fn parse_required_typed_expr(
        &mut self,
        children: &mut SyntaxElementList,
    ) -> ParseResult<()> {
        children.push(self.expect_token(TokenKind::Colon)?);
        children.push(SyntaxElementId::Node(self.parse_type_expr(0)?));
        Ok(())
    }

    pub(crate) fn parse_optional_bound_expr(
        &mut self,
        children: &mut SyntaxElementList,
    ) -> ParseResult<()> {
        if let Some(bind) = self.eat(TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(())
    }
}
