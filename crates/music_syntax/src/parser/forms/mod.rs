use super::*;

mod atoms;
mod attrs;
mod decls;
mod lists;

impl Parser<'_> {
    pub(super) fn parse_atom_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_atom_literal_or_name()
            .or_else(|| self.parse_atom_structural())
            .or_else(|| self.parse_atom_keyword_expr())
            .unwrap_or_else(|| Err(self.expected_expression()))
    }

    fn parse_atom_literal_or_name(&mut self) -> Option<ParseResult<SyntaxNodeId>> {
        Some(match self.peek_kind() {
            TokenKind::Int | TokenKind::Float | TokenKind::String | TokenKind::Rune => {
                Ok(self.parse_literal_expr())
            }
            TokenKind::TemplateNoSubst | TokenKind::TemplateHead => self.parse_template_expr(),
            TokenKind::Ident | TokenKind::OpIdent => self.parse_name_expr(),
            TokenKind::Hash => self.parse_splice_expr(),
            TokenKind::KwQuote => self.parse_quote_expr(),
            _ => return None,
        })
    }

    fn parse_atom_structural(&mut self) -> Option<ParseResult<SyntaxNodeId>> {
        Some(match self.peek_kind() {
            TokenKind::LParen => {
                if self.is_pi_paren() {
                    self.parse_pi_expr()
                } else if self.is_lambda_paren() {
                    self.parse_lambda_expr()
                } else {
                    self.parse_paren_expr()
                }
            }
            TokenKind::LBracket => self.parse_array_expr(),
            TokenKind::LBrace => self.parse_record_expr(),
            TokenKind::Dot => self.parse_dot_prefix_expr(),
            TokenKind::At | TokenKind::KwExport => self.parse_with_mods_expr(),
            _ => return None,
        })
    }

    fn parse_atom_keyword_expr(&mut self) -> Option<ParseResult<SyntaxNodeId>> {
        Some(match self.peek_kind() {
            TokenKind::KwCase => self.parse_case_expr(),
            TokenKind::KwLet => self.parse_let_expr(Vec::new()),
            TokenKind::KwResume => self.parse_resume_expr(),
            TokenKind::KwImport => self.parse_import_expr(),
            TokenKind::KwData => self.parse_data_expr(),
            TokenKind::KwEffect => self.parse_effect_expr(),
            TokenKind::KwClass => self.parse_class_expr(),
            TokenKind::KwInstance => self.parse_instance_expr(Vec::new()),
            TokenKind::KwPerform => self.parse_perform_expr(),
            TokenKind::KwUsing => self.parse_handler_expr(),
            TokenKind::KwHandle => self.parse_handle_expr(),
            TokenKind::KwForeign => self.parse_foreign_expr(Vec::new()),
            _ => return None,
        })
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

    pub(super) fn parse_variant_like(
        &mut self,
        kind: SyntaxNodeKind,
        parse_item: fn(&mut Self) -> ParseResult<SyntaxNodeId>,
    ) -> ParseResult<SyntaxNodeId> {
        let dot = self.expect_token(TokenKind::Dot)?;
        let ident = self.expect_ident_element()?;
        let mut children = vec![dot, ident];
        if self.at(TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            children.extend(self.parse_separated_nodes(
                TokenKind::Comma,
                TokenKind::RParen,
                parse_item,
            )?);
            children.push(self.expect_token(TokenKind::RParen)?);
        }
        Ok(self.builder.push_node_from_children(kind, children))
    }

    pub(super) fn parse_expr_node(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_expr(0)
    }

    pub(super) fn parse_attr_value_node(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_attr_value()
    }
}
