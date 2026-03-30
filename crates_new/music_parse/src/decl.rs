use music_ast::{SyntaxElementId, SyntaxNodeId, SyntaxNodeKind};
use music_lex::TokenKind;

use crate::parser::Parser;

use super::*;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LetBody {
    Required,
    Optional,
}

impl Parser<'_, '_, '_> {
    pub(crate) fn parse_with_attrs_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let attrs = self.parse_attrs()?;
        match self.peek_kind() {
            TokenKind::KwExport => self.parse_export_expr_with_attrs(attrs),
            TokenKind::KwLet => self.parse_let_expr_required_body(attrs),
            TokenKind::KwForeign => self.parse_foreign_expr(attrs),
            TokenKind::KwInstance => self.parse_instance_expr(attrs),
            _ => Err(ParseError {
                kind: ParseErrorKind::InvalidAttributeTarget {
                    found: Box::new(self.peek_kind().clone()),
                },
                span: self.span(),
            }),
        }
    }

    pub(crate) fn parse_attrs(&mut self) -> ParseResult<Vec<SyntaxElementId>> {
        let mut children = vec![];
        while self.at(&TokenKind::At) {
            let attr = self.parse_attr()?;
            children.push(SyntaxElementId::Node(attr));
        }
        Ok(children)
    }

    fn parse_attr(&mut self) -> ParseResult<SyntaxNodeId> {
        let at = self.expect_token(&TokenKind::At)?;
        let mut children = vec![at, self.expect_ident_element()?];
        while let Some(dot) = self.eat(&TokenKind::Dot) {
            children.push(dot);
            children.push(self.expect_ident_element()?);
        }
        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);
            if !self.at(&TokenKind::RParen) {
                children.push(SyntaxElementId::Node(self.parse_attr_arg()?));
                while let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                    if self.at(&TokenKind::RParen) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_attr_arg()?));
                }
            }
            let close = self.expect_token(&TokenKind::RParen)?;
            children.push(close);
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Attr, children))
    }

    fn parse_attr_arg(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![];
        if matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent)
            && matches!(self.nth_kind(1), TokenKind::ColonEq)
        {
            children.push(self.advance_element());
            children.push(self.advance_element());
        }
        let value = self.parse_attr_value()?;
        children.push(SyntaxElementId::Node(value));
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::AttrArg, children))
    }

    fn parse_attr_value(&mut self) -> ParseResult<SyntaxNodeId> {
        match self.peek_kind() {
            TokenKind::StringLit | TokenKind::IntLit | TokenKind::RuneLit => {
                let lit = self.advance_element();
                Ok(self
                    .builder
                    .push_node_from_children(SyntaxNodeKind::LiteralExpr, [lit]))
            }
            TokenKind::Dot => self.parse_attr_variant_value(),
            TokenKind::LBracket => self.parse_attr_array_value(),
            TokenKind::LBrace => self.parse_attr_record_value(),
            _ => {
                self.error(self.expected_attr_value());

                let mut children = vec![];
                if !self.at(&TokenKind::Eof) {
                    children.push(self.advance_element());
                }
                Ok(self.builder.push_error_node(children))
            }
        }
    }

    fn parse_attr_variant_value(&mut self) -> ParseResult<SyntaxNodeId> {
        let dot = self.expect_token(&TokenKind::Dot)?;
        let ident = self.expect_ident_element()?;
        let mut children = vec![dot, ident];

        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            children.push(open);

            if !self.at(&TokenKind::RParen) {
                children.extend(self.parse_attr_value_list(&TokenKind::RParen)?);
            }

            let close = self.expect_token(&TokenKind::RParen)?;
            children.push(close);
        }

        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::VariantExpr, children))
    }

    fn parse_attr_array_value(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LBracket)?;
        let mut children = vec![open];

        while !self.at(&TokenKind::RBracket) && !self.at(&TokenKind::Eof) {
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                continue;
            }

            let item = self.parse_attr_array_item()?;
            children.push(SyntaxElementId::Node(item));
        }

        let close = self.expect_token(&TokenKind::RBracket)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ArrayExpr, children))
    }

    fn parse_attr_array_item(&mut self) -> ParseResult<SyntaxNodeId> {
        let value = self.parse_attr_value()?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ArrayItem, [SyntaxElementId::Node(value)]))
    }

    fn parse_attr_record_value(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LBrace)?;
        let mut children = vec![open];

        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                continue;
            }

            let item = self.parse_attr_record_item()?;
            children.push(SyntaxElementId::Node(item));
        }

        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordExpr, children))
    }

    fn parse_attr_record_item(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![];

        if !matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
            self.error(ParseError {
                kind: ParseErrorKind::ExpectedIdentifier {
                    found: Box::new(self.found_token()),
                },
                span: self.span(),
            });

            if !self.at(&TokenKind::Eof) {
                children.push(self.advance_element());
            }

            return Ok(self.builder.push_error_node(children));
        }

        children.push(self.advance_element());

        if !self.at(&TokenKind::ColonEq) {
            self.error(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected: Box::new(TokenKind::ColonEq),
                    found: Box::new(self.found_token()),
                },
                span: self.span(),
            });

            while !self.at_any(&[
                TokenKind::Comma,
                TokenKind::RBrace,
                TokenKind::RParen,
                TokenKind::RBracket,
                TokenKind::Semi,
                TokenKind::Eof,
            ]) {
                children.push(self.advance_element());
            }

            return Ok(self.builder.push_error_node(children));
        }

        children.push(self.advance_element());
        let value = self.parse_attr_value()?;
        children.push(SyntaxElementId::Node(value));

        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::RecordItem, children))
    }

    fn parse_attr_value_list(&mut self, close: &TokenKind) -> ParseResult<Vec<SyntaxElementId>> {
        let mut children = vec![];
        children.push(SyntaxElementId::Node(self.parse_attr_value()?));
        while let Some(comma) = self.eat(&TokenKind::Comma) {
            children.push(comma);
            if self.at(close) {
                break;
            }
            children.push(SyntaxElementId::Node(self.parse_attr_value()?));
        }
        Ok(children)
    }

    pub(crate) fn parse_export_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_export_expr_with_attrs(vec![])
    }

    fn parse_export_expr_with_attrs(
        &mut self,
        mut attrs: Vec<SyntaxElementId>,
    ) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(&TokenKind::KwExport)?);
        if let Some(opaque) = self.eat(&TokenKind::KwOpaque) {
            attrs.push(opaque);
        }
        if self.at(&TokenKind::KwForeign) {
            attrs.push(self.advance_element());
            if self.at(&TokenKind::StringLit) {
                attrs.push(self.advance_element());
            }
            if self.at(&TokenKind::KwLet) {
                return self.parse_let_expr(attrs);
            }
            if self.at(&TokenKind::LParen) {
                return self.parse_foreign_block_expr(attrs);
            }
        }
        if self.at(&TokenKind::KwInstance) {
            return self.parse_instance_expr(attrs);
        }
        if self.at(&TokenKind::KwLet) {
            return self.parse_let_expr(attrs);
        }
        Err(self.expected_expression())
    }

    pub(crate) fn parse_foreign_expr(
        &mut self,
        mut attrs: Vec<SyntaxElementId>,
    ) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(&TokenKind::KwForeign)?);
        if self.at(&TokenKind::StringLit) {
            attrs.push(self.advance_element());
        }
        if self.at(&TokenKind::KwLet) {
            return self.parse_let_expr(attrs);
        }
        if self.at(&TokenKind::LParen) {
            return self.parse_foreign_block_expr(attrs);
        }
        Err(self.expected_foreign_binding())
    }

    fn parse_foreign_block_expr(
        &mut self,
        mut attrs: Vec<SyntaxElementId>,
    ) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(&TokenKind::LParen)?);
        while !self.at(&TokenKind::RParen) && !self.at(&TokenKind::Eof) {
            let binding = self.parse_foreign_binding()?;
            attrs.push(SyntaxElementId::Node(binding));
            let semi = self.expect_token(&TokenKind::Semi)?;
            attrs.push(semi);
        }
        let close = self.expect_token(&TokenKind::RParen)?;
        attrs.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ForeignBlockExpr, attrs))
    }

    pub(crate) fn parse_let_expr(
        &mut self,
        mut attrs: Vec<SyntaxElementId>,
    ) -> ParseResult<SyntaxNodeId> {
        self.parse_let_expr_inner(&mut attrs, LetBody::Optional)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::LetExpr, attrs))
    }

    pub(crate) fn parse_let_expr_required_body(
        &mut self,
        mut attrs: Vec<SyntaxElementId>,
    ) -> ParseResult<SyntaxNodeId> {
        self.parse_let_expr_inner(&mut attrs, LetBody::Required)?;
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::LetExpr, attrs))
    }

    fn parse_let_expr_inner(
        &mut self,
        attrs: &mut Vec<SyntaxElementId>,
        body: LetBody,
    ) -> ParseResult<()> {
        attrs.push(self.expect_token(&TokenKind::KwLet)?);
        if let Some(mut_kw) = self.eat(&TokenKind::KwMut) {
            attrs.push(mut_kw);
        }
        let pat = self.parse_pat()?;
        attrs.push(SyntaxElementId::Node(pat));

        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            let params = self.parse_param_list_contents(&TokenKind::RParen)?;
            let close = self.expect_token(&TokenKind::RParen)?;
            let list = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
            attrs.push(SyntaxElementId::Node(list));
        }
        if self.at(&TokenKind::LBracket) {
            let open = self.advance_element();
            let params = self.parse_ty_param_list_contents(&TokenKind::RBracket)?;
            let close = self.expect_token(&TokenKind::RBracket)?;
            let list = self.wrap_list(SyntaxNodeKind::TypeParamList, open, params, close);
            attrs.push(SyntaxElementId::Node(list));
        }
        if self.at(&TokenKind::KwWhere) {
            attrs.push(self.advance_element());
            let list = self.parse_constraint_list()?;
            attrs.push(SyntaxElementId::Node(list));
        }
        if self.at(&TokenKind::KwWith) {
            attrs.push(self.advance_element());
            let effect_set = self.parse_effect_set()?;
            attrs.push(SyntaxElementId::Node(effect_set));
        }
        if let Some(colon) = self.eat(&TokenKind::Colon) {
            attrs.push(colon);
            let ty = self.parse_ty()?;
            attrs.push(SyntaxElementId::Node(ty));
        }

        match body {
            LetBody::Required => {
                let bind = self.expect_token(&TokenKind::ColonEq)?;
                attrs.push(bind);
                let expr = self.parse_expr(0)?;
                attrs.push(SyntaxElementId::Node(expr));
            }
            LetBody::Optional => {
                if let Some(bind) = self.eat(&TokenKind::ColonEq) {
                    attrs.push(bind);
                    let expr = self.parse_expr(0)?;
                    attrs.push(SyntaxElementId::Node(expr));
                }
            }
        }

        Ok(())
    }

    fn parse_foreign_binding(&mut self) -> ParseResult<SyntaxNodeId> {
        if !self.at(&TokenKind::KwLet) {
            return Err(self.expected_foreign_binding());
        }

        let mut children = vec![self.advance_element()];
        children.extend(self.parse_attrs()?);
        children.push(self.expect_ident_element()?);

        if self.at(&TokenKind::LBracket) {
            let open = self.advance_element();
            let params = self.parse_ty_param_list_contents(&TokenKind::RBracket)?;
            let close = self.expect_token(&TokenKind::RBracket)?;
            let list = self.wrap_list(SyntaxNodeKind::TypeParamList, open, params, close);
            children.push(SyntaxElementId::Node(list));
        }
        if self.at(&TokenKind::LParen) {
            let open = self.advance_element();
            let params = self.parse_param_list_contents(&TokenKind::RParen)?;
            let close = self.expect_token(&TokenKind::RParen)?;
            let list = self.wrap_list(SyntaxNodeKind::ParamList, open, params, close);
            children.push(SyntaxElementId::Node(list));
        }
        if self.at(&TokenKind::KwWhere) {
            children.push(self.advance_element());
            children.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        if let Some(colon) = self.eat(&TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_ty()?));
        }

        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::LetExpr, children))
    }

    pub(crate) fn parse_import_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let import_kw = self.expect_token(&TokenKind::KwImport)?;
        let path = self.expect_string_element()?;
        let mut children = vec![import_kw, path];
        if self.at(&TokenKind::KwAs) {
            self.error(ParseError {
                kind: ParseErrorKind::ImportAliasNotSupported,
                span: self.span(),
            });

            // Recover by consuming the unsupported alias tail so callers don't cascade
            // into unrelated "expected token" errors.
            let mut tail = vec![];
            while !self.at_any(&[
                TokenKind::Semi,
                TokenKind::Comma,
                TokenKind::RParen,
                TokenKind::RBracket,
                TokenKind::RBrace,
                TokenKind::Pipe,
                TokenKind::Eof,
            ]) {
                if self.at(&TokenKind::DotLBrace) {
                    tail.push(self.advance_element());
                    while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
                        tail.push(self.advance_element());
                    }
                    if self.at(&TokenKind::RBrace) {
                        tail.push(self.advance_element());
                    }
                    continue;
                }
                tail.push(self.advance_element());
            }
            if !tail.is_empty() {
                children.push(SyntaxElementId::Node(self.builder.push_error_node(tail)));
            }
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ImportExpr, children))
    }

    pub(crate) fn parse_data_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let data_kw = self.expect_token(&TokenKind::KwData)?;
        let open = self.expect_token(&TokenKind::LBrace)?;
        let mut children = vec![data_kw, open];
        if self.at(&TokenKind::Pipe) {
            children.push(self.advance_element());
            if !self.at(&TokenKind::RBrace) {
                children.push(SyntaxElementId::Node(self.parse_variant_list()?));
            }
        } else if self.at(&TokenKind::Semi) {
            children.push(self.advance_element());
            if !self.at(&TokenKind::RBrace) {
                children.push(SyntaxElementId::Node(self.parse_field_list()?));
            }
        } else if !self.at(&TokenKind::RBrace) {
            let (first_children, first_has_colon) = self.parse_data_member_parts()?;
            let first_kind = if self.at(&TokenKind::Pipe) {
                SyntaxNodeKind::Variant
            } else if self.at(&TokenKind::Semi) {
                SyntaxNodeKind::Field
            } else if self.at(&TokenKind::RBrace) {
                if first_has_colon {
                    SyntaxNodeKind::Field
                } else {
                    SyntaxNodeKind::Variant
                }
            } else {
                SyntaxNodeKind::Field
            };
            children.push(SyntaxElementId::Node(
                self.builder
                    .push_node_from_children(first_kind, first_children),
            ));

            if first_kind == SyntaxNodeKind::Variant {
                while let Some(pipe) = self.eat(&TokenKind::Pipe) {
                    children.push(pipe);
                    if self.at(&TokenKind::RBrace) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_variant()?));
                }
            } else {
                while let Some(semi) = self.eat(&TokenKind::Semi) {
                    children.push(semi);
                    if self.at(&TokenKind::RBrace) {
                        break;
                    }
                    children.push(SyntaxElementId::Node(self.parse_field()?));
                }
            }
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::DataExpr, children))
    }

    fn parse_data_member_parts(&mut self) -> ParseResult<(Vec<SyntaxElementId>, bool)> {
        let mut children = self.parse_attrs()?;
        children.push(self.expect_ident_element()?);

        let mut has_colon = false;
        if let Some(colon) = self.eat(&TokenKind::Colon) {
            has_colon = true;
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_ty()?));
        }

        if let Some(bind) = self.eat(&TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }

        Ok((children, has_colon))
    }

    fn parse_variant_list(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![SyntaxElementId::Node(self.parse_variant()?)];
        while let Some(pipe) = self.eat(&TokenKind::Pipe) {
            children.push(pipe);
            if self.at(&TokenKind::RBrace) {
                break;
            }
            children.push(SyntaxElementId::Node(self.parse_variant()?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::VariantList, children))
    }

    fn parse_variant(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = self.parse_attrs()?;
        children.push(self.expect_ident_element()?);
        if let Some(colon) = self.eat(&TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_ty()?));
        }
        if let Some(bind) = self.eat(&TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Variant, children))
    }

    fn parse_field_list(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![SyntaxElementId::Node(self.parse_field()?)];
        while let Some(semi) = self.eat(&TokenKind::Semi) {
            children.push(semi);
            if self.at(&TokenKind::RBrace) {
                break;
            }
            children.push(SyntaxElementId::Node(self.parse_field()?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::FieldList, children))
    }

    fn parse_field(&mut self) -> ParseResult<SyntaxNodeId> {
        let ident = self.expect_ident_element()?;
        let colon = self.expect_token(&TokenKind::Colon)?;
        let ty = self.parse_ty()?;
        let mut children = vec![ident, colon, SyntaxElementId::Node(ty)];
        if let Some(bind) = self.eat(&TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Field, children))
    }

    pub(crate) fn parse_effect_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        self.parse_member_body_expr(SyntaxNodeKind::EffectExpr, &TokenKind::KwEffect)
    }

    pub(crate) fn parse_class_expr(&mut self) -> ParseResult<SyntaxNodeId> {
        let class_kw = self.expect_token(&TokenKind::KwClass)?;
        let mut children = vec![class_kw];
        if self.at(&TokenKind::KwWhere) {
            children.push(self.advance_element());
            children.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        let open = self.expect_token(&TokenKind::LBrace)?;
        children.push(open);
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            children.push(SyntaxElementId::Node(self.parse_member()?));
            let _ = self.eat(&TokenKind::Semi);
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::ClassExpr, children))
    }

    pub(crate) fn parse_instance_expr(
        &mut self,
        mut attrs: Vec<SyntaxElementId>,
    ) -> ParseResult<SyntaxNodeId> {
        attrs.push(self.expect_token(&TokenKind::KwInstance)?);
        if self.at(&TokenKind::LBracket) {
            let open = self.advance_element();
            let params = self.parse_ty_param_list_contents(&TokenKind::RBracket)?;
            let close = self.expect_token(&TokenKind::RBracket)?;
            let list = self.wrap_list(SyntaxNodeKind::TypeParamList, open, params, close);
            attrs.push(SyntaxElementId::Node(list));
        }
        if self.at(&TokenKind::KwWhere) {
            attrs.push(self.advance_element());
            attrs.push(SyntaxElementId::Node(self.parse_constraint_list()?));
        }
        attrs.push(SyntaxElementId::Node(self.parse_named_ty()?));
        let open = self.expect_token(&TokenKind::LBrace)?;
        attrs.push(open);
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            attrs.push(SyntaxElementId::Node(self.parse_member()?));
            let _ = self.eat(&TokenKind::Semi);
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        attrs.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::InstanceExpr, attrs))
    }

    fn parse_member_body_expr(
        &mut self,
        kind: SyntaxNodeKind,
        kw: &TokenKind,
    ) -> ParseResult<SyntaxNodeId> {
        let head = self.expect_token(kw)?;
        let open = self.expect_token(&TokenKind::LBrace)?;
        let mut children = vec![head, open];
        while !self.at(&TokenKind::RBrace) && !self.at(&TokenKind::Eof) {
            children.push(SyntaxElementId::Node(self.parse_member()?));
            let _ = self.eat(&TokenKind::Semi);
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self.builder.push_node_from_children(kind, children))
    }

    pub(crate) fn parse_param_list_contents(
        &mut self,
        close: &TokenKind,
    ) -> ParseResult<Vec<SyntaxElementId>> {
        let mut children = vec![];
        if self.at(close) {
            return Ok(children);
        }
        loop {
            let param = self.parse_param()?;
            children.push(SyntaxElementId::Node(param));
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                if self.at(close) {
                    break;
                }
                continue;
            }
            break;
        }
        Ok(children)
    }

    fn parse_param(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![];
        if let Some(mut_kw) = self.eat(&TokenKind::KwMut) {
            children.push(mut_kw);
        }
        children.push(self.expect_ident_element()?);
        if let Some(colon) = self.eat(&TokenKind::Colon) {
            children.push(colon);
            children.push(SyntaxElementId::Node(self.parse_ty()?));
        }
        if let Some(bind) = self.eat(&TokenKind::ColonEq) {
            children.push(bind);
            children.push(SyntaxElementId::Node(self.parse_expr(0)?));
        }
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::Param, children))
    }

    pub(crate) fn parse_ty_param_list_contents(
        &mut self,
        close: &TokenKind,
    ) -> ParseResult<Vec<SyntaxElementId>> {
        let mut children = vec![];
        if self.at(close) {
            return Ok(children);
        }
        loop {
            let ident = self.expect_ident_element()?;
            let node = self
                .builder
                .push_node_from_children(SyntaxNodeKind::TypeParam, [ident]);
            children.push(SyntaxElementId::Node(node));
            if let Some(comma) = self.eat(&TokenKind::Comma) {
                children.push(comma);
                if self.at(close) {
                    break;
                }
                continue;
            }
            break;
        }
        Ok(children)
    }

    pub(crate) fn parse_constraint_list(&mut self) -> ParseResult<SyntaxNodeId> {
        let mut children = vec![SyntaxElementId::Node(self.parse_constraint()?)];
        while let Some(comma) = self.eat(&TokenKind::Comma) {
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
        let op = if self.at(&TokenKind::LtColon) || self.at(&TokenKind::Colon) {
            self.advance_element()
        } else {
            return Err(self.expected_constraint_operator());
        };
        let ty = self.parse_named_ty()?;
        Ok(self.builder.push_node_from_children(
            SyntaxNodeKind::Constraint,
            [ident, op, SyntaxElementId::Node(ty)],
        ))
    }

    fn parse_effect_set(&mut self) -> ParseResult<SyntaxNodeId> {
        let open = self.expect_token(&TokenKind::LBrace)?;
        let mut children = vec![open];
        // Permit empty/sep-only sets in the syntax tree; meaning is handled later.
        while let Some(comma) = self.eat(&TokenKind::Comma) {
            children.push(comma);
        }

        let mut saw_remainder = false;

        if !self.at(&TokenKind::RBrace) {
            self.parse_effect_entry_or_error(&mut children, &mut saw_remainder);

            loop {
                let mut comma_count = 0usize;
                let mut extra_span = None;
                while self.at(&TokenKind::Comma) {
                    comma_count += 1;
                    if comma_count == 2 {
                        extra_span = Some(self.span());
                    }
                    children.push(self.advance_element());
                }

                if comma_count == 0 || self.at(&TokenKind::RBrace) {
                    break;
                }

                // Multiple separators between items are an error, but still recover and keep parsing.
                if comma_count > 1 {
                    self.error(ParseError {
                        kind: ParseErrorKind::ExpectedEffectItem {
                            found: Box::new(TokenKind::Comma),
                        },
                        span: extra_span.expect("count > 1"),
                    });
                }

                while let Some(comma) = self.eat(&TokenKind::Comma) {
                    children.push(comma);
                }

                if self.at(&TokenKind::RBrace) {
                    break;
                }

                if saw_remainder {
                    // The remainder is syntactically last; recover by skipping the rest.
                    self.error(ParseError {
                        kind: ParseErrorKind::EffectRemainderMustBeLast,
                        span: self.span(),
                    });
                    break;
                }

                self.parse_effect_entry_or_error(&mut children, &mut saw_remainder);
            }
        }
        let close = self.expect_token(&TokenKind::RBrace)?;
        children.push(close);
        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::EffectSet, children))
    }

    fn parse_effect_entry_or_error(
        &mut self,
        out: &mut Vec<SyntaxElementId>,
        saw_remainder: &mut bool,
    ) {
        if self.at(&TokenKind::DotDotDot) && !*saw_remainder {
            let dots = self.advance_element();
            out.push(dots);
            let name = match self.peek_kind() {
                TokenKind::Ident | TokenKind::EscapedIdent => self.advance_element(),
                _ => {
                    self.error(ParseError {
                        kind: ParseErrorKind::ExpectedEffectRemainderName {
                            found: Box::new(self.peek_kind().clone()),
                        },
                        span: self.span(),
                    });
                    return;
                }
            };
            out.push(name);
            *saw_remainder = true;
            return;
        }

        if self.at(&TokenKind::DotDotDot) && *saw_remainder {
            self.error(ParseError {
                kind: ParseErrorKind::ExpectedEffectItem {
                    found: Box::new(TokenKind::DotDotDot),
                },
                span: self.span(),
            });
            let bad = self.advance_element();
            out.push(SyntaxElementId::Node(self.builder.push_error_node([bad])));
            return;
        }

        match self.parse_effect_item() {
            Ok(item) => out.push(SyntaxElementId::Node(item)),
            Err(error) => {
                self.error(error);
                if self.at_any(&[TokenKind::Comma, TokenKind::RBrace, TokenKind::Eof]) {
                    return;
                }

                let mut bad = vec![];
                while !self.at_any(&[TokenKind::Comma, TokenKind::RBrace, TokenKind::Eof]) {
                    bad.push(self.advance_element());
                }
                out.push(SyntaxElementId::Node(self.builder.push_error_node(bad)));
            }
        }
    }

    fn parse_effect_item(&mut self) -> ParseResult<SyntaxNodeId> {
        if !matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
            return Err(self.expected_effect_item());
        }

        let mut children = vec![self.advance_element()];
        if self.at(&TokenKind::LBracket) {
            let open = self.advance_element();
            children.push(open);
            children.push(SyntaxElementId::Node(self.parse_ty()?));
            let close = self.expect_token(&TokenKind::RBracket)?;
            children.push(close);
        }

        Ok(self
            .builder
            .push_node_from_children(SyntaxNodeKind::EffectItem, children))
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
