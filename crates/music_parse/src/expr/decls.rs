use music_ast::common::{
    Attr, AttrArg, FnDecl, LawDecl, MemberDecl, MemberName, ModifierSet, OpFixity, RecordDefField,
    VariantDef,
};
use music_ast::expr::{
    ClassDefData, DataBody, ExprKind, ImportKind, InstanceBody, InstanceDef, LetBinding,
};
use music_ast::pat::PatKind;
use music_ast::{AttrId, AttrList, ExprId, IdentList};
use music_lex::TokenKind;
use music_shared::{Ident, Span};

use crate::errors::{ParseError, ParseErrorKind, ParseResult, describe_token};
use crate::parser::Parser;

impl Parser<'_> {
    pub(super) fn parse_import(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwImport, "'import'")?;
        let path_str = self.expect_string()?;
        let path = self.intern(&path_str);
        let _ = self.expect(&TokenKind::KwAs, "'as'")?;
        let kind = self.parse_import_kind()?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::Import { path, kind }, span))
    }

    pub(super) fn parse_import_kind(&mut self) -> ParseResult<ImportKind> {
        if self.eat_wildcard() {
            return Ok(ImportKind::Wildcard);
        }
        let name = self.expect_ident()?;
        if self.eat(&TokenKind::DotLBrace) {
            let mut members = vec![self.expect_ident()?];
            while self.eat(&TokenKind::Comma) {
                if self.at(&TokenKind::RBrace) {
                    break;
                }
                members.push(self.expect_ident()?);
            }
            let _ = self.expect(&TokenKind::RBrace, "'}'")?;
            return Ok(ImportKind::Selective(name, members));
        }
        Ok(ImportKind::Qualified(name))
    }

    pub(super) fn expect_string(&mut self) -> ParseResult<String> {
        if let TokenKind::Str(s) = self.peek_kind() {
            let s = s.clone();
            let _ = self.advance();
            return Ok(s);
        }
        Err(self.err_expected_token("string literal"))
    }

    pub(super) fn parse_foreign(
        &mut self,
        mut modifiers: ModifierSet,
        attrs: AttrList,
    ) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwForeign, "'foreign'")?;
        modifiers.foreign = true;
        let abi = if matches!(self.peek_kind(), TokenKind::Str(_)) {
            let s = self.expect_string()?;
            Some(self.intern(&s))
        } else {
            None
        };
        modifiers.foreign_abi = abi;
        if self.at(&TokenKind::KwImport) {
            return self.parse_foreign_import(start);
        }
        if self.at(&TokenKind::KwLet) {
            return self.parse_let(modifiers, attrs);
        }
        if self.at(&TokenKind::LParen) {
            return self.parse_foreign_block(start, modifiers, attrs);
        }
        Err(self.err_expected_token_in("'import', 'let', or '('", Some("in foreign declaration")))
    }

    pub(super) fn parse_foreign_import(&mut self, start: Span) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::KwImport, "'import'")?;
        let path_str = self.expect_string()?;
        let path = self.intern(&path_str);
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(ExprKind::ForeignImport(path), span))
    }

    pub(super) fn parse_foreign_block(
        &mut self,
        start: Span,
        modifiers: ModifierSet,
        outer_attrs: AttrList,
    ) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::LParen, "'('")?;
        let mut stmts = Vec::new();
        while !self.at(&TokenKind::RParen) && !self.at_eof() {
            let mut attrs = outer_attrs.clone();
            attrs.extend(self.parse_attrs()?);
            let binding = self.parse_foreign_binding(modifiers, attrs)?;
            stmts.push(binding);
            let _ = self.eat(&TokenKind::Semi);
        }
        let end = self.expect(&TokenKind::RParen, "')'")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::Seq(stmts), span))
    }

    pub(super) fn parse_foreign_binding(
        &mut self,
        modifiers: ModifierSet,
        attrs: AttrList,
    ) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwLet, "'let'")?;
        let name = self.expect_ident()?;
        let mut ty_params = self.parse_opt_bracket_params()?;
        let params = self.parse_opt_params()?;
        if ty_params.is_empty() {
            ty_params = self.parse_opt_bracket_params()?;
        }
        let constraints = self.parse_opt_where()?;
        let ret_ty = self.parse_opt_ty_annot()?;
        let pat = self.alloc_pat(PatKind::Bind(name), name.span);
        let sig = Self::build_signature(params, ty_params, constraints, Vec::new(), ret_ty);
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(
            ExprKind::Let(Box::new(LetBinding {
                modifiers,
                attrs,
                pat,
                sig,
                value: None,
            })),
            span,
        ))
    }

    pub(super) fn parse_data_def(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwData, "'data'")?;
        let open_span = self.expect(&TokenKind::LBrace, "'{'")?;

        if self.at(&TokenKind::RBrace) {
            return Err(self.err_expected_token_in(
                "';' or '|' to disambiguate empty data type",
                Some("in data definition"),
            ));
        }

        if self.at(&TokenKind::Pipe) {
            let _ = self.advance();
            let mut variants = Vec::new();
            if !self.at(&TokenKind::RBrace) {
                variants.push(self.parse_variant_def()?);
                variants.extend(self.parse_variant_list()?);
            }
            let end =
                self.expect_closing(&TokenKind::RBrace, "'{'", open_span, "in data definition")?;
            let span = start.to(end);
            return Ok(self.alloc_expr(ExprKind::DataDef(Box::new(DataBody::Sum(variants))), span));
        }

        if self.at(&TokenKind::Semi) {
            let _ = self.advance();
            let mut fields = Vec::new();
            if !self.at(&TokenKind::RBrace) {
                fields.push(self.parse_rec_def_field()?);
                while self.eat(&TokenKind::Semi) {
                    if self.at(&TokenKind::RBrace) {
                        break;
                    }
                    fields.push(self.parse_rec_def_field()?);
                }
            }
            let end =
                self.expect_closing(&TokenKind::RBrace, "'{'", open_span, "in data definition")?;
            let span = start.to(end);
            return Ok(
                self.alloc_expr(ExprKind::DataDef(Box::new(DataBody::Product(fields))), span)
            );
        }

        let body = self.parse_data_body()?;
        let end =
            self.expect_closing(&TokenKind::RBrace, "'{'", open_span, "in data definition")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::DataDef(Box::new(body)), span))
    }

    pub(super) fn parse_data_body(&mut self) -> ParseResult<DataBody> {
        let attrs = self.parse_attrs()?;
        let name = self.expect_ident()?;

        if self.eat(&TokenKind::Colon) {
            let ty = self.parse_ty()?;
            let default = self.parse_opt_default()?;

            match self.peek_kind() {
                TokenKind::Pipe => {
                    let first = VariantDef {
                        attrs,
                        name,
                        payload: Some(ty),
                        default,
                    };
                    let mut variants = vec![first];
                    variants.extend(self.parse_variant_list()?);
                    Ok(DataBody::Sum(variants))
                }
                TokenKind::Semi | TokenKind::RBrace => {
                    let first = RecordDefField { name, ty, default };
                    let mut fields = vec![first];
                    while self.eat(&TokenKind::Semi) {
                        if self.at(&TokenKind::RBrace) {
                            break;
                        }
                        fields.push(self.parse_rec_def_field()?);
                    }
                    Ok(DataBody::Product(fields))
                }
                _ => {
                    Err(self.err_expected_token_in("'|' or ';' or '}'", Some("in data definition")))
                }
            }
        } else {
            let default = self.parse_opt_default()?;
            let first = VariantDef {
                attrs,
                name,
                payload: None,
                default,
            };
            match self.peek_kind() {
                TokenKind::Pipe => {
                    let mut variants = vec![first];
                    variants.extend(self.parse_variant_list()?);
                    Ok(DataBody::Sum(variants))
                }
                TokenKind::RBrace => Ok(DataBody::Sum(vec![first])),
                _ => {
                    Err(self.err_expected_token_in("':', '|', or '}'", Some("in data definition")))
                }
            }
        }
    }

    pub(super) fn parse_variant_list(&mut self) -> ParseResult<Vec<VariantDef>> {
        let mut variants = Vec::new();
        while self.eat(&TokenKind::Pipe) {
            if self.at(&TokenKind::RBrace) {
                break;
            }
            variants.push(self.parse_variant_def()?);
        }
        Ok(variants)
    }

    pub(super) fn parse_variant_def(&mut self) -> ParseResult<VariantDef> {
        let attrs = self.parse_attrs()?;
        let name = self.expect_ident()?;
        let payload = if self.eat(&TokenKind::Colon) {
            Some(self.parse_ty()?)
        } else {
            None
        };
        let default = self.parse_opt_default()?;
        Ok(VariantDef {
            attrs,
            name,
            payload,
            default,
        })
    }

    pub(super) fn parse_rec_def_field(&mut self) -> ParseResult<RecordDefField> {
        let name = self.expect_ident()?;
        let _ = self.expect(&TokenKind::Colon, "':'")?;
        let ty = self.parse_ty()?;
        let default = self.parse_opt_default()?;
        Ok(RecordDefField { name, ty, default })
    }

    pub(super) fn parse_effect_def(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwEffect, "'effect'")?;
        let (members, end) = self.parse_braced_members("in effect definition")?;
        let span = start.to(end);
        Ok(self.alloc_expr(ExprKind::EffectDef(members), span))
    }

    pub(super) fn parse_class_def(&mut self) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwClass, "'class'")?;
        let constraints = self.parse_opt_where()?;
        let (members, end) = self.parse_braced_members("in class definition")?;
        let span = start.to(end);
        Ok(self.alloc_expr(
            ExprKind::ClassDef(Box::new(ClassDefData {
                constraints,
                members,
            })),
            span,
        ))
    }

    pub(super) fn parse_braced_members(
        &mut self,
        ctx: &'static str,
    ) -> ParseResult<(Vec<MemberDecl>, Span)> {
        let open_span = self.expect(&TokenKind::LBrace, "'{'")?;
        let members = self.parse_member_decls()?;
        let end = self.expect_closing(&TokenKind::RBrace, "'{'", open_span, ctx)?;
        Ok((members, end))
    }

    pub(super) fn parse_member_decls(&mut self) -> ParseResult<Vec<MemberDecl>> {
        let mut members = Vec::new();
        while !self.at(&TokenKind::RBrace) && !self.at_eof() {
            members.push(self.parse_member_decl()?);
            let _ = self.eat(&TokenKind::Semi);
        }
        Ok(members)
    }

    pub(super) fn parse_member_decl(&mut self) -> ParseResult<MemberDecl> {
        let attrs = self.parse_attrs()?;
        if self.at(&TokenKind::KwLaw) {
            return Ok(MemberDecl::Law(self.parse_law_decl()?));
        }
        Ok(MemberDecl::Fn(self.parse_fn_decl(attrs)?))
    }

    pub(super) fn parse_fn_decl(&mut self, attrs: AttrList) -> ParseResult<FnDecl> {
        let _ = self.expect(&TokenKind::KwLet, "'let'")?;
        let name = self.parse_member_name()?;
        let params = self.parse_opt_params()?;
        let ret_ty = self.parse_opt_ty_annot()?;
        let body = self.parse_opt_default()?;
        Ok(FnDecl {
            attrs,
            name,
            params,
            ret_ty,
            body,
        })
    }

    pub(super) fn parse_member_name(&mut self) -> ParseResult<MemberName> {
        if self.at(&TokenKind::LParen) {
            let _ = self.advance();
            if self.eat_wildcard() {
                let ident = self.parse_op_ident()?;
                let _ = self.expect_wildcard("'_' after operator in (_ op _)")?;
                let _ = self.expect(&TokenKind::RParen, "')'")?;
                return Ok(MemberName::Op(ident, OpFixity::Infix));
            }
            let ident = self.parse_op_ident()?;
            let _ = self.expect_wildcard("'_' after operator in (op _)")?;
            let _ = self.expect(&TokenKind::RParen, "')'")?;
            return Ok(MemberName::Op(ident, OpFixity::Prefix));
        }
        let ident = self.expect_ident()?;
        Ok(MemberName::Ident(ident))
    }

    pub(super) fn expect_wildcard(&mut self, expected: &'static str) -> ParseResult<Span> {
        if self.eat_wildcard() {
            Ok(self.prev_span())
        } else {
            Err(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected,
                    found: describe_token(&self.peek().kind),
                },
                span: self.span(),
                context: None,
            })
        }
    }

    pub(super) fn is_op_token(&self) -> bool {
        matches!(
            self.peek_kind(),
            TokenKind::Plus
                | TokenKind::Minus
                | TokenKind::Star
                | TokenKind::Slash
                | TokenKind::Percent
                | TokenKind::Eq
                | TokenKind::SlashEq
                | TokenKind::Lt
                | TokenKind::Gt
                | TokenKind::LtEq
                | TokenKind::GtEq
                | TokenKind::ColonColon
                | TokenKind::KwShl
                | TokenKind::KwShr
        )
    }

    pub(super) fn parse_op_ident(&mut self) -> ParseResult<Ident> {
        if !self.is_op_token() {
            return Err(ParseError {
                kind: ParseErrorKind::ExpectedToken {
                    expected: "operator name",
                    found: describe_token(&self.peek().kind),
                },
                span: self.span(),
                context: None,
            });
        }
        let span = self.span();
        let _ = self.advance();
        Ok(self.make_ident_from_span(span))
    }

    pub(super) fn parse_law_decl(&mut self) -> ParseResult<LawDecl> {
        let _ = self.expect(&TokenKind::KwLaw, "'law'")?;
        let name = self.expect_ident()?;
        let params = self.parse_opt_params()?;
        let _ = self.expect(&TokenKind::ColonEq, "':='")?;
        let body = self.parse_expr(0)?;
        Ok(LawDecl { name, params, body })
    }

    pub(super) fn parse_instance_def(
        &mut self,
        exported: bool,
        attrs: AttrList,
    ) -> ParseResult<ExprId> {
        let start = self.expect(&TokenKind::KwInstance, "'instance'")?;
        let ty_params = self.parse_opt_bracket_params()?;
        let constraints = self.parse_opt_where()?;
        let ty = self.parse_ty_ref()?;
        let body = self.parse_instance_body()?;
        let span = start.to(self.prev_span());
        Ok(self.alloc_expr(
            ExprKind::InstanceDef(Box::new(InstanceDef {
                attrs,
                exported,
                ty_params,
                constraints,
                ty,
                body,
            })),
            span,
        ))
    }

    pub(super) fn parse_instance_body(&mut self) -> ParseResult<InstanceBody> {
        if self.eat(&TokenKind::KwVia) {
            let via = self.parse_ty_ref()?;
            return Ok(InstanceBody::Via(via));
        }
        let _ = self.expect(&TokenKind::LBrace, "'{'")?;
        let members = self.parse_member_decls()?;
        let _ = self.expect(&TokenKind::RBrace, "'}'")?;
        Ok(InstanceBody::Methods(members))
    }

    pub(super) fn parse_export(&mut self) -> ParseResult<ExprId> {
        self.parse_export_with_attrs(Vec::new())
    }

    pub(super) fn parse_export_with_attrs(&mut self, attrs: AttrList) -> ParseResult<ExprId> {
        let _ = self.expect(&TokenKind::KwExport, "'export'")?;
        if self.at(&TokenKind::KwInstance) {
            return self.parse_instance_def(true, attrs);
        }
        let mut modifiers = ModifierSet {
            exported: true,
            ..ModifierSet::default()
        };
        if self.eat(&TokenKind::KwOpaque) {
            modifiers.opaque = true;
        }
        if self.at(&TokenKind::KwForeign) {
            return self.parse_foreign(modifiers, attrs);
        }
        self.parse_let(modifiers, attrs)
    }

    pub(super) fn parse_attrs(&mut self) -> ParseResult<AttrList> {
        let mut attrs = Vec::new();
        while self.at(&TokenKind::At) {
            attrs.push(self.parse_attr()?);
        }
        Ok(attrs)
    }

    pub(super) fn parse_attr(&mut self) -> ParseResult<AttrId> {
        let start = self.expect(&TokenKind::At, "'@'")?;
        let path = self.parse_attr_path()?;
        let args = if self.eat(&TokenKind::LParen) {
            let a = self.parse_attr_args()?;
            let _ = self.expect(&TokenKind::RParen, "')'")?;
            a
        } else {
            Vec::new()
        };
        let span = start.to(self.prev_span());
        Ok(self.alloc_attr(Attr { path, args }, span))
    }

    pub(super) fn parse_attr_path(&mut self) -> ParseResult<IdentList> {
        let mut path = vec![self.expect_ident()?];
        while self.eat(&TokenKind::Dot) {
            path.push(self.expect_ident()?);
        }
        Ok(path)
    }

    pub(super) fn parse_attr_args(&mut self) -> ParseResult<Vec<AttrArg>> {
        let mut args = Vec::new();
        while !self.at(&TokenKind::RParen) && !self.at_eof() {
            args.push(self.parse_attr_arg()?);
            if !self.eat(&TokenKind::Comma) {
                break;
            }
        }
        Ok(args)
    }

    pub(super) fn parse_attr_arg(&mut self) -> ParseResult<AttrArg> {
        if matches!(self.peek_kind(), TokenKind::Ident | TokenKind::EscapedIdent) {
            let saved_pos = self.pos;
            let name = self.expect_ident()?;
            if self.eat(&TokenKind::ColonEq) {
                let value = self.parse_expr(0)?;
                return Ok(AttrArg::Named { name, value });
            }
            self.pos = saved_pos;
        }
        let value = self.parse_expr(0)?;
        Ok(AttrArg::Positional(value))
    }

    pub(super) fn parse_with_attrs(&mut self) -> ParseResult<ExprId> {
        let attrs = self.parse_attrs()?;
        match self.peek_kind() {
            TokenKind::KwExport => self.parse_export_with_attrs(attrs),
            TokenKind::KwLet => self.parse_let(ModifierSet::default(), attrs),
            TokenKind::KwForeign => self.parse_foreign(ModifierSet::default(), attrs),
            TokenKind::KwInstance => self.parse_instance_def(false, attrs),
            _ => Err(self.err_expected_token_in(
                "'export', 'let', 'foreign', or 'instance'",
                Some("after attribute"),
            )),
        }
    }
}
