//! Definition expression parsing: fn, record, choice, class, given, bind, and attributes.

use musi_lex::token::TokenKind;

use crate::ast::{
    Attr, AttrArg, ChoiceVariant, ClassMember, Expr, Modifier, Param, RecField, VariantPayload,
};

use super::Parser;

impl Parser<'_> {
    pub(super) fn parse_expr_after_attrs(&mut self) -> Expr {
        let attrs = self.parse_opt_attrs();
        let modifiers = self.parse_modifiers();

        match self.peek_kind() {
            TokenKind::Fn => self.parse_fn_expr(attrs, modifiers),
            TokenKind::Record => self.parse_record(attrs, modifiers),
            TokenKind::Choice => self.parse_choice(attrs, modifiers),
            TokenKind::Const | TokenKind::Var => self.parse_bind(attrs, modifiers),
            TokenKind::While => self.parse_while(),
            TokenKind::Loop => self.parse_loop(),
            TokenKind::For => self.parse_for(),
            TokenKind::Match => self.parse_match(),
            TokenKind::Label => self.parse_label(),
            TokenKind::Class => self.parse_class_def(),
            TokenKind::Given => self.parse_given_def(),
            _ => self.error_expr("expected declaration or loop after attributes/modifiers"),
        }
    }

    pub(super) fn parse_opt_attrs(&mut self) -> Vec<Attr> {
        if self.at(TokenKind::Hash) && self.peek2() == TokenKind::LBracket {
            let _hash = self.expect(TokenKind::Hash);
            self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, Parser::parse_attr)
        } else {
            Vec::new()
        }
    }

    fn parse_attr(&mut self) -> Attr {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::LParen) {
            let list = self.parse_separated_list(TokenKind::RParen, Parser::parse_attr_arg);
            let _rp = self.expect(TokenKind::RParen);
            list
        } else {
            Vec::new()
        };
        Attr {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    fn parse_attr_arg(&mut self) -> AttrArg {
        let start = self.start_span();
        // Named: ident [":=" lit]
        if self.at(TokenKind::Ident) {
            let name = self.expect_symbol();
            let value = if self.eat(TokenKind::ColonEq) {
                Some(self.parse_lit_value())
            } else {
                None
            };
            return AttrArg::Named {
                name,
                value,
                span: self.finish_span(start),
            };
        }
        // Literal argument
        let lit = self.parse_lit_value();
        AttrArg::Value {
            value: lit,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_modifiers(&mut self) -> Vec<Modifier> {
        let mut mods = Vec::new();
        loop {
            if self.eat(TokenKind::Export) {
                mods.push(Modifier::Export);
            } else if self.eat(TokenKind::Opaque) {
                mods.push(Modifier::Opaque);
            } else if self.eat(TokenKind::Extrin) {
                let abi = if self.at(TokenKind::StringLit) {
                    Some(self.expect_symbol())
                } else {
                    None
                };
                mods.push(Modifier::Extrin(abi));
            } else {
                break;
            }
        }
        mods
    }

    pub(super) fn parse_fn_expr(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _fn = self.expect(TokenKind::Fn);

        // LL(1): if next is Ident or Underscore → named FnDef, else Lambda
        if self.at(TokenKind::Ident) || self.at(TokenKind::Underscore) {
            // Named function definition
            let name = self.expect_symbol();
            let ty_params = self.parse_opt_ty_params();
            let params =
                self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_param);
            let ret_ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
            let where_clause = self.parse_opt_where_clause();
            let body = if self.eat(TokenKind::EqGt) {
                Some(self.parse_and_alloc_expr())
            } else {
                None
            };
            Expr::FnDef {
                attrs,
                modifiers,
                name,
                ty_params,
                params,
                ret_ty,
                where_clause,
                body,
                span: self.finish_span(start),
            }
        } else {
            // Lambda: fn [ty_params] (params) [: ret_ty] [where ...] => expr
            let ty_params = self.parse_opt_ty_params();
            let params =
                self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_param);
            let ret_ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
            let where_clause = self.parse_opt_where_clause();
            let _arrow = self.expect(TokenKind::EqGt);
            let body = self.parse_and_alloc_expr();
            Expr::Lambda {
                attrs,
                ty_params,
                params,
                ret_ty,
                where_clause,
                body,
                span: self.finish_span(start),
            }
        }
    }

    pub(super) fn parse_param(&mut self) -> Param {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        Param {
            attrs,
            mutable,
            name,
            ty,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_record(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _rec = self.expect(TokenKind::Record);
        let name = self.optional_ident();
        let ty_params = self.parse_opt_ty_params();
        let fields = self.parse_delimited(
            TokenKind::LBrace,
            TokenKind::RBrace,
            Parser::parse_rec_field,
        );
        Expr::Record {
            attrs,
            modifiers,
            name,
            ty_params,
            fields,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_rec_field(&mut self) -> RecField {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        RecField {
            attrs,
            mutable,
            name,
            ty,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_choice(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _choice = self.expect(TokenKind::Choice);
        let name = self.optional_ident();
        let ty_params = self.parse_opt_ty_params();
        let _lb = self.expect(TokenKind::LBrace);
        let variants = self.parse_pipe_separated(TokenKind::RBrace, Parser::parse_choice_variant);
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Choice {
            attrs,
            modifiers,
            name,
            ty_params,
            variants,
            span: self.finish_span(start),
        }
    }

    fn parse_choice_variant(&mut self) -> ChoiceVariant {
        let start = self.start_span();
        let attrs = self.parse_opt_attrs();
        let name = self.expect_symbol();
        let payload = match self.peek_kind() {
            TokenKind::LParen => {
                let tys =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_ty);
                Some(VariantPayload::Positional(tys))
            }
            TokenKind::LBrace => {
                let fields = self.parse_delimited(
                    TokenKind::LBrace,
                    TokenKind::RBrace,
                    Parser::parse_rec_field,
                );
                Some(VariantPayload::Named(fields))
            }
            TokenKind::ColonEq => {
                let _ceq = self.advance();
                let lit = self.parse_lit_value();
                Some(VariantPayload::Discriminant(lit))
            }
            _ => None,
        };
        ChoiceVariant {
            attrs,
            name,
            payload,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_bind(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let kind = self.parse_bind_kind();
        let pat = self.parse_pat();
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        let init = self.parse_option(TokenKind::ColonEq, Parser::parse_and_alloc_expr);
        Expr::Bind {
            attrs,
            modifiers,
            kind,
            pat,
            ty,
            init,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_class_def(&mut self) -> Expr {
        let start = self.start_span();
        let _class = self.expect(TokenKind::Class);
        let name = self.expect_symbol();
        let ty_params = self.parse_opt_ty_params();
        let supers = if self.eat(TokenKind::Satisfies) {
            let mut list = vec![self.parse_ty_named()];
            while self.eat(TokenKind::Comma) {
                list.push(self.parse_ty_named());
            }
            list
        } else {
            Vec::new()
        };
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::ClassDef {
            name,
            ty_params,
            supers,
            members,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_given_def(&mut self) -> Expr {
        let start = self.start_span();
        let _given = self.expect(TokenKind::Given);
        let class_app = self.parse_ty_named();
        let constraints = self.parse_opt_where_clause();
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::GivenDef {
            class_app,
            constraints,
            members,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_class_body(&mut self) -> Vec<ClassMember> {
        let mut members = Vec::new();
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let member = if self.at(TokenKind::Law) {
                let m_start = self.start_span();
                let _law = self.expect(TokenKind::Law);
                let name = self.expect_symbol();
                let params =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_param);
                let _arrow = self.expect(TokenKind::EqGt);
                let body = self.parse_and_alloc_expr();
                ClassMember::Law {
                    name,
                    params,
                    body,
                    span: self.finish_span(m_start),
                }
            } else {
                let fn_expr = self.parse_fn_expr(Vec::new(), Vec::new());
                let idx = self.alloc_expr(fn_expr);
                ClassMember::Method(idx)
            };
            members.push(member);
            let _semi = self.expect(TokenKind::Semi);
        }
        members
    }
}
