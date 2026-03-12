//! Let/var binding parsing.

use music_ast::expr::{BindKind, Expr, LetFields, RecDefField};
use music_ast::ty::TyParam;
use music_lex::token::TokenKind;

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    /// Parses `'let' ['ref'] pat [ty_annot] ':=' expr ['in' '(' expr ')']`.
    pub(crate) fn parse_expr_let(&mut self) -> Expr {
        let start = self.start_span();
        let _let = self.bump();
        self.parse_expr_binding_immut_body(BindKind::Immut, start)
    }

    /// Parses `'var' ['ref'] pat [ty_annot] ':=' expr`.
    pub(crate) fn parse_expr_binding_mut(&mut self) -> Expr {
        let start = self.start_span();
        let _var = self.bump();
        self.parse_expr_binding_immut_body(BindKind::Mut, start)
    }

    /// Parses `'choice' '{' type '}'`.
    pub(crate) fn parse_expr_choice(&mut self) -> Expr {
        let start = self.start_span();
        let _choice = self.bump();
        let _lb = self.expect(TokenKind::LBrace);
        let body = self.parse_alloc_ty();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Choice {
            body,
            span: self.finish_span(start),
        }
    }

    /// Parses `'record' '{' field { ',' field } '}'` where field is `ident ':' ty [':=' expr]`.
    pub(crate) fn parse_expr_record_def(&mut self) -> Expr {
        let start = self.start_span();
        let _record = self.bump();
        let _lb = self.expect(TokenKind::LBrace);
        let fields = self.comma_sep(TokenKind::RBrace, Self::parse_rec_def_field);
        let _rb = self.expect(TokenKind::RBrace);
        Expr::RecordDef {
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_rec_def_field(&mut self) -> RecDefField {
        let start = self.start_span();
        let name = self.expect_symbol();
        let _colon = self.expect(TokenKind::Colon);
        let ty = self.parse_alloc_ty();
        let default = if self.eat(TokenKind::ColonEq) {
            Some(self.parse_alloc_expr())
        } else {
            None
        };
        RecDefField {
            name,
            ty,
            default,
            span: self.finish_span(start),
        }
    }

    fn parse_optional_bracket_params(&mut self) -> Vec<TyParam> {
        if self.eat(TokenKind::LBracket) {
            let params = self.comma_sep(TokenKind::RBracket, Self::parse_single_bracket_param);
            let _rb = self.expect(TokenKind::RBracket);
            params
        } else {
            vec![]
        }
    }

    fn parse_single_bracket_param(&mut self) -> TyParam {
        let start = self.start_span();
        if !self.at(TokenKind::TyIdent) {
            let span = self.peek().span;
            let _err = self
                .diags
                .report(&ParseError::ExpectedTypeVariable, span, self.file_id);
        }
        let name = self.expect_symbol();
        TyParam {
            name,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_binding_immut_body(&mut self, kind: BindKind, start: u32) -> Expr {
        let heap = self.eat(TokenKind::KwRef);
        let pat = self.parse_alloc_pat();
        let params = self.parse_optional_bracket_params();
        let constraints = if params.is_empty() {
            vec![]
        } else {
            self.parse_opt_where_clause()
        };
        let ty = self.parse_opt_ty_annot();
        // `:= value` is required unless a type annotation is present (stub declaration)
        let value = if self.at(TokenKind::ColonEq) {
            let _ceq = self.bump();
            // use no-in variant so `in` is not consumed as a binary operator
            // when this is a let-in scoped binding
            let v = if kind == BindKind::Immut {
                self.parse_alloc_expr_no_in()
            } else {
                self.parse_alloc_expr()
            };
            Some(v)
        } else if ty.is_some() {
            None
        } else {
            let _ceq = self.expect(TokenKind::ColonEq);
            None
        };

        let fields = LetFields {
            kind,
            heap,
            pat,
            params,
            constraints,
            ty,
            value,
            span: self.finish_span(start),
        };

        // let ... in ( body ) form
        if kind == BindKind::Immut && self.eat(TokenKind::KwIn) {
            let _lp = self.expect(TokenKind::LParen);
            let body_expr = self.parse_alloc_expr();
            let _rp = self.expect(TokenKind::RParen);
            return Expr::Let {
                fields,
                body: Some(body_expr),
                span: self.finish_span(start),
            };
        }

        Expr::Let {
            fields,
            body: None,
            span: self.finish_span(start),
        }
    }
}
