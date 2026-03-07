//! Type and pattern parsing.

use musi_lex::token::TokenKind;
use crate::ast::{Constraint, Pat, PatField, PatSuffix, Ty, TyParam};

use super::Parser;

impl<'a> Parser<'a> {
    pub(super) fn parse_ty_named(&mut self) -> Ty {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::LBracket) {
            let tys = self.parse_separated_list(TokenKind::RBracket, Parser::parse_ty);
            let _rb = self.expect(TokenKind::RBracket);
            tys
        } else {
            Vec::new()
        };
        Ty::Named {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_opt_where_clause(&mut self) -> Vec<Constraint> {
        if !self.eat(TokenKind::Where) {
            return Vec::new();
        }
        let mut constraints = Vec::new();
        loop {
            let c_start = self.start_span();
            let ty_var = self.expect_symbol();
            let _sat = self.expect(TokenKind::Satisfies);
            let bound = self.parse_ty_named();
            constraints.push(Constraint {
                ty_var,
                bound,
                span: self.finish_span(c_start),
            });
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        constraints
    }

    pub(super) fn parse_ty(&mut self) -> Ty {
        let start = self.start_span();
        let first = self.parse_ty_noarrow();

        // Check for function type: T1 -> T2
        if self.at(TokenKind::MinusGt) {
            let mut params = vec![first];
            while self.eat(TokenKind::MinusGt) {
                params.push(self.parse_ty_noarrow());
            }
            // The last element is the return type
            let ret = Box::new(params.pop().expect("at least one parsed"));
            return Ty::Arrow {
                params,
                ret,
                span: self.finish_span(start),
            };
        }
        first
    }

    fn parse_ty_noarrow(&mut self) -> Ty {
        match self.peek_kind() {
            TokenKind::Question => {
                let start = self.start_span();
                let _q = self.advance();
                let inner = Box::new(self.parse_ty_noarrow());
                Ty::Option {
                    inner,
                    span: self.finish_span(start),
                }
            }
            TokenKind::TyIdent => {
                let start = self.start_span();
                let name = self.expect_symbol();
                Ty::Var {
                    name,
                    span: self.finish_span(start),
                }
            }
            TokenKind::Ident => self.parse_ty_named(),
            TokenKind::LParen => {
                let start = self.start_span();
                let elements =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_ty);
                Ty::Prod {
                    elements,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBracket => {
                let start = self.start_span();
                let _lb = self.advance();
                let size = if self.at(TokenKind::IntLit) {
                    let lit_expr = self.parse_lit();
                    Some(self.alloc_expr(lit_expr))
                } else {
                    None
                };
                let _rb = self.expect(TokenKind::RBracket);
                let element = Box::new(self.parse_ty());
                Ty::Arr {
                    element,
                    size,
                    span: self.finish_span(start),
                }
            }
            _ => {
                let span = self.peek().span;
                let _diag = self
                    .diags
                    .error("expected type expression", span, self.file_id);
                Ty::Error {
                    span: self.finish_span(span.start),
                }
            }
        }
    }

    pub(super) fn parse_opt_ty_params(&mut self) -> Vec<TyParam> {
        if !self.at(TokenKind::LBracket) {
            return Vec::new();
        }
        self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, |p| {
            let start = p.start_span();
            let name = p.expect_symbol();
            TyParam {
                name,
                bounds: Vec::new(),
                span: p.finish_span(start),
            }
        })
    }

    pub(super) fn parse_pat(&mut self) -> Pat {
        let start = self.start_span();
        let mut primary = self.parse_pat_primary();

        // Check for `or` alternatives
        if self.at(TokenKind::Or) {
            let mut alternatives = vec![primary];
            while self.eat(TokenKind::Or) {
                alternatives.push(self.parse_pat_primary());
            }
            primary = Pat::Or {
                alternatives,
                span: self.finish_span(start),
            };
        }
        primary
    }

    fn parse_pat_primary(&mut self) -> Pat {
        match self.peek_kind() {
            TokenKind::Ident => self.parse_pat_ident(false),
            TokenKind::Var => {
                let _ = self.advance();
                self.parse_pat_ident(true)
            }
            TokenKind::Dot => {
                let start = self.start_span();
                let _ = self.advance();
                let name = self.expect_symbol();
                let args = if self.at(TokenKind::LParen) {
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_pat)
                } else {
                    Vec::new()
                };
                Pat::DotPrefix {
                    name,
                    args,
                    span: self.finish_span(start),
                }
            }
            TokenKind::Underscore => {
                let start = self.start_span();
                let _us = self.advance();
                Pat::Wild {
                    span: self.finish_span(start),
                }
            }
            TokenKind::IntLit | TokenKind::FloatLit | TokenKind::StringLit | TokenKind::CharLit => {
                let start = self.start_span();
                let value = self.parse_lit_value();
                Pat::Lit {
                    value,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LParen => {
                let start = self.start_span();
                let elements =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_pat);
                Pat::Prod {
                    elements,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBracket => {
                let start = self.start_span();
                let elements =
                    self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, Parser::parse_pat);
                Pat::Arr {
                    elements,
                    span: self.finish_span(start),
                }
            }
            TokenKind::LBrace => {
                let start = self.start_span();
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, Parser::parse_pat_field);
                Pat::AnonRec {
                    fields,
                    span: self.finish_span(start),
                }
            }
            _ => {
                let span = self.peek().span;
                let _diag = self.diags.error("expected pattern", span, self.file_id);
                self.recover();
                Pat::Error {
                    span: self.finish_span(span.start),
                }
            }
        }
    }

    fn parse_pat_ident(&mut self, is_mut: bool) -> Pat {
        let start = self.start_span();
        let name = self.expect_symbol();
        match self.peek_kind() {
            // Positional sum pattern: Name(p1, p2)
            TokenKind::LParen => {
                let args =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_pat);
                let span = self.finish_span(start);
                Pat::Ident {
                    name,
                    suffix: Some(PatSuffix::Positional { args, span }),
                    is_mut,
                    span,
                }
            }
            // Named-field pattern: Name{ f: p }
            TokenKind::LBrace => {
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, Parser::parse_pat_field);
                let span = self.finish_span(start);
                Pat::Ident {
                    name,
                    suffix: Some(PatSuffix::Named { fields, span }),
                    is_mut,
                    span,
                }
            }
            // Bare ident -- variable binding
            _ => Pat::Ident {
                name,
                suffix: None,
                is_mut,
                span: self.finish_span(start),
            },
        }
    }

    pub(super) fn parse_pat_field(&mut self) -> PatField {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let pat = self.parse_option(TokenKind::Colon, Parser::parse_pat);
        PatField {
            attrs,
            mutable,
            name,
            pat,
            span: self.finish_span(start),
        }
    }
}
