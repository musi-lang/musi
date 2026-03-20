//! Export and annotated declaration parsing.

use msc_ast::attr::Attr;
use msc_ast::expr::Expr;
use msc_lex::token::TokenKind;

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    /// Parses `attrs ast_decl`.
    pub(crate) fn parse_expr_annotated_chain(&mut self) -> Expr {
        let start = self.start_span();
        let attrs = self.parse_attrs();
        match self.peek_kind() {
            TokenKind::KwExport => self.parse_expr_annotated_export(start, attrs),
            TokenKind::KwLet => self.parse_expr_annotated_binding(start, attrs),
            TokenKind::KwClass => self.parse_expr_annotated_class(start, attrs),
            TokenKind::KwInstance => self.parse_expr_annotated_given(start, attrs),
            TokenKind::KwEffect => self.parse_expr_annotated_effect(start, attrs),
            TokenKind::KwForeign => self.parse_expr_annotated_foreign(start, attrs),
            _ => self.error_expr(&ParseError::ExpectedDeclAfterAttrs),
        }
    }

    fn parse_expr_annotated_export(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let exported = true;
        let _export = self.bump();

        match self.peek_kind() {
            TokenKind::KwLet => {
                let inner = self.parse_expr_let();
                if let Expr::Let { fields, .. } = inner {
                    return Expr::Let {
                        exported,
                        fields,
                        span: self.finish_span(start),
                    };
                }
                inner
            }
            TokenKind::KwClass => {
                let mut inner = self.parse_expr_class();
                if let Expr::Class {
                    ref mut exported, ..
                } = inner
                {
                    *exported = true;
                }
                if attrs.is_empty() {
                    inner
                } else {
                    let inner_idx = self.alloc_expr(inner);
                    Expr::Annotated {
                        attrs,
                        inner: inner_idx,
                        span: self.finish_span(start),
                    }
                }
            }
            TokenKind::KwInstance => {
                let mut inner = self.parse_expr_instance();
                if let Expr::Instance {
                    ref mut exported, ..
                } = inner
                {
                    *exported = true;
                }
                if attrs.is_empty() {
                    inner
                } else {
                    let inner_idx = self.alloc_expr(inner);
                    Expr::Annotated {
                        attrs,
                        inner: inner_idx,
                        span: self.finish_span(start),
                    }
                }
            }
            TokenKind::KwEffect => {
                let mut inner = self.parse_expr_effect();
                if let Expr::Effect {
                    ref mut exported, ..
                } = inner
                {
                    *exported = true;
                }
                if attrs.is_empty() {
                    inner
                } else {
                    let inner_idx = self.alloc_expr(inner);
                    Expr::Annotated {
                        attrs,
                        inner: inner_idx,
                        span: self.finish_span(start),
                    }
                }
            }
            TokenKind::KwForeign => {
                let inner = self.parse_expr_foreign_exported();
                let inner_idx = self.alloc_expr(inner);
                Expr::Annotated {
                    attrs,
                    inner: inner_idx,
                    span: self.finish_span(start),
                }
            }
            _ => self.error_expr(&ParseError::ExpectedAfterExport),
        }
    }

    fn parse_expr_annotated_binding(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = self.parse_expr_let();
        if let Expr::Let { fields, .. } = inner {
            let binding = Expr::Let {
                exported: false,
                fields,
                span: self.finish_span(start),
            };
            let binding_idx = self.alloc_expr(binding);
            Expr::Annotated {
                attrs,
                inner: binding_idx,
                span: self.finish_span(start),
            }
        } else {
            inner
        }
    }

    fn parse_expr_annotated_class(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = self.parse_expr_class();
        let inner_idx = self.alloc_expr(inner);
        Expr::Annotated {
            attrs,
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_annotated_given(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = self.parse_expr_instance();
        let inner_idx = self.alloc_expr(inner);
        Expr::Annotated {
            attrs,
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_annotated_foreign(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = self.parse_expr_foreign();
        let inner_idx = self.alloc_expr(inner);
        Expr::Annotated {
            attrs,
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_expr_annotated_effect(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = self.parse_expr_effect();
        let inner_idx = self.alloc_expr(inner);
        Expr::Annotated {
            attrs,
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    pub(crate) fn parse_expr_export(&mut self) -> Expr {
        let start = self.start_span();
        let _export = self.bump();

        self.parse_expr_export_rest(start)
    }

    fn parse_expr_export_rest(&mut self, start: u32) -> Expr {
        match self.peek_kind() {
            TokenKind::KwLet => self.parse_export_binding(start),
            TokenKind::KwClass => {
                let mut inner = self.parse_expr_class();
                if let Expr::Class {
                    ref mut exported, ..
                } = inner
                {
                    *exported = true;
                }
                inner
            }
            TokenKind::KwInstance => {
                let mut inner = self.parse_expr_instance();
                if let Expr::Instance {
                    ref mut exported, ..
                } = inner
                {
                    *exported = true;
                }
                inner
            }
            TokenKind::KwEffect => {
                let mut inner = self.parse_expr_effect();
                if let Expr::Effect {
                    ref mut exported, ..
                } = inner
                {
                    *exported = true;
                }
                inner
            }
            TokenKind::KwForeign => self.parse_expr_foreign_exported(),
            _ => self.error_expr(&ParseError::ExpectedAfterExport),
        }
    }

    fn parse_export_binding(&mut self, start: u32) -> Expr {
        let inner = self.parse_expr_let();
        if let Expr::Let { fields, .. } = inner {
            Expr::Let {
                exported: true,
                fields,
                span: self.finish_span(start),
            }
        } else {
            inner
        }
    }
}
