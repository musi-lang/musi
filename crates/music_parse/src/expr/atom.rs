//! Atom parsing: literals, names, records, arrays, variants, return, import.

use music_ast::expr::{ArrayElem, Expr, RecField};
use music_ast::lit::{FStrPart, Lit};
use music_lex::token::{Token, TokenKind};
use music_shared::Symbol;

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    pub(super) fn parse_expr_lit(&mut self) -> Expr {
        let start = self.start_span();
        let lit = self.parse_lit_value();
        Expr::Lit {
            lit,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_expr_lit_fstr(&mut self) -> Expr {
        let start = self.start_span();
        let mut parts = vec![];

        let head_tok = self.bump();
        push_fstr_text(head_tok, &mut parts);

        // Middle/Tail loop
        loop {
            let expr = self.parse_expr();
            let expr_idx = self.alloc_expr(expr);
            let expr_span = self.finish_span(start);
            parts.push(FStrPart::Interpolated {
                expr: expr_idx,
                span: expr_span,
            });

            match self.peek_kind() {
                TokenKind::FStringMiddle => {
                    let mid = self.bump();
                    push_fstr_text(mid, &mut parts);
                }
                TokenKind::FStringTail => {
                    let tail = self.bump();
                    push_fstr_text(tail, &mut parts);
                    break;
                }
                _ => {
                    let _diag = self.diags.report(
                        &ParseError::UnterminatedFString,
                        self.peek().span,
                        self.file_id,
                    );
                    break;
                }
            }
        }

        let span = self.finish_span(start);
        Expr::Lit {
            lit: Lit::FStr { parts, span },
            span,
        }
    }

    pub(super) fn parse_expr_record_or_name(&mut self) -> Expr {
        let start = self.start_span();
        let tok = self.bump();
        let span = tok.span;
        let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));

        // check for named record literal: Name.{ ... }
        if self.at(TokenKind::DotLBrace) {
            let _dlb = self.bump();
            let fields = self.comma_sep(TokenKind::RBrace, Self::parse_rec_field);
            let _rb = self.expect(TokenKind::RBrace);
            return Expr::Record {
                ty_name: Some(sym),
                fields,
                span: self.finish_span(start),
            };
        }

        let name_span = self.finish_span(span.start);
        let name_ref = self.alloc_name_ref(sym, name_span);
        Expr::Name {
            name_ref,
            span: name_span,
        }
    }

    pub(crate) fn parse_rec_field(&mut self) -> RecField {
        let start = self.start_span();

        // Spread: ... expr
        if self.eat(TokenKind::DotDotDot) {
            let expr = self.parse_alloc_expr();
            return RecField::Spread {
                expr,
                span: self.finish_span(start),
            };
        }

        let name = self.expect_symbol();
        let value = if self.eat(TokenKind::ColonEq) {
            Some(self.parse_alloc_expr())
        } else {
            None
        };
        RecField::Named {
            name,
            value,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_array_expr(&mut self) -> Expr {
        let start = self.start_span();
        let _lb = self.bump();
        let mut elems = vec![];

        if !self.at(TokenKind::RBracket) {
            loop {
                if self.eat(TokenKind::DotDotDot) {
                    let expr = self.parse_alloc_expr();
                    let span = self.finish_span(start);
                    elems.push(ArrayElem::Spread { expr, span });
                } else {
                    let expr = self.parse_alloc_expr();
                    let span = self.finish_span(start);
                    elems.push(ArrayElem::Elem { expr, span });
                }
                if !self.eat(TokenKind::Comma) {
                    break;
                }
                if self.at(TokenKind::RBracket) {
                    break;
                }
            }
        }
        let _rb = self.expect(TokenKind::RBracket);
        Expr::Array {
            elems,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_expr_record_anon(&mut self) -> Expr {
        let start = self.start_span();
        let _dlb = self.bump();
        let fields = self.comma_sep(TokenKind::RBrace, Self::parse_rec_field);
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Record {
            ty_name: None,
            fields,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_expr_variant(&mut self) -> Expr {
        let start = self.start_span();
        let _dot = self.bump();
        let name = self.expect_symbol();
        let args = if self.at(TokenKind::LParen) {
            self.delimited(TokenKind::LParen, TokenKind::RParen, Self::parse_alloc_expr)
        } else {
            vec![]
        };
        Expr::Variant {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_expr_return(&mut self) -> Expr {
        let start = self.start_span();
        let _ret = self.bump();
        let value = self.parse_opt_expr();
        Expr::Return {
            value,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_expr_import(&mut self) -> Expr {
        let start = self.start_span();
        let _import = self.bump();
        let tok = self.bump();
        let span = tok.span;
        let path = tok.symbol.unwrap_or_else(|| {
            let _diag = self
                .diags
                .report(&ParseError::ExpectedImportPath, span, self.file_id);
            Symbol(u32::MAX)
        });
        let alias = if self.eat(TokenKind::KwAs) {
            Some(self.expect_symbol())
        } else {
            None
        };
        Expr::Import {
            path,
            alias,
            span: self.finish_span(start),
        }
    }
}

/// Pushes an `FStrPart::Text` if the token carries interned text.
fn push_fstr_text(tok: &Token, parts: &mut Vec<FStrPart>) {
    if let Some(sym) = tok.symbol {
        parts.push(FStrPart::Text {
            raw: sym,
            span: tok.span,
        });
    }
}
