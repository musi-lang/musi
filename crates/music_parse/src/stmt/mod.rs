//! Declaration parsing: class, given, effect, foreign, attrs, fn params, exports.

mod attr;
mod binding;
mod class;
mod export;
mod foreign;

#[cfg(test)]
mod tests;

use music_ast::expr::{Expr, Param, ParamMode};
use music_ast::ty::Quantifier;
use music_lex::token::TokenKind;
use music_shared::{Span, Symbol};

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    pub(crate) fn parse_param(&mut self) -> Param {
        let start = self.start_span();
        let mode = if self.eat(TokenKind::KwInout) {
            ParamMode::Inout
        } else if self.eat(TokenKind::KwVar) {
            ParamMode::Var
        } else if self.eat(TokenKind::KwRef) {
            ParamMode::Ref
        } else {
            ParamMode::Plain
        };
        let name = self.expect_symbol();
        let ty = self.parse_opt_ty_annot();
        let default = if self.eat(TokenKind::ColonEq) {
            Some(self.parse_alloc_expr())
        } else {
            None
        };
        Param {
            mode,
            name,
            ty,
            default,
            span: self.finish_span(start),
        }
    }

    pub(crate) fn parse_expr_quantified(&mut self, kind: Quantifier) -> Expr {
        let start = self.start_span();
        let _kw = self.bump();
        let params = self.parse_ty_param_list();
        let constraints = self.parse_opt_where_clause();
        let _arrow = self.expect(TokenKind::DashGt);
        let body = self.parse_alloc_expr();
        Expr::Quantified {
            kind,
            params,
            constraints,
            body,
            span: self.finish_span(start),
        }
    }

    /// Converts parsed expressions into function parameters.
    /// Each expression must be a Name, optionally with a type annotation following.
    pub(crate) fn reinterpret_as_params(&mut self, exprs: &[Expr]) -> Vec<Param> {
        exprs
            .iter()
            .map(|expr| self.reinterpret_single_param(expr))
            .collect()
    }

    fn reinterpret_single_param(&mut self, expr: &Expr) -> Param {
        if let Expr::Name { name, span, .. } = expr {
            Param {
                mode: ParamMode::Plain,
                name: *name,
                ty: None,
                default: None,
                span: *span,
            }
        } else {
            let span = if let Expr::Error { span } = expr {
                *span
            } else {
                Span::DUMMY
            };
            let _diag = self
                .diags
                .report(&ParseError::ExpectedParamName, span, self.file_id);
            Param {
                mode: ParamMode::Plain,
                name: Symbol(u32::MAX),
                ty: None,
                default: None,
                span,
            }
        }
    }
}
