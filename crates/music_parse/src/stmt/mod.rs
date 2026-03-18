//! Declaration parsing: class, given, effect, foreign, attrs, fn params, exports.

mod attr;
mod binding;
mod class;
mod export;
mod foreign;

#[cfg(test)]
mod tests;

use music_ast::expr::{Expr, Param, ParamMode};
use music_lex::token::TokenKind;
use music_shared::{Span, Symbol};

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    pub(crate) fn parse_param(&mut self) -> Param {
        let start = self.start_span();
        let mode = if self.eat(TokenKind::KwMut) {
            ParamMode::Mut
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

    /// Converts parsed expressions into function parameters.
    /// Each expression must be a Name, optionally with a type annotation following.
    pub(crate) fn reinterpret_as_params(&mut self, exprs: &[Expr]) -> Vec<Param> {
        exprs
            .iter()
            .map(|expr| self.reinterpret_single_param(expr))
            .collect()
    }

    fn reinterpret_single_param(&mut self, expr: &Expr) -> Param {
        if let Expr::Name { name_ref, span, .. } = expr {
            let name = self.arenas.name_refs[*name_ref].name;
            Param {
                mode: ParamMode::Plain,
                name,
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
