//! Type parsing.

#[cfg(test)]
mod tests;

use msc_ast::ExprIdx;
use msc_ast::NameRef;
use msc_ast::expr::{Arrow, EffectItem, EffectSet, Expr, FieldKey};
use msc_ast::ty_param::{Constraint, Rel};
use msc_lex::token::TokenKind;
use msc_shared::Symbol;

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    /// Parses a full type: `ty_arrow`.
    pub(crate) fn parse_ty(&mut self) -> Expr {
        self.parse_ty_arrow()
    }

    /// `ty_arrow = ty_eff { ('->' | '~>') ty_eff }`.
    fn parse_ty_arrow(&mut self) -> Expr {
        let start = self.start_span();
        let (first, first_eff) = self.parse_ty_eff();

        if !self.at(TokenKind::DashGt) && !self.at(TokenKind::TildeGt) {
            // No arrow follows - discard any effect set (it only applies to fn types).
            return first;
        }

        self.parse_ty_arrow_chain(start, first, first_eff)
    }

    /// Parses the chain of arrow types starting with `first`, at source span `start`.
    fn parse_ty_arrow_chain(
        &mut self,
        start: u32,
        first: Expr,
        first_eff: Option<EffectSet>,
    ) -> Expr {
        let mut parts: Vec<(Expr, Option<EffectSet>)> = vec![(first, first_eff)];
        let mut arrows = vec![];
        while self.at(TokenKind::DashGt) || self.at(TokenKind::TildeGt) {
            let arrow = if self.eat(TokenKind::DashGt) {
                Arrow::Pure
            } else {
                let _tg = self.bump();
                Arrow::Effectful
            };
            arrows.push(arrow);
            parts.push(self.parse_ty_eff());
        }

        // Fold right: the effect set from each return-type segment attaches to
        // the `Expr::FnType` node whose return type it annotates.
        let (mut ret, mut ret_eff) = parts.pop().expect("at least two parts");
        while let Some(arrow) = arrows.pop() {
            let (param, _param_eff) = parts.pop().expect("matching param");
            let (params, variadic) = match param {
                Expr::ProductType {
                    fields, variadic, ..
                } => (fields, variadic),
                other => (vec![self.alloc_expr(other)], false),
            };
            let ret_idx = self.alloc_expr(ret);
            ret = Expr::FnType {
                params,
                ret: ret_idx,
                arrow,
                effects: ret_eff,
                variadic,
                span: self.finish_span(start),
            };
            ret_eff = None;
        }
        ret
    }

    /// `ty_eff = ty_sum [ 'with' effect_set ] | 'with' effect_set`.
    ///
    /// The second form (`~> with { IO }`) is sugar for `~> () with { IO }`.
    fn parse_ty_eff(&mut self) -> (Expr, Option<EffectSet>) {
        if self.at(TokenKind::KwWith) {
            let start = self.start_span();
            let _with = self.bump();
            let effects = Some(self.parse_effect_set());
            let unit = Expr::ProductType {
                fields: vec![],
                variadic: false,
                span: self.finish_span(start),
            };
            return (unit, effects);
        }
        let ty = self.parse_ty_sum();
        let effects = if self.eat(TokenKind::KwWith) {
            Some(self.parse_effect_set())
        } else {
            None
        };
        (ty, effects)
    }

    /// `ty_sum = ty_prod { '+' ty_prod }`.
    fn parse_ty_sum(&mut self) -> Expr {
        let start = self.start_span();
        let first = self.parse_ty_prod();
        if !self.at(TokenKind::Plus) {
            return first;
        }
        let mut variants = vec![self.alloc_expr(first)];
        while self.eat(TokenKind::Plus) {
            let t = self.parse_ty_prod();
            variants.push(self.alloc_expr(t));
        }
        Expr::SumType {
            variants,
            span: self.finish_span(start),
        }
    }

    /// `ty_prod = ty_base { '*' ty_base }`.
    fn parse_ty_prod(&mut self) -> Expr {
        let start = self.start_span();
        let first = self.parse_ty_base();
        if !self.at(TokenKind::Star) {
            return first;
        }
        let mut fields = vec![self.alloc_expr(first)];
        while self.eat(TokenKind::Star) {
            let t = self.parse_ty_base();
            fields.push(self.alloc_expr(t));
        }
        Expr::ProductType {
            fields,
            variadic: false,
            span: self.finish_span(start),
        }
    }

    /// Base types: var, option, named, paren, array.
    fn parse_ty_base(&mut self) -> Expr {
        match self.peek_kind() {
            TokenKind::TyIdent => self.parse_ty_var(),
            TokenKind::Question => self.parse_ty_option(),
            TokenKind::Ident => self.parse_ty_named(),
            TokenKind::LParen => self.parse_ty_paren_or_tuple(),
            TokenKind::LBracket => self.parse_ty_array(),
            _ => self.error_expr(&ParseError::ExpectedType),
        }
    }

    fn parse_ty_var(&mut self) -> Expr {
        let start = self.start_span();
        let name = self.expect_symbol();
        let span = self.finish_span(start);
        let name_ref = self.arenas.name_refs.alloc(NameRef {
            name,
            span,
            is_ty_var: true,
        });
        Expr::Name { name_ref, span }
    }

    fn parse_ty_option(&mut self) -> Expr {
        let start = self.start_span();
        let _q = self.bump();
        let inner = self.parse_ty_base();
        let inner_idx = self.alloc_expr(inner);
        Expr::OptionType {
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_named(&mut self) -> Expr {
        let start = self.start_span();
        let name = self.expect_symbol();
        let name_span = self.finish_span(start);
        if self.at(TokenKind::Dot) && self.peek2() == TokenKind::Ident {
            let _dot = self.bump();
            let qualified_name = self.expect_symbol();
            let args = if self.eat(TokenKind::KwOf) {
                self.parse_ty_arg_list()
            } else {
                vec![]
            };
            let module_ref = self.alloc_name_ref(name, name_span);
            let qual_span = self.finish_span(start);
            let mod_expr = self.alloc_expr(Expr::Name {
                name_ref: module_ref,
                span: name_span,
            });
            let field_expr = Expr::Field {
                object: mod_expr,
                field: FieldKey::Name {
                    name: qualified_name,
                    span: qual_span,
                },
                safe: false,
                span: qual_span,
            };
            return if args.is_empty() {
                field_expr
            } else {
                let callee = self.alloc_expr(field_expr);
                Expr::TypeApp {
                    callee,
                    args,
                    span: qual_span,
                }
            };
        }
        let args = if self.eat(TokenKind::KwOf) {
            self.parse_ty_arg_list()
        } else {
            vec![]
        };
        let name_ref = self.alloc_name_ref(name, name_span);
        if args.is_empty() {
            Expr::Name {
                name_ref,
                span: name_span,
            }
        } else {
            let callee = self.alloc_expr(Expr::Name {
                name_ref,
                span: name_span,
            });
            let span = self.finish_span(start);
            Expr::TypeApp { callee, args, span }
        }
    }

    fn parse_ty_paren_or_tuple(&mut self) -> Expr {
        let start = self.start_span();

        // Disambiguate pi type: `(ident : ty) -> ty` vs grouped/tuple type.
        // peek_at(0) = LParen, peek_at(1) = possible Ident, peek_at(2) = possible Colon.
        if self.peek_at(1).kind == TokenKind::Ident && self.peek_at(2).kind == TokenKind::Colon {
            return self.parse_ty_pi(start);
        }

        let _lp = self.bump();
        if self.eat(TokenKind::RParen) {
            return Expr::ProductType {
                fields: vec![],
                variadic: false,
                span: self.finish_span(start),
            };
        }
        let first = self.parse_ty();
        if self.eat(TokenKind::RParen) {
            return first;
        }

        let mut fields = vec![self.alloc_expr(first)];
        while self.eat(TokenKind::Comma) {
            if self.at(TokenKind::RParen) {
                break;
            }
            if self.at(TokenKind::DotDotDot) {
                let _dots = self.bump();
                let _rp = self.expect(TokenKind::RParen);
                return Expr::ProductType {
                    fields,
                    variadic: true,
                    span: self.finish_span(start),
                };
            }
            let t = self.parse_ty();
            fields.push(self.alloc_expr(t));
        }
        let _rp = self.expect(TokenKind::RParen);
        Expr::ProductType {
            fields,
            variadic: false,
            span: self.finish_span(start),
        }
    }

    /// Parses a pi type: `(ident : ty) -> ty`.
    ///
    /// Called only when `peek_at(1)` is `Ident` and `peek_at(2)` is `Colon`,
    /// so the lookahead guarantee holds on entry.
    fn parse_ty_pi(&mut self, start: u32) -> Expr {
        let _lp = self.bump(); // (
        let param = self.expect_symbol(); // ident
        let _colon = self.bump(); // :
        let param_ty = self.parse_alloc_ty();
        let _rp = self.expect(TokenKind::RParen);
        let _arrow = self.expect(TokenKind::DashGt);
        let body = self.parse_alloc_ty();
        Expr::PiType {
            param,
            param_ty,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_array(&mut self) -> Expr {
        let start = self.start_span();
        let _lb = self.bump();
        let len = if self.at(TokenKind::IntLit) {
            let tok = self.bump();
            let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
            let text = self.resolve(sym);
            let n = text.replace('_', "").parse::<u32>().unwrap_or(0);
            Some(n)
        } else {
            None
        };
        let _rb = self.expect(TokenKind::RBracket);
        // Use parse_ty_base (not parse_ty) so that `+` and `->` are not
        // consumed greedily - they belong to the enclosing context.
        // Complex element types need parentheses: `[](Int -> Bool)`.
        let elem = self.parse_ty_base();
        let elem_idx = self.alloc_expr(elem);
        Expr::ArrayType {
            len,
            elem: elem_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_arg_list(&mut self) -> Vec<ExprIdx> {
        let first = self.parse_ty_base();
        let mut args = vec![self.alloc_expr(first)];
        while self.eat(TokenKind::Comma) {
            let t = self.parse_ty_base();
            args.push(self.alloc_expr(t));
        }
        args
    }

    /// Parses a named type reference: `Name [ 'of' type_args ]`.
    pub(crate) fn parse_ty_named_ref(&mut self) -> ExprIdx {
        let start = self.start_span();
        let name = self.expect_symbol();
        let name_span = self.finish_span(start);
        let name_ref = self.alloc_name_ref(name, name_span);
        let args = if self.eat(TokenKind::KwOf) {
            self.parse_ty_arg_list()
        } else {
            vec![]
        };
        if args.is_empty() {
            self.alloc_expr(Expr::Name {
                name_ref,
                span: name_span,
            })
        } else {
            let callee = self.alloc_expr(Expr::Name {
                name_ref,
                span: name_span,
            });
            let span = self.finish_span(start);
            self.alloc_expr(Expr::TypeApp { callee, args, span })
        }
    }

    /// Parses `'where' constraint { ',' constraint }`, or empty vec.
    pub(crate) fn parse_opt_where_clause(&mut self) -> Vec<Constraint> {
        if !self.eat(TokenKind::KwWhere) {
            return vec![];
        }
        let mut constraints = vec![];
        loop {
            if !self.at(TokenKind::TyIdent) {
                let span = self.peek().span;
                let _err = self
                    .diags
                    .report(&ParseError::ExpectedTypeVariable, span, self.file_id);
                break;
            }
            let c_start = self.start_span();
            let param = self.expect_symbol();
            let rel = if self.eat(TokenKind::LtColon) {
                Rel::Sub
            } else if self.eat(TokenKind::ColonGt) {
                Rel::Super
            } else if self.eat(TokenKind::Colon) {
                Rel::Member
            } else {
                let _span = self.expect(TokenKind::LtColon);
                Rel::Sub
            };
            let bound = self.parse_ty_named_ref();
            constraints.push(Constraint {
                param,
                rel,
                bound,
                span: self.finish_span(c_start),
            });
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        constraints
    }

    /// Optional type annotation: `:` ty.
    pub(crate) fn parse_opt_ty_annot(&mut self) -> Option<ExprIdx> {
        if self.eat(TokenKind::Colon) {
            Some(self.parse_alloc_ty())
        } else {
            None
        }
    }

    pub(crate) fn parse_effect_set(&mut self) -> EffectSet {
        let start = self.start_span();
        let _lb = self.expect(TokenKind::LBrace);
        let effects = self.comma_sep(TokenKind::RBrace, Self::parse_effect_item);
        let _rb = self.expect(TokenKind::RBrace);
        EffectSet {
            effects,
            span: self.finish_span(start),
        }
    }

    fn parse_effect_item(&mut self) -> EffectItem {
        let start = self.start_span();
        if self.at(TokenKind::TyIdent) {
            let name = self.expect_symbol();
            return EffectItem::Var {
                name,
                span: self.finish_span(start),
            };
        }
        let name = self.expect_symbol();
        let arg = if self.eat(TokenKind::KwOf) {
            let t = self.parse_ty();
            Some(self.alloc_expr(t))
        } else {
            None
        };
        EffectItem::Named {
            name,
            arg,
            span: self.finish_span(start),
        }
    }
}
