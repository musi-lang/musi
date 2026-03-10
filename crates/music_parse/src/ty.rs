//! Type parsing.

#[cfg(test)]
mod tests;

use music_ast::ExprIdx;
use music_ast::TyIdx;
use music_ast::expr::Arrow;
use music_ast::ty::{
    Constraint, EffectItem, EffectSet, Quantifier, Rel, Ty, TyNamedRef, TyParam, TyRecField,
};
use music_lex::token::TokenKind;
use music_shared::Symbol;

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    /// Parses a full type: `ty_arrow`.
    pub(crate) fn parse_ty(&mut self) -> Ty {
        self.parse_ty_arrow()
    }

    /// `ty_arrow = ty_eff { ('->' | '~>') ty_eff }`.
    fn parse_ty_arrow(&mut self) -> Ty {
        let start = self.start_span();
        let (first, first_eff) = self.parse_ty_eff();

        if !self.at(TokenKind::DashGt) && !self.at(TokenKind::TildeGt) {
            // No arrow follows — discard any effect set (it only applies to fn types).
            return first;
        }

        self.parse_ty_arrow_chain(start, first, first_eff)
    }

    /// Parses the chain of arrow types starting with `first`, at source span `start`.
    fn parse_ty_arrow_chain(&mut self, start: u32, first: Ty, first_eff: Option<EffectSet>) -> Ty {
        let mut parts: Vec<(Ty, Option<EffectSet>)> = vec![(first, first_eff)];
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
        // the `Ty::Fn` node whose return type it annotates.
        let (mut ret, mut ret_eff) = parts.pop().expect("at least two parts");
        while let Some(arrow) = arrows.pop() {
            let (param, _param_eff) = parts.pop().expect("matching param");
            let params = match param {
                Ty::Product { fields, .. } => fields,
                other => {
                    vec![self.alloc_ty(other)]
                }
            };
            let ret_idx = self.alloc_ty(ret);
            ret = Ty::Fn {
                params,
                ret: ret_idx,
                arrow,
                effects: ret_eff,
                span: self.finish_span(start),
            };
            ret_eff = None;
        }
        ret
    }

    /// `ty_eff = ty_sum [ 'under' effect_set ]`.
    fn parse_ty_eff(&mut self) -> (Ty, Option<EffectSet>) {
        let ty = self.parse_ty_sum();
        let effects = if self.eat(TokenKind::KwUnder) {
            Some(self.parse_effect_set())
        } else {
            None
        };
        (ty, effects)
    }

    /// `ty_sum = ty_prod { '+' ty_prod }`.
    fn parse_ty_sum(&mut self) -> Ty {
        let start = self.start_span();
        let first = self.parse_ty_prod();
        if !self.at(TokenKind::Plus) {
            return first;
        }
        let mut variants = vec![self.alloc_ty(first)];
        while self.eat(TokenKind::Plus) {
            let t = self.parse_ty_prod();
            variants.push(self.alloc_ty(t));
        }
        Ty::Sum {
            variants,
            span: self.finish_span(start),
        }
    }

    /// `ty_prod = ty_base { '*' ty_base }`.
    fn parse_ty_prod(&mut self) -> Ty {
        let start = self.start_span();
        let first = self.parse_ty_base();
        if !self.at(TokenKind::Star) {
            return first;
        }
        let mut fields = vec![self.alloc_ty(first)];
        while self.eat(TokenKind::Star) {
            let t = self.parse_ty_base();
            fields.push(self.alloc_ty(t));
        }
        Ty::Product {
            fields,
            span: self.finish_span(start),
        }
    }

    /// Base types: var, option, ref, named, paren, array, record, refinement, quantified.
    fn parse_ty_base(&mut self) -> Ty {
        match self.peek_kind() {
            TokenKind::TyIdent => self.parse_ty_var(),
            TokenKind::Question => self.parse_ty_option(),
            TokenKind::KwRef => self.parse_ty_ref(),
            TokenKind::Ident => self.parse_ty_named(),
            TokenKind::LParen => self.parse_ty_paren_or_tuple(),
            TokenKind::LBracket => self.parse_ty_array(),
            TokenKind::LBrace => self.parse_ty_brace(),
            TokenKind::KwForall => self.parse_ty_quantified(Quantifier::Forall),
            TokenKind::KwExists => self.parse_ty_quantified(Quantifier::Exists),
            _ => self.error_ty(&ParseError::ExpectedType),
        }
    }

    fn parse_ty_var(&mut self) -> Ty {
        let start = self.start_span();
        let name = self.expect_symbol();
        Ty::Var {
            name,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_option(&mut self) -> Ty {
        let start = self.start_span();
        let _q = self.bump();
        let inner = self.parse_ty_base();
        let inner_idx = self.alloc_ty(inner);
        Ty::Option {
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_ref(&mut self) -> Ty {
        let start = self.start_span();
        let _ref = self.bump();
        let inner = self.parse_ty_base();
        let inner_idx = self.alloc_ty(inner);
        Ty::Ref {
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_named(&mut self) -> Ty {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::KwOf) {
            self.parse_ty_arg_list()
        } else {
            vec![]
        };
        Ty::Named {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_paren_or_tuple(&mut self) -> Ty {
        let start = self.start_span();
        let _lp = self.bump();
        if self.eat(TokenKind::RParen) {
            return Ty::Product {
                fields: vec![],
                span: self.finish_span(start),
            };
        }
        let first = self.parse_ty();
        if self.eat(TokenKind::RParen) {
            return first;
        }

        let mut fields = vec![self.alloc_ty(first)];
        while self.eat(TokenKind::Comma) {
            if self.at(TokenKind::RParen) {
                break;
            }
            let t = self.parse_ty();
            fields.push(self.alloc_ty(t));
        }
        let _rp = self.expect(TokenKind::RParen);
        Ty::Product {
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_array(&mut self) -> Ty {
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
        let elem = self.parse_ty();
        let elem_idx = self.alloc_ty(elem);
        Ty::Array {
            len,
            elem: elem_idx,
            span: self.finish_span(start),
        }
    }

    /// Disambiguates `{ fields ; ... }` (record) vs `{ T | pred }` (refinement).
    fn parse_ty_brace(&mut self) -> Ty {
        let start = self.start_span();
        let _lb = self.bump();

        if self.eat(TokenKind::RBrace) {
            return Ty::Record {
                fields: vec![],
                open: false,
                span: self.finish_span(start),
            };
        }
        if self.at(TokenKind::Ident) && self.peek2() == TokenKind::Colon {
            return self.parse_ty_record_body(start);
        }

        let ty = self.parse_ty();
        if self.eat(TokenKind::Pipe) {
            let pred = self.parse_expr();
            let pred_idx = self.alloc_expr(pred);
            let _rb = self.expect(TokenKind::RBrace);
            let base_idx = self.alloc_ty(ty);
            return Ty::Refine {
                base: base_idx,
                pred: pred_idx,
                span: self.finish_span(start),
            };
        }

        let _rb = self.expect(TokenKind::RBrace);
        self.error_ty(&ParseError::ExpectedRecordOrRefinement)
    }

    fn parse_ty_record_body(&mut self, start: u32) -> Ty {
        let fields = self.semi_sep(TokenKind::RBrace, Self::parse_ty_rec_field);
        let open = if self.eat(TokenKind::Semi) {
            self.eat(TokenKind::DotDotDot)
        } else {
            false
        };
        let _rb = self.expect(TokenKind::RBrace);
        Ty::Record {
            fields,
            open,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_rec_field(&mut self) -> TyRecField {
        let start = self.start_span();
        let name = self.expect_symbol();
        let _colon = self.expect(TokenKind::Colon);
        let ty = self.parse_alloc_ty();
        let default: Option<ExprIdx> = if self.eat(TokenKind::ColonEq) {
            Some(self.parse_alloc_expr())
        } else {
            None
        };
        TyRecField {
            name,
            ty,
            default,
            span: self.finish_span(start),
        }
    }

    fn parse_ty_quantified(&mut self, kind: Quantifier) -> Ty {
        let start = self.start_span();
        let _kw = self.bump();
        let params = self.parse_ty_param_list();
        let constraints = self.parse_opt_where_clause();
        let _arrow = self.expect(TokenKind::DashGt);
        let body = self.parse_ty();
        let body_idx = self.alloc_ty(body);
        Ty::Quantified {
            kind,
            params,
            constraints,
            body: body_idx,
            span: self.finish_span(start),
        }
    }

    /// Parses `ty_ident { ',' ty_ident }`.
    pub(crate) fn parse_ty_param_list(&mut self) -> Vec<TyParam> {
        let mut params = vec![];
        loop {
            if !self.at(TokenKind::TyIdent) {
                break;
            }
            let start = self.start_span();
            let name = self.expect_symbol();
            params.push(TyParam {
                name,
                span: self.finish_span(start),
            });
            if !self.eat(TokenKind::Comma) {
                break;
            }
        }
        params
    }

    fn parse_ty_arg_list(&mut self) -> Vec<TyIdx> {
        let mut args = vec![self.parse_alloc_ty()];
        while self.eat(TokenKind::Comma) {
            args.push(self.parse_alloc_ty());
        }
        args
    }

    /// Parses a named type reference: `Name [ 'of' type_args ]`.
    pub(crate) fn parse_ty_named_ref(&mut self) -> TyNamedRef {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::KwOf) {
            self.parse_ty_arg_list()
        } else {
            vec![]
        };
        TyNamedRef {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    /// Parses `'where' constraint { ',' constraint }`, or empty vec.
    pub(crate) fn parse_opt_where_clause(&mut self) -> Vec<Constraint> {
        if !self.eat(TokenKind::KwWhere) {
            return vec![];
        }
        let mut constraints = vec![];
        loop {
            let c_start = self.start_span();
            let param = self.expect_symbol();
            let rel = if self.eat(TokenKind::LtColon) {
                Rel::Sub
            } else if self.eat(TokenKind::ColonGt) {
                Rel::Super
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
    pub(crate) fn parse_opt_ty_annot(&mut self) -> Option<TyIdx> {
        if self.eat(TokenKind::Colon) {
            Some(self.parse_alloc_ty())
        } else {
            None
        }
    }

    fn parse_effect_set(&mut self) -> EffectSet {
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
            Some(self.alloc_ty(t))
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
