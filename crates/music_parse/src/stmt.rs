//! Declaration parsing: class, given, effect, foreign, attrs, fn params, exports.

#[cfg(test)]
mod tests;

use music_ast::attr::{Attr, AttrField, AttrValue};
use music_ast::decl::{ClassMember, EffectOp, ExportItem, FnSig, ForeignDecl};
use music_ast::expr::{BindKind, Expr, LetFields, Param, ParamMode};
use music_ast::ty::{Quantifier, TyParam};
use music_lex::token::TokenKind;
use music_shared::{Span, Symbol};

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    /// Parses `{ '#[' attr_body ']' }`.
    pub(crate) fn parse_attrs(&mut self) -> Vec<Attr> {
        let mut attrs = vec![];
        while self.at(TokenKind::HashLBracket) {
            let start = self.start_span();
            let _hlb = self.bump();
            let name = self.expect_symbol();
            let value = if self.eat(TokenKind::ColonEq) {
                Some(self.parse_attr_value())
            } else if self.at(TokenKind::LParen) {
                Some(self.parse_attr_named_params())
            } else {
                None
            };
            let _rb = self.expect(TokenKind::RBracket);
            attrs.push(Attr {
                name,
                value,
                span: self.finish_span(start),
            });
        }
        attrs
    }

    fn parse_attr_value(&mut self) -> AttrValue {
        let start = self.start_span();
        if self.at(TokenKind::LParen) {
            let _lp = self.bump();
            let lits = self.comma_sep(TokenKind::RParen, Self::parse_lit_value);
            let _rp = self.expect(TokenKind::RParen);
            return AttrValue::Tuple {
                lits,
                span: self.finish_span(start),
            };
        }
        let lit = self.parse_lit_value();
        AttrValue::Lit {
            lit,
            span: self.finish_span(start),
        }
    }

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

    /// Parses `attrs ast_decl`.
    pub(crate) fn parse_expr_annotated_chain(&mut self) -> Expr {
        let start = self.start_span();
        let attrs = self.parse_attrs();
        match self.peek_kind() {
            TokenKind::KwExport => self.parse_expr_annotated_export(start, attrs),
            TokenKind::KwLet | TokenKind::KwVar => self.parse_expr_annotated_binding(start, attrs),
            TokenKind::KwClass => self.parse_expr_annotated_class(start, attrs),
            TokenKind::KwGiven => self.parse_expr_annotated_given(start, attrs),
            TokenKind::KwEffect => self.parse_expr_annotated_effect(start, attrs),
            TokenKind::KwForeign => self.parse_expr_annotated_foreign(start, attrs),
            _ => self.error_expr(&ParseError::ExpectedDeclAfterAttrs),
        }
    }

    fn parse_expr_annotated_export(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let exported = true;
        let _export = self.bump();

        // export { items } [:= import]
        if self.at(TokenKind::LBrace) {
            return self.parse_export_list(start, attrs);
        }

        match self.peek_kind() {
            TokenKind::KwLet => {
                let inner = self.parse_expr_let();
                if let Expr::Let { fields, .. } = inner {
                    return Expr::Binding {
                        exported,
                        fields,
                        span: self.finish_span(start),
                    };
                }
                inner
            }
            TokenKind::KwVar => {
                let inner = self.parse_expr_binding_mut();
                if let Expr::Let { fields, .. } = inner {
                    return Expr::Binding {
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
            TokenKind::KwGiven => {
                let mut inner = self.parse_expr_given();
                if let Expr::Given {
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
        let inner = if self.at(TokenKind::KwLet) {
            self.parse_expr_let()
        } else {
            self.parse_expr_binding_mut()
        };
        if let Expr::Let { fields, .. } = inner {
            let binding = Expr::Binding {
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
        let inner = self.parse_expr_given();
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

    fn parse_export_list(&mut self, start: u32, _attrs: Vec<Attr>) -> Expr {
        let _lb = self.bump();
        let items = self.comma_sep(TokenKind::RBrace, Self::parse_export_item);
        let _rb = self.expect(TokenKind::RBrace);
        let source = if self.eat(TokenKind::ColonEq) {
            // expect import "path"
            let _import = self.expect(TokenKind::KwImport);
            let tok = self.bump();
            tok.symbol
        } else {
            None
        };
        Expr::Export {
            items,
            source,
            span: self.finish_span(start),
        }
    }

    fn parse_export_item(&mut self) -> ExportItem {
        let start = self.start_span();
        let name = self.expect_symbol();
        let alias = if self.eat(TokenKind::KwAs) {
            Some(self.expect_symbol())
        } else {
            None
        };
        ExportItem {
            name,
            alias,
            span: self.finish_span(start),
        }
    }

    pub(crate) fn parse_expr_export(&mut self) -> Expr {
        let start = self.start_span();
        let _export = self.bump();

        // export { items } [:= import]
        if self.at(TokenKind::LBrace) {
            return self.parse_export_list(start, vec![]);
        }

        self.parse_expr_export_rest(start)
    }

    fn parse_expr_export_rest(&mut self, start: u32) -> Expr {
        match self.peek_kind() {
            TokenKind::KwLet => self.parse_export_binding(start, true),
            TokenKind::KwVar => self.parse_export_binding(start, false),
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
            TokenKind::KwGiven => {
                let mut inner = self.parse_expr_given();
                if let Expr::Given {
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

    fn parse_export_binding(&mut self, start: u32, immut: bool) -> Expr {
        let inner = if immut {
            self.parse_expr_let()
        } else {
            self.parse_expr_binding_mut()
        };
        if let Expr::Let { fields, .. } = inner {
            Expr::Binding {
                exported: true,
                fields,
                span: self.finish_span(start),
            }
        } else {
            inner
        }
    }

    pub(crate) fn parse_expr_class(&mut self) -> Expr {
        let start = self.start_span();
        let _class = self.expect(TokenKind::KwClass);
        let name = self.expect_symbol();
        let params = if self.eat(TokenKind::KwOver) {
            self.parse_ty_param_list_maybe_parens()
        } else {
            vec![]
        };
        let constraints = if self.at(TokenKind::KwWhere) {
            self.parse_opt_where_clause()
        } else {
            vec![]
        };
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Class {
            exported: false,
            name,
            params,
            constraints,
            members,
            span: self.finish_span(start),
        }
    }

    /// Parses a type parameter list that may optionally be wrapped in parentheses.
    /// Used after `over` in class/given declarations: `over T` or `over (A, B)`.
    fn parse_ty_param_list_maybe_parens(&mut self) -> Vec<TyParam> {
        if self.eat(TokenKind::LParen) {
            let params = self.parse_ty_param_list();
            let _rp = self.expect(TokenKind::RParen);
            params
        } else {
            self.parse_ty_param_list()
        }
    }

    pub(crate) fn parse_expr_given(&mut self) -> Expr {
        let start = self.start_span();
        let _given = self.expect(TokenKind::KwGiven);
        let target = self.parse_ty_named_ref();
        let params = if self.eat(TokenKind::KwOver) {
            self.parse_ty_param_list_maybe_parens()
        } else {
            vec![]
        };
        let constraints = self.parse_opt_where_clause();
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Given {
            exported: false,
            target,
            params,
            constraints,
            members,
            span: self.finish_span(start),
        }
    }

    /// Parses `'effect' ident ['of' ty_param_list] '{' { effect_op ';' } '}'`.
    pub(crate) fn parse_expr_effect(&mut self) -> Expr {
        let start = self.start_span();
        let _effect = self.expect(TokenKind::KwEffect);
        let name = self.expect_symbol();
        let params = if self.eat(TokenKind::KwOf) {
            self.parse_ty_param_list()
        } else {
            vec![]
        };
        let _lb = self.expect(TokenKind::LBrace);
        let ops = self.parse_effect_op();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Effect {
            exported: false,
            name,
            params,
            ops,
            span: self.finish_span(start),
        }
    }

    fn parse_effect_op(&mut self) -> Vec<EffectOp> {
        let mut ops = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let op_start = self.start_span();
            let name = self.expect_symbol();
            let _colon = self.expect(TokenKind::Colon);
            let ty = self.parse_alloc_ty();
            ops.push(EffectOp {
                name,
                ty,
                span: self.finish_span(op_start),
            });
            if self.at(TokenKind::RBrace) {
                let _ = self.eat(TokenKind::Semi);
            } else {
                let _semi = self.expect(TokenKind::Semi);
            }
        }
        ops
    }

    fn parse_class_body(&mut self) -> Vec<ClassMember> {
        let mut members = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let member = if self.at(TokenKind::KwLaw) {
                self.parse_law_member()
            } else {
                self.parse_fn_member()
            };
            members.push(member);
            if self.at(TokenKind::RBrace) {
                let _ = self.eat(TokenKind::Semi);
            } else {
                let _semi = self.expect(TokenKind::Semi);
            }
        }
        members
    }

    fn parse_fn_member(&mut self) -> ClassMember {
        let start = self.start_span();
        let _let = self.expect(TokenKind::KwLet);
        let sig = self.parse_fn_sig();
        let default = if self.eat(TokenKind::ColonEq) {
            Some(self.parse_alloc_expr())
        } else {
            None
        };
        ClassMember::Fn {
            sig,
            default,
            span: self.finish_span(start),
        }
    }

    fn parse_law_member(&mut self) -> ClassMember {
        let start = self.start_span();
        let _law = self.bump();
        let name = self.expect_symbol();
        let _ceq = self.expect(TokenKind::ColonEq);
        let body = self.parse_alloc_expr();
        ClassMember::Law {
            name,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_fn_sig(&mut self) -> FnSig {
        let start = self.start_span();
        let name = self.parse_op_or_ident();
        let _lp = self.expect(TokenKind::LParen);
        let params = self.comma_sep(TokenKind::RParen, Self::parse_param);
        let _rp = self.expect(TokenKind::RParen);
        let ret = self.parse_opt_ty_annot();
        FnSig {
            name,
            params,
            ret,
            span: self.finish_span(start),
        }
    }

    /// Parses `ident | op_ident`.
    /// `op_ident = '(' op_chars ')'` — e.g. `(+)`, `(::)`.
    fn parse_op_or_ident(&mut self) -> Symbol {
        if self.at(TokenKind::LParen) {
            let _lp = self.bump();
            // try symbol first, fall back to fixed_text sentinel
            let tok = self.bump();
            let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
            let _rp = self.expect(TokenKind::RParen);
            sym
        } else {
            self.expect_symbol()
        }
    }

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

    fn parse_attr_named_params(&mut self) -> AttrValue {
        let start = self.start_span();
        let _lp = self.bump();
        let fields = self.comma_sep(TokenKind::RParen, Self::parse_attr_named_field);
        let _rp = self.expect(TokenKind::RParen);
        AttrValue::Named {
            fields,
            span: self.finish_span(start),
        }
    }

    fn parse_attr_named_field(&mut self) -> AttrField {
        let start = self.start_span();
        let name = self.expect_symbol();
        let _ceq = self.expect(TokenKind::ColonEq);
        let value = self.parse_lit_value();
        AttrField {
            name,
            value,
            span: self.finish_span(start),
        }
    }

    /// Parses `'foreign' string_lit ( foreign_item | '(' { 'let' foreign_binding ';' } ')' )`.
    pub(crate) fn parse_expr_foreign(&mut self) -> Expr {
        let start = self.start_span();
        let _foreign = self.bump();

        // Expect ABI string literal
        let abi = if self.at(TokenKind::StringLit) {
            let tok = self.bump();
            tok.symbol.unwrap_or(Symbol(u32::MAX))
        } else {
            let _span = self.expect(TokenKind::StringLit);
            Symbol(u32::MAX)
        };

        if self.at(TokenKind::LParen) {
            // Block form: foreign "C" ( let ...; let ...; )
            let _lp = self.bump();
            let mut decls = vec![];
            while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                let _let = self.expect(TokenKind::KwLet);
                decls.push(self.parse_foreign_binding());
                let _semi = self.expect(TokenKind::Semi);
            }
            let _rp = self.expect(TokenKind::RParen);
            Expr::Foreign {
                exported: false,
                abi,
                decls,
                span: self.finish_span(start),
            }
        } else {
            // Single item: foreign "C" let ...
            let _let = self.expect(TokenKind::KwLet);
            let decl = self.parse_foreign_binding();
            Expr::Foreign {
                exported: false,
                abi,
                decls: vec![decl],
                span: self.finish_span(start),
            }
        }
    }

    fn parse_expr_foreign_exported(&mut self) -> Expr {
        let mut expr = self.parse_expr_foreign();
        if let Expr::Foreign {
            ref mut exported, ..
        } = expr
        {
            *exported = true;
        }
        expr
    }

    fn parse_foreign_binding(&mut self) -> ForeignDecl {
        let start = self.start_span();
        let name = self.expect_symbol();
        let ext_name = if self.eat(TokenKind::KwAs) {
            if self.at(TokenKind::StringLit) {
                let tok = self.bump();
                tok.symbol
            } else {
                let _span = self.expect(TokenKind::StringLit);
                None
            }
        } else {
            None
        };

        if self.eat(TokenKind::Colon) {
            // Has type annotation → foreign function
            let ty = self.parse_alloc_ty();
            ForeignDecl::Fn {
                name,
                ext_name,
                ty,
                span: self.finish_span(start),
            }
        } else {
            // No type annotation → opaque type
            ForeignDecl::OpaqueType {
                name,
                span: self.finish_span(start),
            }
        }
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
