//! Declaration parsing: class, given, effect, attrs, fn params, exports.

#[cfg(test)]
mod tests;

use music_ast::attr::{Attr, AttrValue};
use music_ast::decl::{ClassMember, EffectOp, ExportItem, FnSig};
use music_ast::expr::{BindKind, Expr, LetFields, Param, ParamMode, TyNamed};
use music_ast::ty::Quantifier;
use music_lex::token::TokenKind;
use music_shared::{Span, Symbol};

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    // -- attributes ----------------------------------------------------------

    /// Parses `{ '#[' attr_body ']' }`.
    pub(crate) fn parse_attrs(&mut self) -> Vec<Attr> {
        let mut attrs = vec![];
        while self.at(TokenKind::HashLBracket) {
            let start = self.start_span();
            let _hlb = self.bump();
            let name = self.expect_symbol();
            let value = if self.eat(TokenKind::ColonEq) {
                Some(self.parse_attr_value())
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

    // -- LET / VAR bindings --------------------------------------------------

    /// Parses `'let' ['ref'] pat [ty_annot] ':=' expr ['in' '(' expr ')']`.
    pub(crate) fn parse_let(&mut self) -> Expr {
        let start = self.start_span();
        let _let = self.bump();
        self.parse_binding_body(BindKind::Immut, start)
    }

    /// Parses `'var' ['ref'] pat [ty_annot] ':=' expr`.
    pub(crate) fn parse_var(&mut self) -> Expr {
        let start = self.start_span();
        let _var = self.bump();
        self.parse_binding_body(BindKind::Mut, start)
    }

    fn parse_binding_body(&mut self, kind: BindKind, start: u32) -> Expr {
        let heap = self.eat(TokenKind::KwRef);
        let pat = self.parse_alloc_pat();
        let ty = self.parse_opt_ty_annot();
        let _ceq = self.expect(TokenKind::ColonEq);
        // use no-in variant so `in` is not consumed as a binary operator
        // when this is a let-in scoped binding
        let value = if kind == BindKind::Immut {
            self.parse_alloc_expr_no_in()
        } else {
            self.parse_alloc_expr()
        };

        let fields = LetFields {
            kind,
            heap,
            pat,
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

    // -- annotated declarations (after `#[...]`) --------------------------------

    /// Parses `attrs ast_decl`.
    pub(crate) fn parse_annotated(&mut self) -> Expr {
        let start = self.start_span();
        let attrs = self.parse_attrs();
        match self.peek_kind() {
            TokenKind::KwExport => self.parse_annotated_export(start, attrs),
            TokenKind::KwLet | TokenKind::KwVar => self.parse_annotated_binding(start, attrs),
            TokenKind::KwClass => self.parse_annotated_class(start, attrs),
            TokenKind::KwGiven => self.parse_annotated_given(start, attrs),
            TokenKind::KwEffect => self.parse_annotated_effect(start, attrs),
            _ => self.error_expr(&ParseError::ExpectedDeclAfterAttrs),
        }
    }

    fn parse_annotated_export(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let exported = true;
        let _export = self.bump();

        // export { items } [:= import]
        if self.at(TokenKind::LBrace) {
            return self.parse_export_list(start, attrs);
        }

        match self.peek_kind() {
            TokenKind::KwLet => {
                let inner = self.parse_let();
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
                let inner = self.parse_var();
                if let Expr::Let { fields, .. } = inner {
                    return Expr::Binding {
                        exported,
                        fields,
                        span: self.finish_span(start),
                    };
                }
                inner
            }
            TokenKind::KwEffect => self.parse_effect(),
            _ => self.error_expr(&ParseError::ExpectedAfterExport),
        }
    }

    fn parse_annotated_binding(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = if self.at(TokenKind::KwLet) {
            self.parse_let()
        } else {
            self.parse_var()
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

    fn parse_annotated_class(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = self.parse_class();
        let inner_idx = self.alloc_expr(inner);
        Expr::Annotated {
            attrs,
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_annotated_given(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = self.parse_given();
        let inner_idx = self.alloc_expr(inner);
        Expr::Annotated {
            attrs,
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    fn parse_annotated_effect(&mut self, start: u32, attrs: Vec<Attr>) -> Expr {
        let inner = self.parse_effect();
        let inner_idx = self.alloc_expr(inner);
        Expr::Annotated {
            attrs,
            inner: inner_idx,
            span: self.finish_span(start),
        }
    }

    // -- export list -----------------------------------------------------------

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

    // -- export expression (standalone, no attrs) ------------------------------

    pub(crate) fn parse_export_expr(&mut self) -> Expr {
        let start = self.start_span();
        let _export = self.bump();

        // export { items } [:= import]
        if self.at(TokenKind::LBrace) {
            return self.parse_export_list(start, vec![]);
        }

        match self.peek_kind() {
            TokenKind::KwLet => {
                let inner = self.parse_let();
                if let Expr::Let { fields, .. } = inner {
                    return Expr::Binding {
                        exported: true,
                        fields,
                        span: self.finish_span(start),
                    };
                }
                inner
            }
            TokenKind::KwVar => {
                let inner = self.parse_var();
                if let Expr::Let { fields, .. } = inner {
                    return Expr::Binding {
                        exported: true,
                        fields,
                        span: self.finish_span(start),
                    };
                }
                inner
            }
            TokenKind::KwEffect => self.parse_effect(),
            _ => self.error_expr(&ParseError::ExpectedAfterExport),
        }
    }

    // -- Class ----------------------------------------------------------------

    pub(crate) fn parse_class(&mut self) -> Expr {
        let start = self.start_span();
        let _class = self.expect(TokenKind::KwClass);
        let name = self.expect_symbol();
        let params = if self.at(TokenKind::TyIdent) {
            self.parse_ty_param_list()
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
            name,
            params,
            constraints,
            members,
            span: self.finish_span(start),
        }
    }

    // -- Given ----------------------------------------------------------------

    pub(crate) fn parse_given(&mut self) -> Expr {
        let start = self.start_span();
        let _given = self.expect(TokenKind::KwGiven);
        let target = self.parse_ty_named();
        let constraints = self.parse_opt_where_clause();
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Given {
            target,
            constraints,
            members,
            span: self.finish_span(start),
        }
    }

    // -- Effect ---------------------------------------------------------------

    /// Parses `'effect' ident ['of' ty_param_list] '{' { effect_op ';' } '}'`.
    pub(crate) fn parse_effect(&mut self) -> Expr {
        let start = self.start_span();
        let _effect = self.expect(TokenKind::KwEffect);
        let name = self.expect_symbol();
        let params = if self.eat(TokenKind::KwOf) {
            self.parse_ty_param_list()
        } else {
            vec![]
        };
        let _lb = self.expect(TokenKind::LBrace);
        let ops = self.parse_effect_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Effect {
            name,
            params,
            ops,
            span: self.finish_span(start),
        }
    }

    fn parse_effect_body(&mut self) -> Vec<EffectOp> {
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
            let _semi = self.expect(TokenKind::Semi);
        }
        ops
    }

    fn parse_ty_named(&mut self) -> TyNamed {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::KwOf) {
            let mut list = vec![self.parse_alloc_ty()];
            while self.eat(TokenKind::Comma) {
                list.push(self.parse_alloc_ty());
            }
            list
        } else {
            vec![]
        };
        TyNamed {
            name,
            args,
            span: self.finish_span(start),
        }
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
            let _semi = self.expect(TokenKind::Semi);
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
        let ty = self.parse_opt_ty_annot();
        FnSig {
            name,
            params,
            ty,
            span: self.finish_span(start),
        }
    }

    /// Parses `ident | op_ident`.
    /// `op_ident = '(' op_chars ')'` — e.g. `(+)`, `(::)`.
    fn parse_op_or_ident(&mut self) -> Symbol {
        if self.at(TokenKind::LParen) {
            let _lp = self.bump();
            // Operator token: try symbol first, fall back to fixed_text sentinel
            let tok = self.bump();
            let sym = tok.symbol.unwrap_or(Symbol(u32::MAX));
            let _rp = self.expect(TokenKind::RParen);
            sym
        } else {
            self.expect_symbol()
        }
    }

    // -- Parameters -----------------------------------------------------------

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

    // -- Quantified expressions -----------------------------------------------

    pub(crate) fn parse_quantified_expr(&mut self, kind: Quantifier) -> Expr {
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

    // -- Function literal reinterpretation ------------------------------------

    /// Converts parsed expressions into function parameters.
    /// Each expression must be a Name, optionally with a type annotation following.
    pub(crate) fn reinterpret_as_params(&mut self, exprs: &[Expr]) -> Vec<Param> {
        let mut params = vec![];
        for expr in exprs {
            if let Expr::Name { ident, span, .. } = expr {
                params.push(Param {
                    mode: ParamMode::Plain,
                    name: *ident,
                    ty: None,
                    default: None,
                    span: *span,
                });
            } else {
                let span = if let Expr::Error { span } = expr {
                    *span
                } else {
                    Span::DUMMY
                };
                let _diag = self
                    .diags
                    .report(&ParseError::ExpectedParamName, span, self.file_id);
                params.push(Param {
                    mode: ParamMode::Plain,
                    name: Symbol(u32::MAX),
                    ty: None,
                    default: None,
                    span,
                });
            }
        }
        params
    }
}
