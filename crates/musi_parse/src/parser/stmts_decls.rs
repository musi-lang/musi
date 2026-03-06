//! Statement and declaration parsing: control flow, imports, fn/record/choice/bind/class.

use musi_lex::token::TokenKind;
use musi_shared::Idx;

use crate::ast::{
    Attr, AttrArg, ChoiceVariant, Cond, ElifBranch, ExportItem, Expr, ImportClause, ImportItem,
    MatchArm, Modifier, Param, RecField, VariantPayload,
};

use super::{InfixKind, Parser};

impl<'a> Parser<'a> {
    pub(super) fn parse_if(&mut self) -> Expr {
        let start = self.start_span();
        let _if = self.expect(TokenKind::If);

        let cond = Box::new(self.parse_cond());
        let _then = self.expect(TokenKind::Then);
        let then_body = self.parse_and_alloc_expr();

        let mut elif_chains = Vec::new();
        while self.at(TokenKind::Elif) {
            let elif_start = self.start_span();
            let _elif = self.advance();
            let elif_cond = Box::new(self.parse_cond());
            let elif_guard = self.parse_opt_guard();
            let _then = self.expect(TokenKind::Then);
            let elif_body = self.parse_and_alloc_expr();
            elif_chains.push(ElifBranch {
                cond: elif_cond,
                guard: elif_guard,
                body: elif_body,
                span: self.finish_span(elif_start),
            });
        }

        let else_body = self.parse_option(TokenKind::Else, Parser::parse_and_alloc_expr);

        Expr::If {
            cond,
            then_body,
            elif_chains,
            else_body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_cond(&mut self) -> Cond {
        if self.at(TokenKind::Case) {
            let start = self.start_span();
            let _case = self.advance();
            let pat = self.parse_pat();
            let _bind = self.expect(TokenKind::ColonEq);
            let init = self.parse_and_alloc_expr();
            Cond::Case {
                pat,
                init,
                span: self.finish_span(start),
            }
        } else {
            Cond::Expr(self.parse_and_alloc_expr())
        }
    }

    fn parse_match_arm(&mut self) -> MatchArm {
        let arm_start = self.start_span();
        let attrs = self.parse_opt_attrs();
        let pat = self.parse_pat();
        let guard = self.parse_opt_guard();
        let _arrow = self.expect(TokenKind::EqGt);
        let body = self.parse_alloc_arm_body();
        MatchArm {
            attrs,
            pat,
            guard,
            body,
            span: self.finish_span(arm_start),
        }
    }

    pub(super) fn parse_match(&mut self) -> Expr {
        let start = self.start_span();
        let _match = self.expect(TokenKind::Match);
        let scrutinee = self.parse_and_alloc_expr();
        let _with = self.expect(TokenKind::With);
        let _lp = self.expect(TokenKind::LParen);

        let mut arms = vec![self.parse_match_arm()];
        while self.eat(TokenKind::Pipe) {
            arms.push(self.parse_match_arm());
        }

        let _rp = self.expect(TokenKind::RParen);
        Expr::Match {
            scrutinee,
            arms,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_alloc_arm_body(&mut self) -> Idx<Expr> {
        let expr = self.parse_arm_body();
        self.alloc_expr(expr)
    }

    /// Like `parse_pratt(0)` but stops before a top-level `|` (Pipe),
    /// which serves as the arm separator in match expressions.
    fn parse_arm_body(&mut self) -> Expr {
        let start = self.start_span();
        let mut lhs = self.parse_unary_expr();
        lhs = self.parse_postfix_chain(lhs, start);
        loop {
            if self.at(TokenKind::Pipe) {
                break;
            }
            let Some((_l_bp, r_bp, op_kind)) = self.infix_info() else {
                break;
            };
            let _op_tok = self.advance();
            let rhs_expr = self.parse_pratt(r_bp);
            let lhs_idx = self.alloc_expr(lhs);
            let rhs_idx = self.alloc_expr(rhs_expr);
            let span = self.finish_span(start);
            lhs = match op_kind {
                InfixKind::Binary(op) => Expr::Binary {
                    op,
                    lhs: lhs_idx,
                    rhs: rhs_idx,
                    span,
                },
                InfixKind::Assign => Expr::Assign {
                    target: lhs_idx,
                    value: rhs_idx,
                    span,
                },
            };
        }
        lhs
    }

    pub(super) fn parse_return(&mut self) -> Expr {
        let start = self.start_span();
        let _ret = self.expect(TokenKind::Return);
        let value = self.parse_opt_expr();
        Expr::Return {
            value,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_break(&mut self) -> Expr {
        let start = self.start_span();
        let _brk = self.expect(TokenKind::Break);
        let value = self.parse_opt_expr();
        Expr::Break {
            label: None,
            value,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_cycle(&mut self) -> Expr {
        let start = self.start_span();
        let _cyc = self.expect(TokenKind::Cycle);
        let label = self.optional_ident();
        let guard = self.parse_opt_guard();
        Expr::Cycle {
            label,
            guard,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_defer(&mut self) -> Expr {
        let start = self.start_span();
        let _def = self.expect(TokenKind::Defer);
        let body = self.parse_and_alloc_expr();
        Expr::Defer {
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_import(&mut self) -> Expr {
        let start = self.start_span();
        let _imp = self.expect(TokenKind::Import);

        let items = if self.eat(TokenKind::Star) {
            if self.eat(TokenKind::As) {
                let name = self.expect_symbol();
                ImportClause::GlobAs(name)
            } else {
                ImportClause::Glob
            }
        } else {
            let list = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
                let item_start = p.start_span();
                let name = p.expect_symbol();
                let alias = p.parse_option(TokenKind::As, Parser::expect_symbol);
                ImportItem {
                    name,
                    alias,
                    span: p.finish_span(item_start),
                }
            });
            ImportClause::Items(list)
        };

        let _from = self.expect(TokenKind::From);
        let path = self.expect_symbol();
        Expr::Import {
            items,
            path,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_export(&mut self) -> Expr {
        let start = self.start_span();
        let _export = self.expect(TokenKind::Export);
        let items = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, |p| {
            let item_start = p.start_span();
            let name = p.expect_symbol();
            let alias = p.parse_option(TokenKind::As, Parser::expect_symbol);
            ExportItem {
                name,
                alias,
                span: p.finish_span(item_start),
            }
        });
        let _from = self.expect(TokenKind::From);
        let path = self.expect_symbol();
        Expr::Export {
            items,
            path,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_using(&mut self) -> Expr {
        let start = self.start_span();
        let _using = self.expect(TokenKind::Using);
        let name = self.expect_symbol();
        let _assign = self.expect(TokenKind::ColonEq);
        let init = self.parse_and_alloc_expr();
        let body = self.parse_alloc_block();
        Expr::Using {
            name,
            init,
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_expr_after_attrs(&mut self) -> Expr {
        let attrs = self.parse_opt_attrs();
        let modifiers = self.parse_modifiers();

        match self.peek_kind() {
            TokenKind::Fn => self.parse_fn_expr(attrs, modifiers),
            TokenKind::Record => self.parse_record(attrs, modifiers),
            TokenKind::Choice => self.parse_choice(attrs, modifiers),
            TokenKind::Const | TokenKind::Var => self.parse_bind(attrs, modifiers),
            TokenKind::While => self.parse_while(),
            TokenKind::Loop => self.parse_loop(),
            TokenKind::For => self.parse_for(),
            TokenKind::Match => self.parse_match(),
            TokenKind::Label => self.parse_label(),
            TokenKind::Class => self.parse_class_def(),
            TokenKind::Given => self.parse_given_def(),
            _ => self.error_expr("expected declaration or loop after attributes/modifiers"),
        }
    }

    pub(super) fn parse_opt_attrs(&mut self) -> Vec<Attr> {
        if self.at(TokenKind::Hash) && self.peek2() == TokenKind::LBracket {
            let _hash = self.expect(TokenKind::Hash);
            self.parse_delimited(TokenKind::LBracket, TokenKind::RBracket, Parser::parse_attr)
        } else {
            Vec::new()
        }
    }

    fn parse_attr(&mut self) -> Attr {
        let start = self.start_span();
        let name = self.expect_symbol();
        let args = if self.eat(TokenKind::LParen) {
            let list = self.parse_separated_list(TokenKind::RParen, Parser::parse_attr_arg);
            let _rp = self.expect(TokenKind::RParen);
            list
        } else {
            Vec::new()
        };
        Attr {
            name,
            args,
            span: self.finish_span(start),
        }
    }

    fn parse_attr_arg(&mut self) -> AttrArg {
        let start = self.start_span();
        // Named: ident [":=" lit]
        if self.at(TokenKind::Ident) {
            let name = self.expect_symbol();
            let value = if self.eat(TokenKind::ColonEq) {
                Some(self.parse_lit_value())
            } else {
                None
            };
            return AttrArg::Named {
                name,
                value,
                span: self.finish_span(start),
            };
        }
        // Literal argument
        let lit = self.parse_lit_value();
        AttrArg::Value {
            value: lit,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_modifiers(&mut self) -> Vec<Modifier> {
        let mut mods = Vec::new();
        loop {
            if self.eat(TokenKind::Export) {
                mods.push(Modifier::Export);
            } else if self.eat(TokenKind::Opaque) {
                mods.push(Modifier::Opaque);
            } else if self.eat(TokenKind::Extrin) {
                let abi = if self.at(TokenKind::StringLit) {
                    Some(self.expect_symbol())
                } else {
                    None
                };
                mods.push(Modifier::Extrin(abi));
            } else {
                break;
            }
        }
        mods
    }

    pub(super) fn parse_fn_expr(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _fn = self.expect(TokenKind::Fn);

        // LL(1): if next is Ident or Underscore → named FnDef, else Lambda
        if self.at(TokenKind::Ident) || self.at(TokenKind::Underscore) {
            // Named function definition
            let name = self.expect_symbol();
            let ty_params = self.parse_opt_ty_params();
            let params =
                self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_param);
            let ret_ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
            let where_clause = self.parse_opt_where_clause();
            let body = if self.eat(TokenKind::EqGt) {
                Some(self.parse_and_alloc_expr())
            } else {
                None
            };
            Expr::FnDef {
                attrs,
                modifiers,
                name,
                ty_params,
                params,
                ret_ty,
                where_clause,
                body,
                span: self.finish_span(start),
            }
        } else {
            // Lambda: fn [ty_params] (params) [: ret_ty] [where ...] => expr
            let ty_params = self.parse_opt_ty_params();
            let params =
                self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_param);
            let ret_ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
            let where_clause = self.parse_opt_where_clause();
            let _arrow = self.expect(TokenKind::EqGt);
            let body = self.parse_and_alloc_expr();
            Expr::Lambda {
                attrs,
                ty_params,
                params,
                ret_ty,
                where_clause,
                body,
                span: self.finish_span(start),
            }
        }
    }

    pub(super) fn parse_param(&mut self) -> Param {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        Param {
            attrs,
            mutable,
            name,
            ty,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_record(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _rec = self.expect(TokenKind::Record);
        let name = self.optional_ident();
        let ty_params = self.parse_opt_ty_params();
        let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, Parser::parse_rec_field);
        Expr::Record {
            attrs,
            modifiers,
            name,
            ty_params,
            fields,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_rec_field(&mut self) -> RecField {
        let (start, attrs, mutable, name) = self.parse_field_header();
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        RecField {
            attrs,
            mutable,
            name,
            ty,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_choice(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let _choice = self.expect(TokenKind::Choice);
        let name = self.optional_ident();
        let ty_params = self.parse_opt_ty_params();
        let _lb = self.expect(TokenKind::LBrace);
        let variants = self.parse_pipe_separated(TokenKind::RBrace, Parser::parse_choice_variant);
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Choice {
            attrs,
            modifiers,
            name,
            ty_params,
            variants,
            span: self.finish_span(start),
        }
    }

    fn parse_choice_variant(&mut self) -> ChoiceVariant {
        let start = self.start_span();
        let attrs = self.parse_opt_attrs();
        let name = self.expect_symbol();
        let payload = match self.peek_kind() {
            TokenKind::LParen => {
                let tys =
                    self.parse_delimited(TokenKind::LParen, TokenKind::RParen, Parser::parse_ty);
                Some(VariantPayload::Positional(tys))
            }
            TokenKind::LBrace => {
                let fields = self.parse_delimited(TokenKind::LBrace, TokenKind::RBrace, Parser::parse_rec_field);
                Some(VariantPayload::Named(fields))
            }
            TokenKind::ColonEq => {
                let _ceq = self.advance();
                let lit = self.parse_lit_value();
                Some(VariantPayload::Discriminant(lit))
            }
            _ => None,
        };
        ChoiceVariant {
            attrs,
            name,
            payload,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_bind(&mut self, attrs: Vec<Attr>, modifiers: Vec<Modifier>) -> Expr {
        let start = self.start_span();
        let kind = self.parse_bind_kind();
        let pat = self.parse_pat();
        let ty = self.parse_option(TokenKind::Colon, Parser::parse_ty);
        let init = self.parse_option(TokenKind::ColonEq, Parser::parse_and_alloc_expr);
        Expr::Bind {
            attrs,
            modifiers,
            kind,
            pat,
            ty,
            init,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_while(&mut self) -> Expr {
        let start = self.start_span();
        let _while = self.expect(TokenKind::While);
        let cond = Box::new(self.parse_cond());
        let guard = self.parse_opt_guard();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        Expr::While {
            cond,
            guard,
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_loop(&mut self) -> Expr {
        let start = self.start_span();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        let post_cond = self.parse_option(TokenKind::While, |p| Box::new(p.parse_cond()));
        Expr::Loop {
            body,
            post_cond,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_for(&mut self) -> Expr {
        let start = self.start_span();
        let _for = self.expect(TokenKind::For);
        let pat = self.parse_pat();
        let _in = self.expect(TokenKind::In);
        let iter = self.parse_and_alloc_expr();
        let guard = self.parse_opt_guard();
        let _loop = self.expect(TokenKind::Loop);
        let body = self.parse_alloc_block();
        Expr::For {
            pat,
            iter,
            guard,
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_label(&mut self) -> Expr {
        let start = self.start_span();
        let _label = self.expect(TokenKind::Label);
        let name = self.expect_symbol();
        let body = self.parse_alloc_block();
        Expr::Label {
            name,
            body,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_class_def(&mut self) -> Expr {
        let start = self.start_span();
        let _class = self.expect(TokenKind::Class);
        let name = self.expect_symbol();
        let ty_params = self.parse_opt_ty_params();
        let supers = if self.eat(TokenKind::Satisfies) {
            let mut list = vec![self.parse_ty_named()];
            while self.eat(TokenKind::Comma) {
                list.push(self.parse_ty_named());
            }
            list
        } else {
            Vec::new()
        };
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::ClassDef {
            name,
            ty_params,
            supers,
            members,
            span: self.finish_span(start),
        }
    }

    pub(super) fn parse_given_def(&mut self) -> Expr {
        let start = self.start_span();
        let _given = self.expect(TokenKind::Given);
        let class_app = self.parse_ty_named();
        let constraints = self.parse_opt_where_clause();
        let _lb = self.expect(TokenKind::LBrace);
        let members = self.parse_class_body();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::GivenDef {
            class_app,
            constraints,
            members,
            span: self.finish_span(start),
        }
    }
}
