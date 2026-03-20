//! Class, given, and effect declaration parsing.

use msc_ast::decl::{ClassMember, EffectOp, FnSig};
use msc_ast::expr::{Expr, InstanceBody};
use msc_lex::token::TokenKind;
use msc_shared::Symbol;

use crate::error::ParseError;
use crate::parser::Parser;

impl Parser<'_> {
    pub(crate) fn parse_expr_class(&mut self) -> Expr {
        let start = self.start_span();
        let _class = self.expect(TokenKind::KwClass);
        let name = self.expect_symbol();
        let params = self.parse_optional_bracket_params();
        let constraints = self.parse_opt_where_clause();
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

    pub(crate) fn parse_expr_instance(&mut self) -> Expr {
        let start = self.start_span();
        let _instance = self.expect(TokenKind::KwInstance);
        let params = self.parse_optional_bracket_params();
        let constraints = self.parse_opt_where_clause();
        let target = self.parse_ty_named_ref();
        let body = if self.at(TokenKind::LBrace) {
            let _lb = self.expect(TokenKind::LBrace);
            let members = self.parse_class_body();
            let _rb = self.expect(TokenKind::RBrace);
            InstanceBody::Manual { members }
        } else if self.eat(TokenKind::KwVia) {
            let via_start = self.start_span();
            let delegate = self.parse_ty_named_ref();
            InstanceBody::Via {
                delegate,
                span: self.finish_span(via_start),
            }
        } else if self.eat(TokenKind::KwDerives) {
            let derives_start = self.start_span();
            let mut classes = vec![self.expect_symbol()];
            while self.eat(TokenKind::Comma) {
                classes.push(self.expect_symbol());
            }
            InstanceBody::Derives {
                classes,
                span: self.finish_span(derives_start),
            }
        } else {
            let _lb = self.expect(TokenKind::LBrace);
            InstanceBody::Manual { members: vec![] }
        };
        Expr::Instance {
            exported: false,
            params,
            constraints,
            target,
            body,
            span: self.finish_span(start),
        }
    }

    /// Parses `'effect' ident ['of' ty_param_list] [where-clause] '{' { effect_op ';' } '}'`.
    pub(crate) fn parse_expr_effect(&mut self) -> Expr {
        let start = self.start_span();
        let _effect = self.expect(TokenKind::KwEffect);
        let name = self.expect_symbol();
        let params = self.parse_optional_bracket_params();
        let constraints = self.parse_opt_where_clause();
        let _lb = self.expect(TokenKind::LBrace);
        let ops = self.parse_effect_ops();
        let _rb = self.expect(TokenKind::RBrace);
        Expr::Effect {
            exported: false,
            name,
            params,
            constraints,
            ops,
            span: self.finish_span(start),
        }
    }

    fn parse_effect_ops(&mut self) -> Vec<EffectOp> {
        let mut ops = vec![];
        while !self.at(TokenKind::RBrace) && !self.at(TokenKind::Eof) {
            let op_start = self.start_span();
            let fatal = self.eat(TokenKind::KwFatal);
            let _let = self.expect(TokenKind::KwLet);
            let sig = self.parse_fn_sig();
            ops.push(EffectOp {
                fatal,
                name: sig.name,
                params: sig.params,
                ret: sig.ret,
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
        let params = if self.at(TokenKind::LParen) {
            let _lp = self.bump();
            let ps = self.comma_sep(TokenKind::RParen, Self::parse_param);
            let _rp = self.expect(TokenKind::RParen);
            ps
        } else {
            vec![]
        };
        let _ceq = self.expect(TokenKind::ColonEq);
        let body = self.parse_alloc_expr();
        ClassMember::Law {
            name,
            params,
            body,
            span: self.finish_span(start),
        }
    }

    fn parse_fn_sig(&mut self) -> FnSig {
        let start = self.start_span();
        let name = self.parse_op_or_ident();
        let params = if self.at(TokenKind::LParen) {
            let _lp = self.bump();
            let params = self.comma_sep(TokenKind::RParen, Self::parse_param);
            let _rp = self.expect(TokenKind::RParen);
            params
        } else {
            vec![]
        };
        let ret = self.parse_opt_ty_annot();
        FnSig {
            name,
            params,
            ret,
            span: self.finish_span(start),
        }
    }

    /// Parses `ident | op_ident`.
    /// `op_ident = '(' op_chars ')'` - e.g. `(+)`, `(::)`.
    fn parse_op_or_ident(&mut self) -> Symbol {
        if self.at(TokenKind::LParen) {
            let _lp = self.bump();
            let tok = self.bump();
            let sym = if let Some(s) = tok.symbol {
                s
            } else if let Some(text) = tok.kind.fixed_text() {
                self.interner.intern(text)
            } else {
                let span = tok.span;
                let err = ParseError::ExpectedIdent;
                let _diag = self.diags.report(&err, span, self.file_id);
                Symbol(u32::MAX)
            };
            let _rp = self.expect(TokenKind::RParen);
            sym
        } else {
            self.expect_symbol()
        }
    }
}
