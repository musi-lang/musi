//! Foreign declaration parsing.

use msc_ast::attr::Attr;
use msc_ast::decl::ForeignDecl;
use msc_ast::expr::Expr;
use msc_lex::token::TokenKind;
use msc_shared::Symbol;

use crate::parser::Parser;

impl Parser<'_> {
    /// Parses `'foreign' string_lit ( foreign_item | '(' { [attrs] 'let' foreign_binding ';' } ')' )`.
    pub(crate) fn parse_expr_foreign(&mut self) -> Expr {
        let start = self.start_span();
        let _foreign = self.bump();

        // ABI string literal is optional; defaults to "C".
        let abi = if self.at(TokenKind::StringLit) {
            let tok = self.bump();
            tok.symbol.unwrap_or(Symbol(u32::MAX))
        } else {
            self.interner.intern("C")
        };

        if self.eat(TokenKind::KwImport) {
            // `foreign ["abi"] import "path"` - import a module from the host environment.
            let tok = self.bump();
            let span = tok.span;
            let path = tok.symbol.unwrap_or_else(|| {
                use crate::error::ParseError;
                let _diag = self
                    .diags
                    .report(&ParseError::ExpectedImportPath, span, self.file_id);
                Symbol(u32::MAX)
            });
            return Expr::Import {
                path,
                alias: None,
                span: self.finish_span(start),
            };
        }

        if self.at(TokenKind::LParen) {
            // Block form: foreign "C" ( let ...; let ...; )
            let _lp = self.bump();
            let mut decls = vec![];
            while !self.at(TokenKind::RParen) && !self.at(TokenKind::Eof) {
                let attrs = self.parse_attrs();
                let _let = self.expect(TokenKind::KwLet);
                decls.push(self.parse_foreign_binding(attrs));
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
            let decl = self.parse_foreign_binding(vec![]);
            Expr::Foreign {
                exported: false,
                abi,
                decls: vec![decl],
                span: self.finish_span(start),
            }
        }
    }

    pub(crate) fn parse_expr_foreign_exported(&mut self) -> Expr {
        let mut expr = self.parse_expr_foreign();
        if let Expr::Foreign {
            ref mut exported, ..
        } = expr
        {
            *exported = true;
        }
        expr
    }

    fn parse_foreign_binding(&mut self, attrs: Vec<Attr>) -> ForeignDecl {
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
            // Has type annotation -> foreign function
            let ty = self.parse_alloc_ty();
            ForeignDecl::Fn {
                attrs,
                name,
                ext_name,
                ty,
                span: self.finish_span(start),
            }
        } else {
            // No type annotation -> opaque type
            ForeignDecl::OpaqueType {
                name,
                span: self.finish_span(start),
            }
        }
    }
}
