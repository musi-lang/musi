use std::mem;

use musi_basic::{interner::Interner, source::SourceFile, span::Span};
pub use musi_errors::ParseErrorKind;
pub use parser::Parser;

use musi_ast::{AstArena, Expr, ExprId, Ident, Pat, PatId, Prog, TyExpr, TyExprId};
use musi_errors::DiagnosticBag;
use musi_lex::token::Token;

pub mod expr;
pub mod parser;
pub mod pat;
pub mod stmt;
pub mod ty_expr;

pub struct TestCtx {
    pub interner: Interner,
    pub arena: AstArena,
}

impl TestCtx {
    pub fn new() -> Self {
        Self {
            interner: Interner::new(),
            arena: AstArena::new(),
        }
    }

    pub fn tokenize(&mut self, input: &str) -> Vec<Token> {
        let source = SourceFile::new("test.ms".into(), input.into(), 0);
        let (tokens, _) = musi_lex::tokenize(&source, &mut self.interner);
        tokens
    }

    pub fn parse_expr(&mut self, input: &str) -> ExprId {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens, &mut self.arena, &self.interner);
        parser.parse_expr().expect("parse failed")
    }

    pub fn parse_pat(&mut self, input: &str) -> PatId {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens, &mut self.arena, &self.interner);
        parser.parse_pat().expect("parse failed")
    }

    pub fn parse_typ(&mut self, input: &str) -> TyExprId {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens, &mut self.arena, &self.interner);
        parser.parse_ty_expr().expect("parse failed")
    }

    pub fn expr(&self, id: ExprId) -> &Expr {
        self.arena.exprs.get(id)
    }

    pub fn pat(&self, id: PatId) -> &Pat {
        self.arena.pats.get(id)
    }

    pub fn ty_expr(&self, id: TyExprId) -> &TyExpr {
        self.arena.ty_exprs.get(id)
    }

    pub fn intern(&mut self, s: &str) -> Ident {
        Ident::new(self.interner.intern(s), Span::DUMMY)
    }
}

pub struct ParseResult {
    pub prog: Prog,
    pub arena: AstArena,
    pub diagnostics: DiagnosticBag,
}

#[must_use]
pub fn parse(tokens: &[Token], interner: &Interner) -> ParseResult {
    let mut arena = AstArena::new();
    let mut parser = Parser::new(tokens, &mut arena, interner);
    let prog = parser.parse_prog();
    let diagnostics = mem::take(&mut parser.diagnostics);

    ParseResult {
        prog,
        arena,
        diagnostics,
    }
}
