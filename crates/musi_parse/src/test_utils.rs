use crate::Parser;
use musi_ast::{AstArena, Expr, ExprId, Pat, PatId, TyExpr, TyExprId};
use musi_basic::{interner::Interner, source::SourceFile};
use musi_lex::{Tokens, lexer::tokenize};

pub struct TestContext {
    pub interner: Interner,
    pub arena: AstArena,
}

impl TestContext {
    pub fn new() -> Self {
        Self {
            interner: Interner::new(),
            arena: AstArena::new(),
        }
    }

    pub fn tokenize(&mut self, input: &str) -> Tokens {
        let source = SourceFile::new("test.ms".into(), input.into(), 0);
        let (tokens, _) = tokenize(&source, &mut self.interner);
        tokens
    }

    pub fn parse_expr(&mut self, input: &str) -> ExprId {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens, &mut self.arena);
        parser.parse_expr().expect("parse failed")
    }

    pub fn parse_pat(&mut self, input: &str) -> PatId {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens, &mut self.arena);
        parser.parse_pat().expect("parse failed")
    }

    pub fn parse_typ(&mut self, input: &str) -> TyExprId {
        let tokens = self.tokenize(input);
        let mut parser = Parser::new(&tokens, &mut self.arena);
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

    pub fn intern(&mut self, s: &str) -> u32 {
        self.interner.intern(s)
    }
}
