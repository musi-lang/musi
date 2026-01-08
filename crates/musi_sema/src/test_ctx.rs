use musi_ast::{AstArena, ExprId};
use musi_core::{Interner, SourceFile, Span, Symbol, Token};

use crate::inferer::Inferer;
use crate::table::UnificationTable;
use crate::ty::{TyArena, TyKind};
use crate::ty_env::TyEnv;

pub struct TestCtx {
    pub interner: Interner,
    pub ast_arena: AstArena,
    pub ty_arena: TyArena,
    pub env: TyEnv,
    pub table: UnificationTable,
}

impl TestCtx {
    pub fn new() -> Self {
        Self {
            interner: Interner::new(),
            ast_arena: AstArena::new(),
            ty_arena: TyArena::new(),
            env: TyEnv::new(),
            table: UnificationTable::new(),
        }
    }

    pub fn tokenize(&mut self, input: &str) -> Vec<Token> {
        let source = SourceFile::new("test.ms".into(), input.into(), 0);
        let (tokens, _) = musi_lex::tokenize(&source, &mut self.interner, true);
        tokens
    }

    pub fn parse_expr(&mut self, input: &str) -> ExprId {
        let tokens = self.tokenize(input);
        let mut parser = musi_parse::Parser::new(&tokens, &mut self.ast_arena, &self.interner);
        parser.parse_expr().expect("parse failed")
    }

    pub fn infer(&mut self, expr_id: ExprId) -> TyKind {
        let mut inferer = Inferer::new(
            &self.ast_arena,
            &self.interner,
            &mut self.ty_arena,
            &mut self.env,
            &mut self.table,
        );
        let ty_id = inferer.infer_expr(expr_id).expect("infer failed");
        self.ty_arena.get(ty_id).kind.clone()
    }

    pub fn intern(&mut self, s: &str) -> Symbol {
        Symbol::new(self.interner.intern(s), Span::DUMMY)
    }
}
