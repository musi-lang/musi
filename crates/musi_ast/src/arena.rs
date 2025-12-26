use musi_basic::{
    arena::{Arena, NodeId},
    span::Span,
};

use crate::{
    Cond, CondId, CondKind, Expr, ExprId, ExprKind, Pat, PatId, PatKind, Stmt, StmtId, StmtKind,
    TyExpr, TyExprId, TyExprKind,
};

#[derive(Debug, Default)]
pub struct AstArena {
    pub ty_exprs: Arena<TyExpr>,
    pub pats: Arena<Pat>,
    pub exprs: Arena<Expr>,
    pub conds: Arena<Cond>,
    pub stmts: Arena<Stmt>,
}

impl AstArena {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    pub fn alloc_ty_expr(&mut self, kind: TyExprKind, span: Span) -> TyExprId {
        let id = NodeId::new(u32::try_from(self.ty_exprs.len()).expect("arena overflow"));
        let _ = self.ty_exprs.alloc(TyExpr::new(id, kind, span));
        id
    }

    pub fn alloc_pat(&mut self, kind: PatKind, span: Span) -> PatId {
        let id = NodeId::new(u32::try_from(self.pats.len()).expect("arena overflow"));
        let _ = self.pats.alloc(Pat::new(id, kind, span));
        id
    }

    pub fn alloc_expr(&mut self, kind: ExprKind, span: Span) -> ExprId {
        let id = NodeId::new(u32::try_from(self.exprs.len()).expect("arena overflow"));
        let _ = self.exprs.alloc(Expr::new(id, kind, span));
        id
    }

    pub fn alloc_cond(&mut self, kind: CondKind) -> CondId {
        let id = NodeId::new(u32::try_from(self.conds.len()).expect("arena overflow"));
        let _ = self.conds.alloc(Cond::new(id, kind));
        id
    }

    pub fn alloc_stmt(&mut self, kind: StmtKind, span: Span) -> StmtId {
        let id = NodeId::new(u32::try_from(self.stmts.len()).expect("arena overflow"));
        let _ = self.stmts.alloc(Stmt::new(id, kind, span));
        id
    }
}
