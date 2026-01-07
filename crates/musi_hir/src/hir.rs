use musi_ast::LitKind;
use musi_core::{Span, Symbol, TokenKind};

use crate::DefId;

pub type HirExprId = u32;
pub type HirPatId = u32;
pub type HirTyId = u32;

#[derive(Debug, Clone)]
pub struct HirExpr {
    pub kind: HirExprKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirExprKind {
    Lit(LitKind),
    Ref(DefId),
    Tuple(Vec<HirExprId>),
    Array(Vec<HirExprId>),
    Record {
        base: Option<HirExprId>,
        fields: Vec<(Symbol, HirExprId)>,
    },
    Binary {
        op: TokenKind,
        lhs: HirExprId,
        rhs: HirExprId,
    },
    Unary {
        op: TokenKind,
        operand: HirExprId,
    },
    Call {
        callee: HirExprId,
        args: Vec<HirExprId>,
    },
    Field {
        base: HirExprId,
        field: Symbol,
    },
    Index {
        base: HirExprId,
        index: HirExprId,
    },
    Block {
        stmts: Vec<HirExprId>,
        expr: Option<HirExprId>,
    },
    If {
        cond: HirExprId,
        then_br: HirExprId,
        else_br: Option<HirExprId>,
    },
    Match {
        scrutinee: HirExprId,
        cases: Vec<HirMatchCase>,
    },
    For {
        pat: HirPatId,
        iter: HirExprId,
        body: HirExprId,
    },
    While {
        cond: HirExprId,
        body: HirExprId,
    },
    Fn {
        def: DefId,
        params: Vec<DefId>,
        body: HirExprId,
    },
    Binding {
        def: DefId,
        mutable: bool,
        init: HirExprId,
    },
    Assign {
        target: HirExprId,
        value: HirExprId,
    },
    Return(Option<HirExprId>),
    Break(Option<HirExprId>),
    Cycle,
    Defer(HirExprId),
    Import(Symbol),
}

#[derive(Debug, Clone)]
pub struct HirMatchCase {
    pub pat: HirPatId,
    pub guard: Option<HirExprId>,
    pub body: HirExprId,
}

#[derive(Debug, Clone)]
pub struct HirPat {
    pub kind: HirPatKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirPatKind {
    Wild,
    Bind(DefId),
    Lit(LitKind),
    Tuple(Vec<HirPatId>),
    Array(Vec<HirPatId>),
    Variant { def: DefId, args: Vec<HirPatId> },
    Or(Vec<HirPatId>),
}

#[derive(Debug, Clone)]
pub struct HirTy {
    pub kind: HirTyKind,
    pub span: Span,
}

#[derive(Debug, Clone)]
pub enum HirTyKind {
    Ref(DefId),
    Tuple(Vec<HirTyId>),
    Array(HirTyId),
    Fn { params: Vec<HirTyId>, ret: HirTyId },
    Ptr(HirTyId),
    Optional(HirTyId),
}

#[derive(Debug, Default)]
pub struct HirModule {
    pub exprs: Vec<HirExpr>,
    pub pats: Vec<HirPat>,
    pub tys: Vec<HirTy>,
    pub top_level: Vec<HirExprId>,
}

impl HirModule {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// # Panics
    /// Panics if the expression count exceeds `u32::MAX`.
    pub fn alloc_expr(&mut self, kind: HirExprKind, span: Span) -> HirExprId {
        let id = u32::try_from(self.exprs.len()).expect("HIR expression overflow");
        self.exprs.push(HirExpr { kind, span });
        id
    }

    /// # Panics
    /// Panics if the pattern count exceeds `u32::MAX`.
    pub fn alloc_pat(&mut self, kind: HirPatKind, span: Span) -> HirPatId {
        let id = u32::try_from(self.pats.len()).expect("HIR pattern overflow");
        self.pats.push(HirPat { kind, span });
        id
    }

    /// # Panics
    /// Panics if the type count exceeds `u32::MAX`.
    pub fn alloc_ty(&mut self, kind: HirTyKind, span: Span) -> HirTyId {
        let id = u32::try_from(self.tys.len()).expect("HIR type overflow");
        self.tys.push(HirTy { kind, span });
        id
    }
}
