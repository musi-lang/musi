#![allow(clippy::let_underscore_must_use)]

use std::ops::ControlFlow;

use music_shared::{Span, Symbol};

use crate::expr::{BinOp, BindKind, Expr, LetFields, UnaryOp};
use crate::pat::Pat;
use crate::ty::Ty;
use crate::visitor::AstVisitor;
use crate::{AstArenas, ExprIdx, PatIdx, TyIdx, visitor};

/// A visitor that counts how many expr nodes it visits.
struct CountingVisitor {
    expr_count: usize,
    ty_count: usize,
    pat_count: usize,
}

impl CountingVisitor {
    fn new() -> Self {
        Self {
            expr_count: 0,
            ty_count: 0,
            pat_count: 0,
        }
    }
}

impl AstVisitor for CountingVisitor {
    type Break = ();

    fn visit_expr(&mut self, idx: ExprIdx, ctx: &AstArenas) -> ControlFlow<()> {
        self.expr_count += 1;
        visitor::walk_expr(self, idx, ctx)
    }

    fn visit_ty(&mut self, idx: TyIdx, ctx: &AstArenas) -> ControlFlow<()> {
        self.ty_count += 1;
        visitor::walk_ty(self, idx, ctx)
    }

    fn visit_pat(&mut self, idx: PatIdx, ctx: &AstArenas) -> ControlFlow<()> {
        self.pat_count += 1;
        visitor::walk_pat(self, idx, ctx)
    }
}

#[test]
fn test_walk_expr_visits_binop_children_in_order() {
    let mut arenas = AstArenas::new();
    let left = arenas.exprs.alloc(Expr::Name {
        name: Symbol(0),
        span: Span::new(0, 1),
    });
    let right = arenas.exprs.alloc(Expr::Name {
        name: Symbol(1),
        span: Span::new(4, 1),
    });
    let root = arenas.exprs.alloc(Expr::BinOp {
        op: BinOp::Add,
        left,
        right,
        span: Span::new(0, 5),
    });

    let mut visitor = CountingVisitor::new();
    let _ = visitor.visit_expr(root, &arenas);
    // root + left + right = 3
    assert_eq!(visitor.expr_count, 3);
}

/// A visitor that breaks after seeing N expressions.
struct BreakAfter {
    limit: usize,
    seen: usize,
}

impl AstVisitor for BreakAfter {
    type Break = &'static str;

    fn visit_expr(&mut self, idx: ExprIdx, ctx: &AstArenas) -> ControlFlow<&'static str> {
        self.seen += 1;
        if self.seen >= self.limit {
            return ControlFlow::Break("limit reached");
        }
        visitor::walk_expr(self, idx, ctx)
    }
}

#[test]
fn test_visitor_short_circuits_on_break() {
    let mut arenas = AstArenas::new();
    let left = arenas.exprs.alloc(Expr::Name {
        name: Symbol(0),
        span: Span::new(0, 1),
    });
    let right = arenas.exprs.alloc(Expr::Name {
        name: Symbol(1),
        span: Span::new(4, 1),
    });
    let root = arenas.exprs.alloc(Expr::BinOp {
        op: BinOp::Add,
        left,
        right,
        span: Span::new(0, 5),
    });

    let mut visitor = BreakAfter { limit: 2, seen: 0 };
    let result = visitor.visit_expr(root, &arenas);
    assert_eq!(result, ControlFlow::Break("limit reached"));
    // should have seen root + left = 2, then broke before right
    assert_eq!(visitor.seen, 2);
}

#[test]
fn test_walk_expr_crosses_into_ty() {
    let mut arenas = AstArenas::new();
    let pat = arenas.pats.alloc(Pat::Wild {
        span: Span::new(4, 1),
    });
    let ty = arenas.tys.alloc(Ty::Named {
        name: Symbol(0),
        args: vec![],
        span: Span::new(6, 3),
    });
    let value = arenas.exprs.alloc(Expr::Name {
        name: Symbol(1),
        span: Span::new(12, 1),
    });
    let root = arenas.exprs.alloc(Expr::Let {
        fields: LetFields {
            kind: BindKind::Immut,
            heap: false,
            pat,
            params: vec![],
            constraints: vec![],
            ty: Some(ty),
            value: Some(value),
            span: Span::new(0, 13),
        },
        body: None,
        span: Span::new(0, 13),
    });

    let mut visitor = CountingVisitor::new();
    let _ = visitor.visit_expr(root, &arenas);
    // exprs: root + value = 2
    assert_eq!(visitor.expr_count, 2);
    // tys: the annotation = 1
    assert_eq!(visitor.ty_count, 1);
    // pats: the wildcard = 1
    assert_eq!(visitor.pat_count, 1);
}

#[test]
fn test_walk_expr_visits_record_def_fields() {
    use crate::expr::RecDefField;
    let mut arenas = AstArenas::new();
    let ty = arenas.tys.alloc(Ty::Named {
        name: Symbol(0),
        args: vec![],
        span: Span::new(10, 3),
    });
    let root = arenas.exprs.alloc(Expr::RecordDef {
        fields: vec![RecDefField {
            name: Symbol(1),
            ty,
            default: None,
            span: Span::new(0, 10),
        }],
        span: Span::new(0, 20),
    });

    let mut visitor = CountingVisitor::new();
    let _ = visitor.visit_expr(root, &arenas);
    // exprs: root = 1
    assert_eq!(visitor.expr_count, 1);
    // tys: the field type = 1
    assert_eq!(visitor.ty_count, 1);
}

#[test]
fn test_walk_pat_visits_or_branches() {
    let mut arenas = AstArenas::new();
    let left = arenas.pats.alloc(Pat::Wild {
        span: Span::new(0, 1),
    });
    let right = arenas.pats.alloc(Pat::Wild {
        span: Span::new(4, 1),
    });
    let root = arenas.pats.alloc(Pat::Or {
        left,
        right,
        span: Span::new(0, 5),
    });

    let mut visitor = CountingVisitor::new();
    let _ = visitor.visit_pat(root, &arenas);
    // root + left + right = 3
    assert_eq!(visitor.pat_count, 3);
}

#[test]
fn test_walk_expr_visits_unary_op_defer_operand() {
    let mut arenas = AstArenas::new();
    let inner = arenas.exprs.alloc(Expr::Name {
        name: Symbol(0),
        span: Span::new(6, 3),
    });
    let root = arenas.exprs.alloc(Expr::UnaryOp {
        op: UnaryOp::Defer,
        operand: inner,
        span: Span::new(0, 9),
    });

    let mut visitor = CountingVisitor::new();
    let _ = visitor.visit_expr(root, &arenas);
    // root + operand = 2
    assert_eq!(visitor.expr_count, 2);
}
