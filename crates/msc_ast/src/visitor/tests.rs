#![allow(clippy::let_underscore_must_use)]

use std::ops::ControlFlow;

use msc_shared::{Span, Symbol};

use crate::expr::{BinOp, BindKind, Expr, LetFields, UnaryOp};
use crate::pat::Pat;
use crate::visitor::AstVisitor;
use crate::{AstArenas, ExprIdx, NameRef, PatIdx, visitor};

/// A visitor that counts how many expr nodes it visits.
struct CountingVisitor {
    expr_count: usize,
    pat_count: usize,
}

impl CountingVisitor {
    fn new() -> Self {
        Self {
            expr_count: 0,
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

    fn visit_pat(&mut self, idx: PatIdx, ctx: &AstArenas) -> ControlFlow<()> {
        self.pat_count += 1;
        visitor::walk_pat(self, idx, ctx)
    }
}

#[test]
fn test_walk_expr_visits_binop_children_in_order() {
    let mut arenas = AstArenas::new();
    let left_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(0),
        span: Span::new(0, 1),
    });
    let left = arenas.exprs.alloc(Expr::Name {
        name_ref: left_ref,
        span: Span::new(0, 1),
    });
    let right_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(1),
        span: Span::new(4, 1),
    });
    let right = arenas.exprs.alloc(Expr::Name {
        name_ref: right_ref,
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
    let left_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(0),
        span: Span::new(0, 1),
    });
    let left = arenas.exprs.alloc(Expr::Name {
        name_ref: left_ref,
        span: Span::new(0, 1),
    });
    let right_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(1),
        span: Span::new(4, 1),
    });
    let right = arenas.exprs.alloc(Expr::Name {
        name_ref: right_ref,
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
fn test_walk_expr_crosses_into_ty_annotation() {
    let mut arenas = AstArenas::new();
    let pat = arenas.pats.alloc(Pat::Wild {
        span: Span::new(4, 1),
    });
    let ty_name_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(0),
        span: Span::new(6, 3),
    });
    // Type annotation is now an Expr::Name in the expr arena
    let ty_expr = arenas.exprs.alloc(Expr::Name {
        name_ref: ty_name_ref,
        span: Span::new(6, 3),
    });
    let val_name_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(1),
        span: Span::new(12, 1),
    });
    let value = arenas.exprs.alloc(Expr::Name {
        name_ref: val_name_ref,
        span: Span::new(12, 1),
    });
    let root = arenas.exprs.alloc(Expr::Let {
        fields: LetFields {
            kind: BindKind::Immut,
            pat,
            params: vec![],
            constraints: vec![],
            ty: Some(ty_expr),
            value: Some(value),
            with_effects: None,
            span: Span::new(0, 13),
        },
        body: None,
        span: Span::new(0, 13),
    });

    let mut visitor = CountingVisitor::new();
    let _ = visitor.visit_expr(root, &arenas);
    // exprs: root + ty_expr + value = 3
    assert_eq!(visitor.expr_count, 3);
    // pats: the wildcard = 1
    assert_eq!(visitor.pat_count, 1);
}

#[test]
fn test_walk_expr_visits_record_def_fields() {
    use crate::expr::RecDefField;
    let mut arenas = AstArenas::new();
    let ty_name_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(0),
        span: Span::new(10, 3),
    });
    let ty_expr = arenas.exprs.alloc(Expr::Name {
        name_ref: ty_name_ref,
        span: Span::new(10, 3),
    });
    let root = arenas.exprs.alloc(Expr::RecordDef {
        fields: vec![RecDefField {
            name: Symbol(1),
            ty: ty_expr,
            default: None,
            span: Span::new(0, 10),
        }],
        span: Span::new(0, 20),
    });

    let mut visitor = CountingVisitor::new();
    let _ = visitor.visit_expr(root, &arenas);
    // exprs: root + ty_expr = 2
    assert_eq!(visitor.expr_count, 2);
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
    let inner_ref = arenas.name_refs.alloc(NameRef {
        name: Symbol(0),
        span: Span::new(6, 3),
    });
    let inner = arenas.exprs.alloc(Expr::Name {
        name_ref: inner_ref,
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
