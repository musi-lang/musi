#![allow(clippy::panic)]

use music_shared::{Span, Symbol};

use crate::expr::{BinOp, BindKind, Expr, LetFields};
use crate::pat::Pat;
use crate::{AstArenas, Lit};

#[test]
fn test_let_fields_round_trip_through_arena() {
    let mut arenas = AstArenas::new();
    let pat = arenas.pats.alloc(Pat::Wild {
        span: Span::new(4, 1),
    });
    let value = arenas.exprs.alloc(Expr::Lit {
        lit: Lit::Int {
            value: 1,
            span: Span::new(8, 1),
        },
        span: Span::new(8, 1),
    });
    let fields = LetFields {
        kind: BindKind::Immut,
        heap: false,
        pat,
        params: vec![],
        constraints: vec![],
        ty: None,
        value: Some(value),
        span: Span::new(0, 10),
    };
    let idx = arenas.exprs.alloc(Expr::Let {
        fields,
        body: None,
        span: Span::new(0, 10),
    });
    let Expr::Let { fields: stored, .. } = &arenas.exprs[idx] else {
        panic!("expected Let");
    };
    assert_eq!(stored.kind, BindKind::Immut);
    assert!(!stored.heap);
    assert_eq!(stored.pat, pat);
    assert_eq!(stored.value, Some(value));
}

#[test]
fn test_binop_children_accessible_via_idx() {
    let mut arenas = AstArenas::new();
    let left = arenas.exprs.alloc(Expr::Lit {
        lit: Lit::Int {
            value: 1,
            span: Span::new(0, 1),
        },
        span: Span::new(0, 1),
    });
    let right = arenas.exprs.alloc(Expr::Lit {
        lit: Lit::Int {
            value: 2,
            span: Span::new(4, 1),
        },
        span: Span::new(4, 1),
    });
    let idx = arenas.exprs.alloc(Expr::BinOp {
        op: BinOp::Add,
        left,
        right,
        span: Span::new(0, 5),
    });
    let Expr::BinOp {
        left: l, right: r, ..
    } = &arenas.exprs[idx]
    else {
        panic!("expected BinOp");
    };
    assert_eq!(*l, left);
    assert_eq!(*r, right);
}

#[test]
fn test_block_with_no_tail() {
    let mut arenas = AstArenas::new();
    let s1 = arenas.exprs.alloc(Expr::Lit {
        lit: Lit::Unit {
            span: Span::new(1, 2),
        },
        span: Span::new(1, 2),
    });
    let idx = arenas.exprs.alloc(Expr::Block {
        stmts: vec![s1],
        tail: None,
        span: Span::new(0, 5),
    });
    let Expr::Block { stmts, tail, .. } = &arenas.exprs[idx] else {
        panic!("expected Block");
    };
    assert_eq!(stmts.len(), 1);
    assert!(tail.is_none());
}

#[test]
fn test_binding_embeds_let_fields() {
    let mut arenas = AstArenas::new();
    let pat = arenas.pats.alloc(Pat::Wild {
        span: Span::new(4, 1),
    });
    let value = arenas.exprs.alloc(Expr::Lit {
        lit: Lit::Int {
            value: 42,
            span: Span::new(8, 2),
        },
        span: Span::new(8, 2),
    });
    let fields = LetFields {
        kind: BindKind::Mut,
        heap: true,
        pat,
        params: vec![],
        constraints: vec![],
        ty: None,
        value: Some(value),
        span: Span::new(0, 10),
    };
    let idx = arenas.exprs.alloc(Expr::Binding {
        exported: true,
        fields,
        span: Span::new(0, 10),
    });
    let Expr::Binding {
        exported, fields, ..
    } = &arenas.exprs[idx]
    else {
        panic!("expected Binding");
    };
    assert!(*exported);
    assert_eq!(fields.kind, BindKind::Mut);
    assert!(fields.heap);
}

#[test]
fn test_binop_cons_round_trip_through_arena() {
    let mut arenas = AstArenas::new();
    let head = arenas.exprs.alloc(Expr::Lit {
        lit: Lit::Int {
            value: 1,
            span: Span::new(0, 1),
        },
        span: Span::new(0, 1),
    });
    let tail = arenas.exprs.alloc(Expr::Name {
        name: Symbol(0),
        span: Span::new(5, 2),
    });
    let idx = arenas.exprs.alloc(Expr::BinOp {
        op: BinOp::Cons,
        left: head,
        right: tail,
        span: Span::new(0, 7),
    });
    let Expr::BinOp {
        op,
        left: l,
        right: r,
        ..
    } = &arenas.exprs[idx]
    else {
        panic!("expected BinOp");
    };
    assert_eq!(*op, BinOp::Cons);
    assert_eq!(*l, head);
    assert_eq!(*r, tail);
}

#[test]
fn test_expr_error_is_leaf() {
    let mut arenas = AstArenas::new();
    let idx = arenas.exprs.alloc(Expr::Error {
        span: Span::new(0, 1),
    });
    assert!(matches!(&arenas.exprs[idx], Expr::Error { .. }));
}
