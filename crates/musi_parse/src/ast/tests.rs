
use super::*;
use musi_shared::{Span, Symbol};

#[test]
fn parse_ctx_new_creates_empty_arenas() {
    let ctx = AstArenas::new();
    assert!(ctx.exprs.is_empty());
    assert!(ctx.tys.is_empty());
    assert!(ctx.pats.is_empty());
}

#[test]
fn parse_ctx_default_matches_new() {
    let ctx = AstArenas::default();
    assert!(ctx.exprs.is_empty());
    assert!(ctx.tys.is_empty());
    assert!(ctx.pats.is_empty());
}

#[test]
fn alloc_expr_and_retrieve() {
    let mut ctx = AstArenas::new();
    let span = Span::new(0, 5);
    let idx = ctx.exprs.alloc(Expr::Unit { span });
    let node = ctx.exprs.get(idx);
    assert_eq!(*node, Expr::Unit { span });
}

#[test]
fn alloc_ty_and_retrieve() {
    let mut ctx = AstArenas::new();
    let span = Span::new(10, 3);
    let idx = ctx.tys.alloc(Ty::Var {
        name: Symbol(0),
        span,
    });
    let node = ctx.tys.get(idx);
    assert_eq!(
        *node,
        Ty::Var {
            name: Symbol(0),
            span,
        }
    );
}

#[test]
fn alloc_pat_and_retrieve() {
    let mut ctx = AstArenas::new();
    let span = Span::new(20, 1);
    let idx = ctx.pats.alloc(Pat::Wild { span });
    let node = ctx.pats.get(idx);
    assert_eq!(*node, Pat::Wild { span });
}

#[test]
fn multiple_exprs_independent_indices() {
    let mut ctx = AstArenas::new();
    let a = ctx.exprs.alloc(Expr::Unit {
        span: Span::new(0, 2),
    });
    let b = ctx.exprs.alloc(Expr::Error {
        span: Span::new(5, 1),
    });
    assert_ne!(a, b);
    assert_eq!(
        *ctx.exprs.get(a),
        Expr::Unit {
            span: Span::new(0, 2),
        }
    );
    assert_eq!(
        *ctx.exprs.get(b),
        Expr::Error {
            span: Span::new(5, 1),
        }
    );
}

// Compile-time safety: `Idx<Expr>` cannot be used to index `Arena<Ty>`.
// This is enforced by the type system and cannot be demonstrated in a
// runtime test. Attempting `ctx.tys.get(expr_idx)` would fail to compile.

#[test]
fn leaf_lit_node_preserves_span() {
    let span = Span::new(42, 7);
    let lit = Expr::Lit {
        value: LitValue::Int(123),
        span,
    };
    assert!(
        matches!(&lit, Expr::Lit { value: LitValue::Int(123), span: s } if s.start == 42 && s.length == 7),
        "expected Expr::Lit(Int(123))"
    );
}

#[test]
fn binary_expr_round_trip() {
    let mut ctx = AstArenas::new();
    let lhs = ctx.exprs.alloc(Expr::Lit {
        value: LitValue::Int(1),
        span: Span::new(0, 1),
    });
    let rhs = ctx.exprs.alloc(Expr::Lit {
        value: LitValue::Int(2),
        span: Span::new(4, 1),
    });
    let add = ctx.exprs.alloc(Expr::Binary {
        op: BinOp::Add,
        lhs,
        rhs,
        span: Span::new(0, 5),
    });
    let node = ctx.exprs.get(add);
    assert!(
        matches!(node, Expr::Binary { op: BinOp::Add, span, .. } if span.start == 0 && span.length == 5)
    );
    if let Expr::Binary { lhs: l, rhs: r, .. } = node {
        assert!(matches!(
            ctx.exprs.get(*l),
            Expr::Lit {
                value: LitValue::Int(1),
                ..
            }
        ));
        assert!(matches!(
            ctx.exprs.get(*r),
            Expr::Lit {
                value: LitValue::Int(2),
                ..
            }
        ));
    }
}

#[test]
fn parsed_module_stores_items() {
    let mut ctx = AstArenas::new();
    let a = ctx.exprs.alloc(Expr::Unit {
        span: Span::new(0, 2),
    });
    let b = ctx.exprs.alloc(Expr::Unit {
        span: Span::new(3, 2),
    });
    let items = ctx.expr_lists.alloc_slice([a, b]);
    let module = ParsedModule {
        items,
        ctx,
        span: Span::new(0, 5),
    };
    let slice = module.ctx.expr_lists.get_slice(module.items);
    assert_eq!(slice.len(), 2);
    assert_eq!(
        *module.ctx.exprs.get(slice[0]),
        Expr::Unit {
            span: Span::new(0, 2)
        }
    );
}
