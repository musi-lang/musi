use music_found::{Literal, Span, Spanned};

use crate::data::AstData;
use crate::expr::{BinOp, ExprKind};
use crate::walk::map_expr_children;

fn dummy(kind: ExprKind) -> Spanned<ExprKind> {
    Spanned::new(kind, Span::DUMMY)
}

#[test]
fn identity_returns_same_id() {
    let mut ast = AstData::new();
    let lit = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(1))));
    let result = map_expr_children(&mut ast, lit, &mut |_, id| id);
    assert_eq!(result, lit, "identity transform must not allocate");
}

#[test]
fn identity_on_binop_returns_same_id() {
    let mut ast = AstData::new();
    let lhs = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(1))));
    let rhs = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(2))));
    let binop = ast
        .exprs
        .alloc(dummy(ExprKind::BinOp(BinOp::Add, lhs, rhs)));
    let result = map_expr_children(&mut ast, binop, &mut |_, id| id);
    assert_eq!(result, binop, "identity on BinOp must not allocate");
}

#[test]
fn transforms_binop_child() {
    let mut ast = AstData::new();
    let old_lhs = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(1))));
    let rhs = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(2))));
    let replacement = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(99))));
    let binop = ast
        .exprs
        .alloc(dummy(ExprKind::BinOp(BinOp::Add, old_lhs, rhs)));

    let result = map_expr_children(&mut ast, binop, &mut |_, id| {
        if id == old_lhs {
            replacement
        } else {
            id
        }
    });

    assert_ne!(result, binop, "changed child must produce new node");
    match &ast.exprs.get(result).kind {
        ExprKind::BinOp(BinOp::Add, new_lhs, new_rhs) => {
            assert_eq!(*new_lhs, replacement);
            assert_eq!(*new_rhs, rhs);
        }
        other => panic!("expected BinOp, got {other:?}"),
    }
}

#[test]
fn identity_on_seq_returns_same_id() {
    let mut ast = AstData::new();
    let a = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(1))));
    let b = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(2))));
    let seq = ast.exprs.alloc(dummy(ExprKind::Seq(vec![a, b])));
    let result = map_expr_children(&mut ast, seq, &mut |_, id| id);
    assert_eq!(result, seq);
}

#[test]
fn transforms_seq_child() {
    let mut ast = AstData::new();
    let a = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(1))));
    let b = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(2))));
    let replacement = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(42))));
    let seq = ast.exprs.alloc(dummy(ExprKind::Seq(vec![a, b])));

    let result = map_expr_children(&mut ast, seq, &mut |_, id| {
        if id == a {
            replacement
        } else {
            id
        }
    });

    assert_ne!(result, seq);
    match &ast.exprs.get(result).kind {
        ExprKind::Seq(items) => {
            assert_eq!(items.len(), 2);
            assert_eq!(items[0], replacement);
            assert_eq!(items[1], b);
        }
        other => panic!("expected Seq, got {other:?}"),
    }
}

#[test]
fn identity_on_branch_returns_same_id() {
    let mut ast = AstData::new();
    let cond = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(0))));
    let then_br = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(1))));
    let else_br = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(2))));
    let branch = ast.exprs.alloc(dummy(ExprKind::Branch {
        cond,
        then_br,
        else_br,
    }));
    let result = map_expr_children(&mut ast, branch, &mut |_, id| id);
    assert_eq!(result, branch);
}

#[test]
fn transforms_branch_child() {
    let mut ast = AstData::new();
    let cond = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(0))));
    let then_br = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(1))));
    let else_br = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(2))));
    let replacement = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(99))));
    let branch = ast.exprs.alloc(dummy(ExprKind::Branch {
        cond,
        then_br,
        else_br,
    }));

    let result = map_expr_children(&mut ast, branch, &mut |_, id| {
        if id == else_br {
            replacement
        } else {
            id
        }
    });

    assert_ne!(result, branch);
    match &ast.exprs.get(result).kind {
        ExprKind::Branch {
            cond: c,
            then_br: t,
            else_br: e,
        } => {
            assert_eq!(*c, cond);
            assert_eq!(*t, then_br);
            assert_eq!(*e, replacement);
        }
        other => panic!("expected Branch, got {other:?}"),
    }
}

#[test]
fn leaf_node_unchanged() {
    let mut ast = AstData::new();
    let var = ast.exprs.alloc(dummy(ExprKind::Lit(Literal::Int(7))));
    let arena_len_before = ast.exprs.len();
    let result = map_expr_children(&mut ast, var, &mut |_, _| {
        panic!("closure should not be called for leaf nodes");
    });
    assert_eq!(result, var);
    assert_eq!(ast.exprs.len(), arena_len_before);
}
