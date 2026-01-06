use crate::ctx::TestCtx;
use musi_ast::TyExprKind;

#[test]
fn test_ty_expr_ident() {
    let mut ctx = TestCtx::new();
    let int = ctx.intern("Int");
    let id = ctx.parse_typ("Int");
    assert!(matches!(ctx.ty_expr(id).kind, TyExprKind::Ident(i) if i == int));
}

#[test]
fn test_ty_expr_app() {
    let mut ctx = TestCtx::new();
    let list = ctx.intern("List");
    let id = ctx.parse_typ("List[Int]");
    let kind = &ctx.ty_expr(id).kind;
    assert!(matches!(kind, TyExprKind::App { base, args } if *base == list && args.len() == 1));
}

#[test]
fn test_ty_expr_optional() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_typ("?Int");
    assert!(matches!(&ctx.ty_expr(id).kind, TyExprKind::Optional(_)));
}

#[test]
fn test_ty_expr_ptr() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_typ("^Int");
    assert!(matches!(&ctx.ty_expr(id).kind, TyExprKind::Ptr(_)));
}

#[test]
fn test_ty_expr_array_unsized() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_typ("[]Int");
    let kind = &ctx.ty_expr(id).kind;
    assert!(matches!(kind, TyExprKind::Array { size: None, .. }));
}

#[test]
fn test_ty_expr_array_sized() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_typ("[10]Int");
    let kind = &ctx.ty_expr(id).kind;
    assert!(matches!(kind, TyExprKind::Array { size: Some(10), .. }));
}

#[test]
fn test_ty_expr_fn() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_typ("Int -> String");
    assert!(matches!(&ctx.ty_expr(id).kind, TyExprKind::Fn { .. }));
}

#[test]
fn test_ty_expr_tuple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_typ("(Int, String)");
    let kind = &ctx.ty_expr(id).kind;
    assert!(matches!(kind, TyExprKind::Tuple(elems) if elems.len() == 2));
}
