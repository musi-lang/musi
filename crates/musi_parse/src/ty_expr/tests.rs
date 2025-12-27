use crate::test_utils::TestContext;
use musi_ast::TyExprKind;

#[test]
fn test_ty_expr_ident() {
    let mut ctx = TestContext::new();
    let int = ctx.intern("Int");
    let id = ctx.parse_typ("Int");
    assert!(matches!(ctx.ty_expr(id).kind, TyExprKind::Ident(i) if i == int));
}

#[test]
fn test_ty_expr_app() {
    let mut ctx = TestContext::new();
    let list = ctx.intern("List");
    let id = ctx.parse_typ("List[Int]");
    if let TyExprKind::App { base, args } = &ctx.ty_expr(id).kind {
        assert_eq!(*base, list);
        assert_eq!(args.len(), 1);
    } else {
        panic!("expected type application");
    }
}

#[test]
fn test_ty_expr_optional() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_typ("?Int");
    assert!(matches!(&ctx.ty_expr(id).kind, TyExprKind::Optional(_)));
}

#[test]
fn test_ty_expr_ptr() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_typ("^Int");
    assert!(matches!(&ctx.ty_expr(id).kind, TyExprKind::Ptr(_)));
}

#[test]
fn test_ty_expr_array_unsized() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_typ("[]Int");
    if let TyExprKind::Array { size, .. } = &ctx.ty_expr(id).kind {
        assert!(size.is_none());
    } else {
        panic!("expected array type");
    }
}

#[test]
fn test_ty_expr_array_sized() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_typ("[10]Int");
    if let TyExprKind::Array { size, .. } = &ctx.ty_expr(id).kind {
        assert_eq!(*size, Some(10));
    } else {
        panic!("expected sized array type");
    }
}

#[test]
fn test_ty_expr_fn() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_typ("Int -> String");
    assert!(matches!(&ctx.ty_expr(id).kind, TyExprKind::Fn { .. }));
}

#[test]
fn test_ty_expr_tuple() {
    let mut ctx = TestContext::new();
    let id = ctx.parse_typ("(Int, String)");
    if let TyExprKind::Tuple(elems) = &ctx.ty_expr(id).kind {
        assert_eq!(elems.len(), 2);
    } else {
        panic!("expected tuple type");
    }
}
