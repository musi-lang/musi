use crate::Inferer;
use crate::test_ctx::TestCtx;
use crate::ty::TyKind;

#[test]
fn test_infer_lit_int() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("42");
    assert!(matches!(ctx.infer(id), TyKind::Int));
}

#[test]
fn test_infer_lit_real() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("3.14");
    assert!(matches!(ctx.infer(id), TyKind::Real));
}

#[test]
fn test_infer_lit_string() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr(r#""hello""#);
    assert!(matches!(ctx.infer(id), TyKind::String));
}

#[test]
fn test_infer_lit_bool() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("true");
    assert!(matches!(ctx.infer(id), TyKind::Bool));
}

#[test]
fn test_infer_tuple() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("(1, 2, 3)");
    assert!(matches!(ctx.infer(id), TyKind::Tuple(elems) if elems.len() == 3));
}

#[test]
fn test_infer_array() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("[1, 2, 3]");
    assert!(matches!(ctx.infer(id), TyKind::Array(_)));
}

#[test]
fn test_infer_record() {
    let mut ctx = TestCtx::new();
    let x = ctx.intern("x");
    let id = ctx.parse_expr(".{x := 1}");
    assert!(
        matches!(ctx.infer(id), TyKind::Record { fields } if fields.len() == 1 && fields[0].0 == x)
    );
}

#[test]
fn test_infer_binary_arithmetic() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("1 + 2");
    assert!(matches!(ctx.infer(id), TyKind::Int));
}

#[test]
fn test_infer_binary_comparison() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("1 < 2");
    assert!(matches!(ctx.infer(id), TyKind::Bool));
}

#[test]
fn test_infer_unary_not() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("not true");
    assert!(matches!(ctx.infer(id), TyKind::Bool));
}

#[test]
fn test_infer_range() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("1..10");
    assert!(matches!(ctx.infer(id), TyKind::Range(_)));
}

#[test]
fn test_infer_block() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("{ 1; 2; 3 }");
    assert!(matches!(ctx.infer(id), TyKind::Int));
}

#[test]
fn test_infer_if() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("if true { 1 } else { 2 }");
    assert!(matches!(ctx.infer(id), TyKind::Int));
}

#[test]
fn test_infer_binding() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("val x := 42");
    assert!(matches!(ctx.infer(id), TyKind::Unit));
}

#[test]
fn test_infer_record_def() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("record Point { x: Int, y: Int }");
    assert!(matches!(ctx.infer(id), TyKind::Unit));
}

#[test]
fn test_infer_choice_def() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("choice Option { case Some(Int), case None }");
    assert!(matches!(ctx.infer(id), TyKind::Unit));
}

#[test]
fn test_infer_type_def() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("type MyInt := Int");
    assert!(matches!(ctx.infer(id), TyKind::Unit));
}

#[test]
fn test_infer_match_tuple_pattern() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("match (1, 2) { case (a, b) => a + b }");
    assert!(matches!(ctx.infer(id), TyKind::Int));
}

#[test]
fn test_infer_match_array_pattern() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("match [1, 2] { case [a, b] => a + b }");
    assert!(matches!(ctx.infer(id), TyKind::Int));
}

#[test]
fn test_infer_match_or_pattern() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("match 1 { case 0 | 1 | 2 => true }");
    assert!(matches!(ctx.infer(id), TyKind::Bool));
}

#[test]
fn test_infer_match_as_pattern() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("match 42 { case x as y => x + y }");
    assert!(matches!(ctx.infer(id), TyKind::Int));
}

#[test]
fn test_infer_binding_with_type_annotation() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("val x: Int := 42");
    assert!(matches!(ctx.infer(id), TyKind::Unit));
}

#[test]
fn test_infer_binding_optional_type_mismatch() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("val x: ?Int := 42");
    let mut inferer = Inferer::new(
        &ctx.ast_arena,
        &ctx.interner,
        &mut ctx.ty_arena,
        &mut ctx.env,
        &mut ctx.table,
    );
    assert!(inferer.infer_expr(id).is_err());
}

#[test]
fn test_infer_binding_array_type() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("val x: []Int := [1, 2, 3]");
    assert!(matches!(ctx.infer(id), TyKind::Unit));
}

#[test]
fn test_infer_binding_tuple_type() {
    let mut ctx = TestCtx::new();
    let id = ctx.parse_expr("val x: (Int, Bool) := (1, true)");
    assert!(matches!(ctx.infer(id), TyKind::Unit));
}
