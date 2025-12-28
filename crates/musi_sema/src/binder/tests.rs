use crate::test_utils::TestCtx;

#[test]
fn bind_inference_simple() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := 10;");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_inference_transitive() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := 10; val y := x;");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_gradual_any() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x: Any := 10; val y: Int := x;");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_mutability_ok() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("var x := 10; x <- 22;");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_mutability_error() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := 10; x <- 20;");
    assert!(
        diags.errors > 0,
        "expected error for assigning to immutable binding"
    );
    let msg = diags.diagnostics[0].message.clone();
    assert!(
        msg.contains("cannot assign to immutable binding"),
        "unexpected error: {msg}"
    );
}

#[test]
fn bind_scope_shadowing() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := 10; { val x := true; };");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_scope_access_error() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("{ val x := 10; }; x;");
    assert!(
        diags.errors > 0,
        "expected error for accessing variable out of scope"
    );
    let msg = diags.diagnostics[0].message.clone();
    assert!(
        msg.contains("undefined identifier"),
        "unexpected error: {msg}"
    );
}

#[test]
fn bind_if_expr_types() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := if true { 10 } else { 20 };");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_if_expr_mismatch() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := if true { 10 } else { true };");
    assert!(diags.errors > 0, "Expected error for type mismatch");
    let msg = diags.diagnostics[0].message.clone();
    assert!(
        msg.contains("cannot convert type"),
        "Unexpected error: {msg}"
    );
}

#[test]
fn bind_while_scope() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("while true { val x := 10; }; x;");
    assert!(
        diags.errors > 0,
        "expected error for accessing while-loop variable out of scope"
    );
    let msg = diags.diagnostics[0].message.clone();
    assert!(
        msg.contains("undefined identifier"),
        "unexpected error: {msg}"
    );
}

#[test]
fn bind_for_scope() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("for i in [1, 2, 3] { i; }; i;");
    assert!(
        diags.errors > 0,
        "expected error for accessing for-loop variable out of scope"
    );
    let msg = diags.diagnostics[0].message.clone();
    assert!(
        msg.contains("undefined identifier"),
        "unexpected error: {msg}"
    );
}

#[test]
fn bind_fn_basic() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("fn add(x: Int, y: Int): Int { x + y };");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_fn_ret_ty() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("fn foo(): Int { true; };");
    assert!(diags.errors > 0, "expected return type mismatch error");
    let msg = diags.diagnostics[0].message.clone();
    assert!(
        msg.contains("cannot convert type"),
        "unexpected error: {msg}"
    );
}

#[test]
fn bind_fn_params() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("fn foo(x: Int) { x + true; };");
    assert!(
        diags.errors > 0,
        "expected error for binary op type mismatch"
    );
}

#[test]
fn bind_binary_arithmetic() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := 10 + 20 * 30 / 2;");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_binary_relational() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := 10 < 20; val y := 30 >= 40;");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_unary_neg() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := -10; val y := not true;");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_array_lit() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := [1, 2, 3];");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_tuple_lit() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind(r#"val x := (1, true, "hi");"#);
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_index_expr() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := [1, 2, 3]; val y := x.[0];");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_match_basic() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val x := match 10 { case 10 => true, case _ => false };");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_for_iterator() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("var sum := 0; for i in [1, 2, 3] { sum <- sum + i; };");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}

#[test]
fn bind_pat_tuple() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind("val (x, y) := (1, 2);");
    assert!(diags.errors == 0, "bind errors: {diags:?}");
}
#[test]
fn bind_redundant_type_def() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) =
        ctx.check_bind("val Option := choice Option { case Some(Int), case None };");
    assert!(
        diags.errors == 0,
        "expected no duplicate definition error, got: {diags:?}"
    );
}

#[test]
fn bind_template_expr() {
    let mut ctx = TestCtx::new();
    let (_, _, diags) = ctx.check_bind(r#"val name := "World"; val msg := $"Hello {name}";"#);
    assert!(
        diags.errors == 0,
        "expected no errors in template, got: {diags:?}"
    );
}
