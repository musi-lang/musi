use crate::test_utils::TestContext;

#[test]
fn bind_inference_simple() {
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("val x := 10;");
    assert!(diags.is_empty(), "bind errors: {diags:?}");
}

#[test]
fn bind_inference_transitive() {
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("val x := 10; val y := x;");
    assert!(diags.is_empty(), "bind errors: {diags:?}");
}

#[test]
fn bind_gradual_any() {
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("val x: Any := 10; val y: Int := x;");
    assert!(diags.is_empty(), "bind errors: {diags:?}");
}

#[test]
fn bind_mutability_ok() {
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("var x := 10; x <- 22;");
    assert!(diags.is_empty(), "bind errors: {diags:?}");
}

#[test]
fn bind_mutability_error() {
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("val x := 10; x <- 20;");
    assert!(
        !diags.is_empty(),
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
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("val x := 10; { val x := true; };");
    assert!(diags.is_empty(), "bind errors: {diags:?}");
}

#[test]
fn bind_scope_access_error() {
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("{ val x := 10; }; x;");
    assert!(
        !diags.is_empty(),
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
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("val x := if true { 10 } else { 20 };");
    assert!(diags.is_empty(), "bind errors: {diags:?}");
}

#[test]
fn bind_if_expr_mismatch() {
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("val x := if true { 10 } else { true };");
    assert!(!diags.is_empty(), "Expected error for type mismatch");
    let msg = diags.diagnostics[0].message.clone();
    assert!(
        msg.contains("cannot convert type"),
        "Unexpected error: {msg}"
    );
}

#[test]
fn bind_while_scope() {
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("while true { val x := 10; }; x;");
    assert!(
        !diags.is_empty(),
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
    let mut ctx = TestContext::new();
    let (_, diags) = ctx.check_bind("for i in [1, 2, 3] { i; }; i;");
    assert!(
        !diags.is_empty(),
        "expected error for accessing for-loop variable out of scope"
    );
    let msg = diags.diagnostics[0].message.clone();
    assert!(
        msg.contains("undefined identifier"),
        "unexpected error: {msg}"
    );
}
