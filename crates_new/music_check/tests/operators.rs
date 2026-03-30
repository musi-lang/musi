mod support;

use support::analyze_text;

#[test]
fn test_symbolic_infix_typechecks_like_call() {
    let kinds = analyze_text(
        r#"
let (++) (a : Int, b : Int) : Int := a + b;
1 ++ 2;
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}
