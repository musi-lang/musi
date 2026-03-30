mod support;

use music_check::SemaErrorKind;

use support::analyze_text;

#[test]
fn test_multi_arg_call_typechecks() {
    let kinds = analyze_text(
        r#"
let add (a : Int, b : Int) : Int := a + b;
add(1, 2);
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_multi_arg_call_reports_mismatch() {
    let kinds = analyze_text(
        r#"
let add (a : Int, b : Int) : Int := a + b;
add(1, "x");
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. }))
    );
}
