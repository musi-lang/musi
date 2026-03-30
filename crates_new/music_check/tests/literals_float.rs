mod support;

use music_check::SemaErrorKind;

use support::analyze_text;

#[test]
fn test_float_literals_support_exponent() {
    let kinds = analyze_text(
        r#"
let a : Float := 1e3;
let b : Float := 3.14e-2;
(a; b);
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_float_int_mismatch_reports_error() {
    let kinds = analyze_text("let x : Int := 1e3; x;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. }))
    );
}
