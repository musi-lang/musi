mod support;

use music_check::SemaErrorKind;

use support::{analyze_text, analyze_text_full};

#[test]
fn test_typechecks_simple_let_annot() {
    let kinds = analyze_text("let x : Int := 1; x;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_reports_type_mismatch() {
    let kinds = analyze_text("let x : Int := \"hi\"; x;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. }))
    );
}

#[test]
fn test_fstring_interpolation_requires_string_parts() {
    let kinds = analyze_text("let x := 1; f\"{x}\";");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TypeMismatch { .. }))
    );
}

#[test]
fn test_typechecks_handle_value_and_op_clauses() {
    let errors = analyze_text_full(
        r#"
let Console := effect { let writeln (msg : String) : Unit; };
let work () with { Console } : Int := (
  perform Console.writeln("x");
  1
);

handle work() with Console of (
| v => v
| writeln(msg, k) => resume ()
);
"#,
    );
    assert!(errors.is_empty(), "expected no errors, got {errors:?}");
}
