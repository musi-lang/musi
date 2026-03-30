mod support;

use music_check::SemaErrorKind;

use support::analyze_text;

#[test]
fn test_tuple_projection_dot_int_typechecks() {
    let kinds = analyze_text("let t := (1, 2); t.0;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_tuple_projection_dot_int_reports_out_of_range() {
    let kinds = analyze_text("let t := (1, 2); t.2;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TupleIndexOutOfRange { .. }))
    );
}

#[test]
fn test_tuple_index_brackets_typechecks() {
    let kinds = analyze_text("let t := (1, 2); t.[1];");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_record_computed_field_typechecks() {
    let kinds = analyze_text("let r := { x := 1, y := 2 }; r.[\"x\"];");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_data_computed_field_typechecks() {
    let kinds = analyze_text(
        r#"
let P := data { x : Int; y : Int; };
let p : P := { x := 1, y := 2 };
p.["x"];
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_computed_field_reports_missing() {
    let kinds = analyze_text("let r := { x := 1 }; r.[\"y\"];");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::FieldNotFound { .. }))
    );
}

#[test]
fn test_multi_index_reports_exceeds_array_nesting() {
    let kinds = analyze_text("let xs := [1, 2]; xs.[0, 0];");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::IndexExceedsArrayNesting))
    );
}
