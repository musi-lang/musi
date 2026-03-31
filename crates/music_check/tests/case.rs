mod support;

use music_check::SemaErrorKind;

use support::analyze_text;

#[test]
fn test_case_tuple_pattern_requires_tuple() {
    let kinds = analyze_text(
        r#"
case 1 of (
| (x, y) => 0
);
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::TuplePatternRequiresTuple))
    );
}

#[test]
fn test_case_record_pattern_requires_record() {
    let kinds = analyze_text(
        r#"
case 1 of (
| {x} => 0
);
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::RecordPatternRequiresRecord))
    );
}

#[test]
fn test_case_variant_pattern_unknown_variant_reports_error() {
    let kinds = analyze_text(
        r#"
let Option[T] := data { Some : T | None };
let v : Option[Int] := .None;
case v of (
| .Missing => 0
| .None => 1
);
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::VariantNotFound { .. }))
    );
}

#[test]
fn test_record_pat_mut_allows_assignment() {
    let kinds = analyze_text(
        r#"
case {x := 1} of (
| { mut x } => (x <- 2; x)
);
"#,
    );
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_record_pat_without_mut_reports_assignment_error() {
    let kinds = analyze_text(
        r#"
case {x := 1} of (
| { x } => (x <- 2; x)
);
"#,
    );
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::AssignTargetRequiresMutableBinding { .. }))
    );
}
