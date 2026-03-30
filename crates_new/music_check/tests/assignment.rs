mod support;

use music_check::SemaErrorKind;

use support::analyze_text;

#[test]
fn test_assign_requires_let_mut_binding() {
    let kinds = analyze_text("let immutable := 0; immutable <- 1;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::AssignTargetRequiresMutableBinding { .. }))
    );
}

#[test]
fn test_assign_allows_let_mut_binding() {
    let kinds = analyze_text("let mut mutable := 0; mutable <- 1;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_assign_index_requires_mut_value() {
    let kinds = analyze_text("let array := [1, 2, 3]; array.[0] <- 4;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::AssignTargetRequiresWritableBase))
    );
}

#[test]
fn test_assign_index_allows_mut_value() {
    let kinds = analyze_text("let array := mut [1, 2, 3]; array.[0] <- 4;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_assign_member_allows_mut_value() {
    let kinds = analyze_text("let p := mut { x := 1 }; p.x <- 2;");
    assert!(kinds.is_empty(), "expected no errors, got {kinds:?}");
}

#[test]
fn test_assign_rejects_non_place_target() {
    let kinds = analyze_text("1 <- 2;");
    assert!(
        kinds
            .iter()
            .any(|k| matches!(k, SemaErrorKind::AssignTargetInvalid))
    );
}
