use music_basic::{DiagCode, SourceMap, Span};

use super::*;

#[test]
fn test_expected_type_found_type_has_stable_code() {
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", "let x := 1;");

    let err = SemaError {
        kind: SemaErrorKind::TypeMismatch {
            expected: "Int".to_string(),
            found: "String".to_string(),
        },
        source_id,
        span: Span::new(1, 2),
    };

    let diag = err.to_diag();
    assert_eq!(diag.code, Some(DiagCode::new(3006)));
}

#[test]
fn test_missing_with_clause_has_stable_code() {
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", "let x := 1;");

    let err = SemaError {
        kind: SemaErrorKind::MissingWithClause,
        source_id,
        span: Span::new(3, 4),
    };

    let diag = err.to_diag();
    assert_eq!(diag.code, Some(DiagCode::new(3007)));
}
