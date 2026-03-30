use music_basic::{DiagCode, SourceMap, Span};

use super::*;

#[test]
fn undefined_binding_has_stable_code() {
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", "let x := 1;");

    let err = ResolveError {
        kind: ResolveErrorKind::UndefinedBinding {
            name: "x".to_string(),
        },
        source_id,
        span: Span::new(1, 2),
    };

    let diag = err.to_diag();
    assert_eq!(diag.code, Some(DiagCode::new(3001)));
}

#[test]
fn duplicate_binding_has_stable_code() {
    let mut sources = SourceMap::new();
    let source_id = sources.add("test.ms", "let x := 1;");

    let err = ResolveError {
        kind: ResolveErrorKind::DuplicateBinding {
            name: "x".to_string(),
            first: Span::new(1, 2),
        },
        source_id,
        span: Span::new(3, 4),
    };

    let diag = err.to_diag();
    assert_eq!(diag.code, Some(DiagCode::new(3002)));
}

