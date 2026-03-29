use music_basic::{DiagCode, SourceMap, Span};

use super::*;

#[test]
fn test_display_matches_error_kind() {
    let error = LexError {
        kind: LexErrorKind::InvalidEscape('z'),
        span: Span::new(4, 6),
    };

    assert_eq!(error.to_string(), "invalid escape sequence '\\z'");
}

#[test]
fn test_diagnostic_uses_expected_code() {
    let error = LexError {
        kind: LexErrorKind::UnterminatedString,
        span: Span::new(1, 4),
    };
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "\"bad");
    let diag = error.diagnostic(source_id);

    assert_eq!(diag.code, Some(DiagCode::new(1002)));
    assert_eq!(diag.labels.len(), 1);
}
