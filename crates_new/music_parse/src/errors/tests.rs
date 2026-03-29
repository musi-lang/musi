use music_basic::{SourceMap, Span};
use music_lex::TokenKind;

use super::*;

#[test]
fn test_parse_error_converts_to_diag() {
    let mut sources = SourceMap::default();
    let source_id = sources.add("test.ms", "let x := ");
    let error = ParseError {
        kind: ParseErrorKind::ExpectedExpression {
            found: Box::new(TokenKind::Eof),
        },
        span: Span::new(9, 9),
    };

    let diag = error.to_diag(source_id);

    assert_eq!(diag.labels.len(), 1);
    assert_eq!(
        error.kind.to_string(),
        "expected expression, found end of file"
    );
}

#[test]
fn test_token_expectation_renders_source_spelling() {
    let error = ParseErrorKind::ExpectedToken {
        expected: Box::new(TokenKind::LBrace),
        found: Box::new(TokenKind::RParen),
    };

    assert_eq!(error.to_string(), "expected token '{', found ')'");
}
