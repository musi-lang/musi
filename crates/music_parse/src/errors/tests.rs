use music_shared::Span;

use crate::errors::{ParseError, ParseErrorKind};

#[test]
fn display_expected_token() {
    let err = ParseError {
        kind: ParseErrorKind::ExpectedToken {
            expected: "')'",
            found: "';'",
        },
        span: Span::new(0, 1),
        context: None,
    };
    assert_eq!(err.to_string(), "expected ')', found ';'");
}

#[test]
fn display_unexpected_eof() {
    let err = ParseError {
        kind: ParseErrorKind::UnexpectedEof {
            expected: "expression",
        },
        span: Span::new(5, 5),
        context: None,
    };
    assert_eq!(
        err.to_string(),
        "unexpected end of input, expected expression"
    );
}

#[test]
fn display_expected_expr() {
    let err = ParseError {
        kind: ParseErrorKind::ExpectedExpr { found: "';'" },
        span: Span::new(0, 1),
        context: None,
    };
    assert_eq!(err.to_string(), "expected expression, found ';'");
}

#[test]
fn display_expected_pat() {
    let err = ParseError {
        kind: ParseErrorKind::ExpectedPat { found: "'+'" },
        span: Span::new(0, 1),
        context: None,
    };
    assert_eq!(err.to_string(), "expected pattern, found '+'");
}

#[test]
fn display_expected_type() {
    let err = ParseError {
        kind: ParseErrorKind::ExpectedType { found: "'+'" },
        span: Span::new(0, 1),
        context: None,
    };
    assert_eq!(err.to_string(), "expected type, found `'+'`");
}

#[test]
fn display_unclosed_delimiter() {
    let err = ParseError {
        kind: ParseErrorKind::UnclosedDelimiter {
            open: "'('",
            open_span: Span::new(0, 1),
        },
        span: Span::new(5, 5),
        context: None,
    };
    assert_eq!(err.to_string(), "unclosed delimiter '('");
}

#[test]
fn display_invalid_paren_form() {
    let err = ParseError {
        kind: ParseErrorKind::InvalidParenForm,
        span: Span::new(0, 5),
        context: None,
    };
    assert_eq!(err.to_string(), "invalid parenthesized form");
}

#[test]
fn display_non_associative_chain() {
    let err = ParseError {
        kind: ParseErrorKind::NonAssociativeChain,
        span: Span::new(2, 3),
        context: None,
    };
    assert_eq!(err.to_string(), "non-associative operator chained");
}

#[test]
fn display_with_context() {
    let err = ParseError {
        kind: ParseErrorKind::ExpectedToken {
            expected: "')'",
            found: "';'",
        },
        span: Span::new(0, 1),
        context: Some("in match expression"),
    };
    assert_eq!(
        err.to_string(),
        "expected ')', found ';' in match expression"
    );
}

#[test]
fn span_returns_correct_span() {
    let s = Span::new(10, 20);

    assert_eq!(
        ParseError {
            kind: ParseErrorKind::ExpectedToken {
                expected: "x",
                found: "y",
            },
            span: s,
            context: None,
        }
        .span(),
        s
    );
    assert_eq!(
        ParseError {
            kind: ParseErrorKind::UnexpectedEof { expected: "x" },
            span: s,
            context: None,
        }
        .span(),
        s
    );
    assert_eq!(
        ParseError {
            kind: ParseErrorKind::ExpectedExpr { found: "y" },
            span: s,
            context: None,
        }
        .span(),
        s
    );
    assert_eq!(
        ParseError {
            kind: ParseErrorKind::ExpectedPat { found: "y" },
            span: s,
            context: None,
        }
        .span(),
        s
    );
    assert_eq!(
        ParseError {
            kind: ParseErrorKind::ExpectedType { found: "y" },
            span: s,
            context: None,
        }
        .span(),
        s
    );
    assert_eq!(
        ParseError {
            kind: ParseErrorKind::UnclosedDelimiter {
                open: "(",
                open_span: Span::new(0, 1),
            },
            span: s,
            context: None,
        }
        .span(),
        s
    );
    assert_eq!(
        ParseError {
            kind: ParseErrorKind::InvalidParenForm,
            span: s,
            context: None,
        }
        .span(),
        s
    );
    assert_eq!(
        ParseError {
            kind: ParseErrorKind::NonAssociativeChain,
            span: s,
            context: None,
        }
        .span(),
        s
    );
}
