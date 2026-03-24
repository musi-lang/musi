use super::*;

#[test]
fn display_unexpected_char() {
    let err = LexError {
        kind: LexErrorKind::UnexpectedChar('#'),
        span: Span::new(0, 1),
    };
    assert_eq!(err.to_string(), "unexpected character '#'");
}

#[test]
fn display_unterminated_string() {
    let err = LexError {
        kind: LexErrorKind::UnterminatedString,
        span: Span::new(0, 5),
    };
    assert_eq!(err.to_string(), "unterminated string literal");
}

#[test]
fn display_unterminated_fstring() {
    let err = LexError {
        kind: LexErrorKind::UnterminatedFString,
        span: Span::new(0, 5),
    };
    assert_eq!(err.to_string(), "unterminated f-string literal");
}

#[test]
fn display_unterminated_fstring_expr() {
    let err = LexError {
        kind: LexErrorKind::UnterminatedFStringExpr,
        span: Span::new(0, 5),
    };
    assert_eq!(err.to_string(), "unterminated f-string interpolation");
}

#[test]
fn display_unterminated_rune() {
    let err = LexError {
        kind: LexErrorKind::UnterminatedRune,
        span: Span::new(0, 2),
    };
    assert_eq!(err.to_string(), "unterminated rune literal");
}

#[test]
fn display_unterminated_escaped_ident() {
    let err = LexError {
        kind: LexErrorKind::UnterminatedEscapedIdent,
        span: Span::new(0, 3),
    };
    assert_eq!(err.to_string(), "unterminated escaped identifier");
}

#[test]
fn display_unterminated_block_comment() {
    let err = LexError {
        kind: LexErrorKind::UnterminatedBlockComment,
        span: Span::new(0, 4),
    };
    assert_eq!(err.to_string(), "unterminated block comment");
}

#[test]
fn display_empty_rune() {
    let err = LexError {
        kind: LexErrorKind::EmptyRune,
        span: Span::new(0, 2),
    };
    assert_eq!(err.to_string(), "empty rune literal");
}

#[test]
fn display_multi_char_rune() {
    let err = LexError {
        kind: LexErrorKind::MultiCharRune,
        span: Span::new(0, 4),
    };
    assert_eq!(
        err.to_string(),
        "rune literal contains more than one character"
    );
}

#[test]
fn display_invalid_escape() {
    let err = LexError {
        kind: LexErrorKind::InvalidEscape('z'),
        span: Span::new(1, 3),
    };
    assert_eq!(err.to_string(), "invalid escape sequence '\\z'");
}

#[test]
fn display_invalid_hex_escape() {
    let err = LexError {
        kind: LexErrorKind::InvalidHexEscape { expected: 2 },
        span: Span::new(0, 4),
    };
    assert_eq!(err.to_string(), "invalid hex escape: expected 2 hex digits");
}

#[test]
fn display_invalid_unicode_escape() {
    let err = LexError {
        kind: LexErrorKind::InvalidUnicodeEscape,
        span: Span::new(0, 6),
    };
    assert_eq!(err.to_string(), "invalid unicode escape");
}

#[test]
fn display_invalid_number_prefix() {
    let err = LexError {
        kind: LexErrorKind::InvalidNumberPrefix,
        span: Span::new(0, 2),
    };
    assert_eq!(
        err.to_string(),
        "invalid number literal: expected digits after base prefix"
    );
}

#[test]
fn display_number_overflow() {
    let err = LexError {
        kind: LexErrorKind::NumberOverflow,
        span: Span::new(0, 20),
    };
    assert_eq!(err.to_string(), "number literal overflow");
}

#[test]
fn span_returns_correct_span_for_each_variant() {
    let span = Span::new(5, 10);

    let variants: Vec<LexError> = vec![
        LexError {
            kind: LexErrorKind::UnexpectedChar('x'),
            span,
        },
        LexError {
            kind: LexErrorKind::UnterminatedString,
            span,
        },
        LexError {
            kind: LexErrorKind::UnterminatedFString,
            span,
        },
        LexError {
            kind: LexErrorKind::UnterminatedFStringExpr,
            span,
        },
        LexError {
            kind: LexErrorKind::UnterminatedRune,
            span,
        },
        LexError {
            kind: LexErrorKind::UnterminatedEscapedIdent,
            span,
        },
        LexError {
            kind: LexErrorKind::UnterminatedBlockComment,
            span,
        },
        LexError {
            kind: LexErrorKind::EmptyRune,
            span,
        },
        LexError {
            kind: LexErrorKind::MultiCharRune,
            span,
        },
        LexError {
            kind: LexErrorKind::InvalidEscape('z'),
            span,
        },
        LexError {
            kind: LexErrorKind::InvalidHexEscape { expected: 2 },
            span,
        },
        LexError {
            kind: LexErrorKind::InvalidUnicodeEscape,
            span,
        },
        LexError {
            kind: LexErrorKind::InvalidNumberPrefix,
            span,
        },
        LexError {
            kind: LexErrorKind::NumberOverflow,
            span,
        },
    ];

    for variant in &variants {
        assert_eq!(variant.span(), span, "failed for {variant}");
    }
}
