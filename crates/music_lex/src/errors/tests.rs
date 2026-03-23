use super::*;

#[test]
fn display_unexpected_char() {
    let err = LexError::UnexpectedChar {
        ch: '#',
        span: Span::new(0, 1),
    };
    assert_eq!(err.to_string(), "unexpected character '#'");
}

#[test]
fn display_unterminated_string() {
    let err = LexError::UnterminatedString {
        span: Span::new(0, 5),
    };
    assert_eq!(err.to_string(), "unterminated string literal");
}

#[test]
fn display_unterminated_fstring() {
    let err = LexError::UnterminatedFString {
        span: Span::new(0, 5),
    };
    assert_eq!(err.to_string(), "unterminated f-string literal");
}

#[test]
fn display_unterminated_fstring_expr() {
    let err = LexError::UnterminatedFStringExpr {
        span: Span::new(0, 5),
    };
    assert_eq!(err.to_string(), "unterminated f-string interpolation");
}

#[test]
fn display_unterminated_rune() {
    let err = LexError::UnterminatedRune {
        span: Span::new(0, 2),
    };
    assert_eq!(err.to_string(), "unterminated rune literal");
}

#[test]
fn display_unterminated_escaped_ident() {
    let err = LexError::UnterminatedEscapedIdent {
        span: Span::new(0, 3),
    };
    assert_eq!(err.to_string(), "unterminated escaped identifier");
}

#[test]
fn display_unterminated_block_comment() {
    let err = LexError::UnterminatedBlockComment {
        span: Span::new(0, 4),
    };
    assert_eq!(err.to_string(), "unterminated block comment");
}

#[test]
fn display_empty_rune() {
    let err = LexError::EmptyRune {
        span: Span::new(0, 2),
    };
    assert_eq!(err.to_string(), "empty rune literal");
}

#[test]
fn display_multi_char_rune() {
    let err = LexError::MultiCharRune {
        span: Span::new(0, 4),
    };
    assert_eq!(
        err.to_string(),
        "rune literal contains more than one character"
    );
}

#[test]
fn display_invalid_escape() {
    let err = LexError::InvalidEscape {
        ch: 'z',
        span: Span::new(1, 3),
    };
    assert_eq!(err.to_string(), "invalid escape sequence '\\z'");
}

#[test]
fn display_invalid_hex_escape() {
    let err = LexError::InvalidHexEscape {
        expected: 2,
        span: Span::new(0, 4),
    };
    assert_eq!(err.to_string(), "invalid hex escape: expected 2 hex digits");
}

#[test]
fn display_invalid_unicode_escape() {
    let err = LexError::InvalidUnicodeEscape {
        span: Span::new(0, 6),
    };
    assert_eq!(err.to_string(), "invalid unicode escape");
}

#[test]
fn display_invalid_number_prefix() {
    let err = LexError::InvalidNumberPrefix {
        span: Span::new(0, 2),
    };
    assert_eq!(
        err.to_string(),
        "invalid number literal: expected digits after base prefix"
    );
}

#[test]
fn display_number_overflow() {
    let err = LexError::NumberOverflow {
        span: Span::new(0, 20),
    };
    assert_eq!(err.to_string(), "number literal overflow");
}

#[test]
fn span_returns_correct_span_for_each_variant() {
    let span = Span::new(5, 10);

    let variants: Vec<LexError> = vec![
        LexError::UnexpectedChar { ch: 'x', span },
        LexError::UnterminatedString { span },
        LexError::UnterminatedFString { span },
        LexError::UnterminatedFStringExpr { span },
        LexError::UnterminatedRune { span },
        LexError::UnterminatedEscapedIdent { span },
        LexError::UnterminatedBlockComment { span },
        LexError::EmptyRune { span },
        LexError::MultiCharRune { span },
        LexError::InvalidEscape { ch: 'z', span },
        LexError::InvalidHexEscape { expected: 2, span },
        LexError::InvalidUnicodeEscape { span },
        LexError::InvalidNumberPrefix { span },
        LexError::NumberOverflow { span },
    ];

    for variant in &variants {
        assert_eq!(variant.span(), span, "failed for {variant}");
    }
}
