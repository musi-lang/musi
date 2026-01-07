use musi_core::{MusiError, Span, TokenKind};

#[must_use]
pub fn expected_ident(span: Span) -> MusiError {
    MusiError::new("expected identifier", span)
}

#[must_use]
pub fn expected_expr(span: Span) -> MusiError {
    MusiError::new("expected expression", span)
}

#[must_use]
pub fn expected_typ(span: Span) -> MusiError {
    MusiError::new("expected type", span)
}

#[must_use]
pub fn expected_pat(span: Span) -> MusiError {
    MusiError::new("expected pattern", span)
}

#[must_use]
pub fn expected_lit(span: Span) -> MusiError {
    MusiError::new("expected literal", span)
}

#[must_use]
pub fn invalid_literal(span: Span) -> MusiError {
    MusiError::new("invalid literal", span)
}

#[must_use]
pub fn expected_string_lit(span: Span) -> MusiError {
    MusiError::new("expected string literal", span)
}

#[must_use]
pub fn expected_token(expected: TokenKind, span: Span) -> MusiError {
    MusiError::new(format!("expected '{expected}'"), span)
}

#[must_use]
pub fn expected_separator(sep: TokenKind, span: Span) -> MusiError {
    MusiError::new(format!("expected '{sep}' separator"), span)
}

#[must_use]
pub fn unexpected_token(tok: TokenKind, span: Span) -> MusiError {
    MusiError::new(format!("unexpected '{tok}'"), span)
}

#[must_use]
pub fn unexpected_eof(span: Span) -> MusiError {
    MusiError::new("unexpected end of file", span)
}

#[must_use]
pub fn unclosed_delimiter(delim: TokenKind, span: Span) -> MusiError {
    MusiError::new(format!("unclosed '{delim}' delimiter"), span)
}

#[must_use]
pub fn unclosed_attr(span: Span) -> MusiError {
    MusiError::new("unclosed attribute", span).with_hint("add ']'")
}

#[must_use]
pub fn unclosed_template_expr(span: Span) -> MusiError {
    MusiError::new("unclosed template expression", span).with_hint("add '}'")
}
