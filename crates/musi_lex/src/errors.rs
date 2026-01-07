use musi_core::{Diagnostic, ErrorCode, Span};

#[must_use]
pub fn unknown_char(c: char, span: Span) -> Diagnostic {
    Diagnostic::error(format!("unknown character '{c}'"), span).with_code(ErrorCode::E1001)
}

#[must_use]
pub fn unclosed_string(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed string literal", span).with_code(ErrorCode::E1002)
}

#[must_use]
pub fn unclosed_template(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed template literal", span).with_code(ErrorCode::E1003)
}

#[must_use]
pub fn unclosed_escaped_ident(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed escaped identifier", span).with_code(ErrorCode::E1004)
}

#[must_use]
pub fn unclosed_rune(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed rune literal", span).with_code(ErrorCode::E1005)
}

#[must_use]
pub fn unclosed_block_comment(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed block comment", span).with_code(ErrorCode::E1006)
}

#[must_use]
pub fn invalid_rune(span: Span) -> Diagnostic {
    Diagnostic::error("invalid rune literal", span).with_code(ErrorCode::E1008)
}

#[must_use]
pub fn unknown_escape(seq: char, span: Span) -> Diagnostic {
    Diagnostic::error(format!("unknown escape sequence '\\{seq}'"), span)
        .with_code(ErrorCode::E1011)
}
