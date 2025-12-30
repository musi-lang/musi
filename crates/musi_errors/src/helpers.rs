use crate::{code::ErrorCode, diagnostic::Diagnostic};
use musi_basic::span::Span;

pub fn unknown_char(c: char, span: Span) -> Diagnostic {
    Diagnostic::error(format!("unknown character '{c}'"), span).with_code(ErrorCode::E1001)
}

pub fn unclosed_string(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed string literal", span).with_code(ErrorCode::E1002)
}

pub fn unclosed_template(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed template literal", span).with_code(ErrorCode::E1003)
}

pub fn unclosed_escaped_ident(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed escaped identifier", span).with_code(ErrorCode::E1004)
}

pub fn unclosed_rune(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed rune literal", span).with_code(ErrorCode::E1005)
}

pub fn unclosed_block_comment(span: Span) -> Diagnostic {
    Diagnostic::error("unclosed block comment", span).with_code(ErrorCode::E1006)
}

pub fn invalid_ident(span: Span) -> Diagnostic {
    Diagnostic::error("invalid identifier", span).with_code(ErrorCode::E1007)
}

pub fn invalid_rune(span: Span) -> Diagnostic {
    Diagnostic::error("invalid rune literal", span).with_code(ErrorCode::E1008)
}

pub fn malformed_numeric(span: Span) -> Diagnostic {
    Diagnostic::error("malformed numeric literal", span).with_code(ErrorCode::E1009)
}

pub fn malformed_underscore(what: &str, span: Span) -> Diagnostic {
    Diagnostic::error(format!("malformed '_' in {what} literal"), span)
        .with_code(ErrorCode::E1010)
        .with_note("underscores must separate digits", span)
}

pub fn unknown_escape(seq: String, span: Span) -> Diagnostic {
    Diagnostic::error(format!("unknown escape sequence '\\{seq}'"), span)
        .with_code(ErrorCode::E1011)
}
