//! Structured lexer errors.

use msc_shared::{IntoDiagnostic, Severity};

#[derive(Debug, Clone, Copy, thiserror::Error)]
pub enum LexError {
    #[error("unexpected character `{0}`")]
    UnexpectedCharacter(char),
    #[error("unterminated string literal")]
    UnterminatedString,
    #[error("unterminated backtick-quoted identifier")]
    UnterminatedEscapedIdent,
    #[error("unterminated rune literal")]
    UnterminatedRuneLit,
    #[error("unterminated interpolated string literal")]
    UnterminatedFString,
    #[error("unclosed block comment")]
    UnclosedBlockComment,
}

impl IntoDiagnostic for LexError {
    fn severity(&self) -> Severity {
        Severity::Error
    }
}
