//! Error types for the Musi bytecode format and code generator.

use thiserror::Error;

#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum DeserError {
    #[error("unexpected end of input")]
    UnexpectedEof,

    #[error("invalid magic bytes (expected b\"MUSI\")")]
    InvalidMagic,

    #[error("unsupported format version {0}; expected 1")]
    UnsupportedVersion(u16),

    #[error("unknown const-pool tag {0:#04x}")]
    UnknownConstTag(u8),

    #[error("unknown opcode {tag:#04x} at offset {offset}")]
    UnknownOpcode { tag: u8, offset: usize },

    #[error("symbol name is not valid UTF-8")]
    InvalidUtf8,
}

#[derive(Debug, thiserror::Error)]
pub enum CodegenError {
    #[error("too many functions (limit u16::MAX)")]
    TooManyFunctions,
    #[error("too many constants (limit u16::MAX)")]
    TooManyConstants,
    #[error("too many symbols (limit u16::MAX)")]
    TooManySymbols,
    #[error("function has more than u8::MAX parameters")]
    ParameterCountOverflow,
    #[error("unknown function '{0}'")]
    UnknownFunction(Box<str>),
    #[error("expression kind is not supported")]
    UnsupportedExpr,
    #[error("undefined variable '{0}'")]
    UndefinedVariable(Box<str>),
    #[error("too many local variables (limit u16::MAX)")]
    TooManyLocals,
    #[error("jump offset overflowed i32 range")]
    JumpOffsetOverflow,
    #[error("unknown type '{0}'")]
    UnknownType(Box<str>),
    #[error("unknown variant '{0}'")]
    UnknownVariant(Box<str>),
    #[error("unknown field '{0}'")]
    UnknownField(Box<str>),
}
