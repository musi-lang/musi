//! Error types for the Musi bytecode format and code generator.

use core::fmt;

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

#[derive(Debug)]
pub enum CodegenError {
    TooManyFunctions,
    TooManyConstants,
    TooManySymbols,
    ParameterCountOverflow,
    UnknownFunction(Box<str>),
    UnsupportedExpr,
    UndefinedVariable(Box<str>),
    TooManyLocals,
    JumpOffsetOverflow,
    UnknownType(Box<str>),
    UnknownVariant(Box<str>),
    UnknownField(Box<str>),
}

impl fmt::Display for CodegenError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::TooManyFunctions => write!(f, "too many functions (limit u16::MAX)"),
            Self::TooManyConstants => write!(f, "too many constants (limit u16::MAX)"),
            Self::TooManySymbols => write!(f, "too many symbols (limit u16::MAX)"),
            Self::ParameterCountOverflow => write!(f, "function has more than u8::MAX parameters"),
            Self::UnknownFunction(name) => write!(f, "unknown function '{name}'"),
            Self::UnsupportedExpr => write!(f, "expression kind is not supported"),
            Self::UndefinedVariable(name) => write!(f, "undefined variable '{name}'"),
            Self::TooManyLocals => write!(f, "too many local variables (limit u16::MAX)"),
            Self::JumpOffsetOverflow => write!(f, "jump offset overflowed i32 range"),
            Self::UnknownType(name) => write!(f, "unknown type '{name}'"),
            Self::UnknownVariant(name) => write!(f, "unknown variant '{name}'"),
            Self::UnknownField(name) => write!(f, "unknown field '{name}'"),
        }
    }
}

impl std::error::Error for CodegenError {}
