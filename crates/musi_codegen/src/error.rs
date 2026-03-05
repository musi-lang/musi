//! Deserialization errors for the `.mso` binary format.

use thiserror::Error;

/// Errors that can occur when deserializing a `.mso` binary module.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum DeserError {
    /// The input ended before all expected data was read.
    #[error("unexpected end of input")]
    UnexpectedEof,

    /// The magic bytes do not match `b\"MUSI\"`.
    #[error("invalid magic bytes (expected b\"MUSI\")")]
    InvalidMagic,

    /// The format version is not supported (only version 1 is).
    #[error("unsupported format version {0}; expected 1")]
    UnsupportedVersion(u16),

    /// An unknown tag was encountered in the const pool.
    #[error("unknown const-pool tag {0:#04x}")]
    UnknownConstTag(u8),

    /// An unknown opcode tag was encountered.
    #[error("unknown opcode {tag:#04x} at offset {offset}")]
    UnknownOpcode {
        /// The opcode byte.
        tag: u8,
        /// Byte offset of the bad opcode in the code slice.
        offset: usize,
    },

    /// A symbol name contained invalid UTF-8.
    #[error("symbol name is not valid UTF-8")]
    InvalidUtf8,
}
