use std::io;
use thiserror::Error;

pub type MusiResult<T> = Result<T, MusiError>;

impl From<io::Error> for MusiError {
    fn from(err: io::Error) -> Self {
        MusiError::IoError(err.to_string())
    }
}

#[derive(Error, Debug, Clone, PartialEq)]
pub enum MusiError {
    #[error("attempted to pop from empty stack")]
    StackUnderflow,

    #[error("exceeded maximum stack size of {0}")]
    StackOverflow(usize),

    #[error("memory allocation failed ({reason})")]
    AllocationFailed { reason: String },

    #[error("reference counting failed ({0})")]
    RefCountError(String),

    #[error("type mismatch")]
    TypeMismatch,

    #[error("invalid bytecode ({0})")]
    InvalidBytecode(String),

    #[error("unknown opcode 0x{opcode:02x} at offset {offset}")]
    UnknownOpcode { opcode: u8, offset: usize },

    #[error("unexpected end of bytecode at offset {0}")]
    UnexpectedEndOfBytecode(usize),

    #[error("index {idx} out of bounds for length {len}")]
    IndexOutOfBounds { idx: usize, len: usize },

    #[error("array index {idx} out of bounds for size {size}")]
    ArrayBounds { idx: isize, size: usize },

    #[error("integer overflow in {op}")]
    IntegerOverflow { op: String },

    #[error("division by zero")]
    DivisionByZero,

    #[error("modulo by zero")]
    ModuloByZero,

    #[error("builtin procedure '{0}' not found")]
    BuiltinNotFound(String),

    #[error("module '{0}' not found")]
    ModuleNotFound(String),

    #[error("procedure '{0}' not found")]
    ProcedureNotFound(String),

    #[error("call depth exceeded maximum of {0}")]
    CallDepthExceeded(usize),

    #[error("{0}")]
    IoError(String),

    #[error("external call to '{callee}' failed ({reason})")]
    ExternalCallError { callee: String, reason: String },

    #[error("invalid branch offset {0}")]
    InvalidBranchOffset(i32),

    #[error("unhandled exception ({0})")]
    UnhandledException(String),

    #[error("rethrow without active exception")]
    RethrowWithoutException,

    #[error("invalid VM state ({0})")]
    InvalidVmState(String),
}

pub trait IntoMusiError<T> {
    fn into_musi_error(self) -> MusiResult<T>;
}

impl<T> IntoMusiError<T> for Result<T, std::io::Error> {
    fn into_musi_error(self) -> MusiResult<T> {
        self.map_err(|why| MusiError::IoError(why.to_string()))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_error_display() {
        let err = MusiError::DivisionByZero;
        assert_eq!(err.to_string(), "division by zero");

        let err = MusiError::IndexOutOfBounds { idx: 5, len: 3 };
        assert!(err.to_string().contains("index 5"));
        assert!(err.to_string().contains("length 3"));
    }
}
