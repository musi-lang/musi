//! Runtime errors for the Musi VM.

use musi_codegen::DeserError;
use thiserror::Error;

/// Errors that can occur during VM execution.
#[derive(Debug, Error)]
pub enum VmError {
    /// The operand stack was empty when a value was expected.
    #[error("stack underflow")]
    StackUnderflow,

    /// The call stack was empty when a frame was expected.
    #[error("no active call frame")]
    NoFrames,

    /// A function index exceeded the function table.
    #[error("function index {0} out of bounds")]
    FunctionOutOfBounds(u16),

    /// A symbol index exceeded the symbol table.
    #[error("symbol index {0} out of bounds")]
    SymbolOutOfBounds(u16),

    /// A const-pool index exceeded the const pool.
    #[error("const-pool index {0} out of bounds")]
    ConstOutOfBounds(u16),

    /// A local-variable slot index exceeded the locals array.
    #[error("local variable index {0} out of bounds")]
    LocalOutOfBounds(u16),

    /// The function's code slice fell outside the code section.
    #[error("function code region is outside the code section")]
    CodeOutOfBounds,

    /// An intrinsic ID had no registered handler.
    #[error("no handler registered for intrinsic {0}")]
    UnknownIntrinsic(u16),

    /// An opcode could not be decoded.
    #[error("decode error: {0}")]
    Decode(#[from] DeserError),

    /// Division or remainder by zero.
    #[error("division by zero")]
    DivisionByZero,

    /// An operand on the stack had an unexpected type.
    #[error("type mismatch")]
    TypeMismatch,

    /// A match expression had no matching arm.
    #[error("non-exhaustive match")]
    MatchFailure,

    /// `CallDynamic` was given a non-Function value.
    #[error("value is not a function")]
    NotAFunction,

    /// A field index exceeded the object's field count.
    #[error("field index {0} out of bounds")]
    FieldOutOfBounds(u16),

    /// No method implementation found for the given name and receiver type.
    #[error("no method implementation found")]
    MethodNotFound,

    /// An extrin function call (FFI) failed.
    #[error("FFI error: {0}")]
    FfiFailed(Box<str>),

    /// Array index was out of bounds.
    #[error("index {index} out of bounds for array of length {len}")]
    IndexOutOfBounds { index: i64, len: usize },

    /// An `assert` or `assert_msg` call failed.
    #[error("assertion failed: {0}")]
    AssertionFailed(Box<str>),

    /// A value that cannot be used as a `HashMap` key.
    #[error("value is not hashable (only Int, String can be map keys)")]
    UnhashableKey,
}
