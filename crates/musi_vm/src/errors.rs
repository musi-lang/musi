use music_il::opcode::Opcode;

#[derive(Debug, thiserror::Error)]
pub enum LoadError {
    #[error("invalid magic bytes")]
    InvalidMagic,
    #[error("unsupported version {major}.{minor}")]
    UnsupportedVersion { major: u8, minor: u8 },
    #[error("truncated header")]
    TruncatedHeader,
    #[error("truncated section")]
    TruncatedSection,
    #[error("invalid opcode 0x{byte:02x} at offset {offset}")]
    InvalidOpcode { byte: u8, offset: usize },
    #[error("invalid constant tag 0x{tag:02x}")]
    InvalidConstantTag { tag: u8 },
}

#[derive(Debug, thiserror::Error)]
pub enum VmError {
    #[error("no entry point (main method) found")]
    NoEntryPoint,
    #[error("stack underflow")]
    StackUnderflow,
    #[error("invalid local slot {0}")]
    InvalidLocal(usize),
    #[error("invalid constant pool index {0}")]
    InvalidConstant(usize),
    #[error("pc out of bounds")]
    PcOutOfBounds,
    #[error("division by zero")]
    DivisionByZero,
    #[error("type error: expected {expected}, found {found}")]
    TypeError {
        expected: &'static str,
        found: &'static str,
    },
    #[error("invalid opcode 0x{0:02x}")]
    InvalidOpcode(u8),
    #[error("unsupported opcode '{0:?}'")]
    UnsupportedOpcode(Opcode),
    #[error("explicit panic")]
    ExplicitPanic,
    #[error("invalid global slot {0}")]
    InvalidGlobal(usize),
    #[error("not callable")]
    NotCallable,
    #[error("invalid upvalue slot {0}")]
    InvalidUpvalue(usize),
    #[error("no closure context for upvalue access")]
    NoClosureContext,
    #[error("call stack overflow")]
    CallStackOverflow,
    #[error("invalid method index {0}")]
    InvalidMethod(usize),
    #[error("no effect handler installed")]
    NoEffectHandler,
    #[error("array index {index} out of bounds (length {length})")]
    IndexOutOfBounds { index: usize, length: usize },
    #[error("expected array")]
    NotAnArray,
}
