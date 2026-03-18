//! VM runtime errors.

/// All errors that can occur during loading, verification, or execution.
#[derive(Debug, thiserror::Error)]
pub enum VmError {
    #[error("bad magic bytes - not a .muse file")]
    BadMagic,
    #[error("checksum mismatch - file may be corrupted")]
    BadChecksum,
    #[error("malformed bytecode, {desc}")]
    Malformed { desc: Box<str> },
    #[error("verification failed, {desc}")]
    Verify { desc: Box<str> },
    #[error("division by zero")]
    DivideByZero,
    #[error("call stack overflow")]
    StackOverflow,
    #[error("expected {expected} but found {found}")]
    TypeError {
        expected: &'static str,
        found: &'static str,
    },
    #[error("index `{index}` out of bounds, length `{len}`")]
    OutOfBounds { index: usize, len: usize },
    #[error("no handler for effect `{effect_id}`")]
    NoHandler { effect_id: u8 },
    #[error("effect aborted")]
    EffectAborted,
    #[error("attempted to resume fatal effect op `{op_id}`")]
    FatalEffectResumed { op_id: u32 },
    #[error("unimplemented {desc}")]
    Unimplemented { desc: &'static str },
    #[error("in fn `{fn_id}` at offset `{ip}`, {source}")]
    Runtime {
        fn_id: u32,
        ip: usize,
        source: Box<Self>,
    },
    #[error("halted")]
    Halted,
    #[error("instruction limit exceeded, `{limit}`")]
    InstructionLimitExceeded { limit: u64 },
    #[error("freed heap object at index `{index}`")]
    FreedObject { index: usize },
    #[error("deadlock, no runnable tasks remain")]
    Deadlock,
    #[error("unknown task id `{task_id}`")]
    UnknownTask { task_id: u32 },
    #[error("unknown channel id `{channel_id}`")]
    UnknownChannel { channel_id: u32 },
}

macro_rules! malformed {
    ($lit:literal) => {
        $crate::error::VmError::Malformed { desc: $lit.into() }
    };
    ($fmt:literal, $($arg:tt)*) => {
        $crate::error::VmError::Malformed { desc: format!($fmt, $($arg)*).into_boxed_str() }
    };
}
pub(crate) use malformed;
