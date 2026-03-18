//! Bytecode emission errors.

/// Errors that can occur during bytecode emission.
#[derive(Debug, thiserror::Error)]
pub enum EmitError {
    #[error("too many constants (max 65535)")]
    TooManyConsts,
    #[error("too many types (max 65535)")]
    TooManyTypes,
    #[error("function body too large (max code len u32)")]
    FunctionTooLarge,
    #[error("jump offset out of range")]
    JumpTooFar,
    #[error("unresolvable type `{desc}`")]
    UnresolvableType { desc: Box<str> },
    #[error("unresolvable label in function `{name}`")]
    UnresolvableLabel { name: Box<str> },
    #[error("operand overflow `{desc}`")]
    OperandOverflow { desc: Box<str> },
    #[error("unsupported feature `{desc}`")]
    UnsupportedFeature { desc: Box<str> },
    #[error("field not found; {desc}")]
    FieldNotFound { desc: Box<str> },
    #[error("missing type info for {desc}")]
    NoTypeInfo { desc: Box<str> },
    #[error("too many strings (max 65535)")]
    TooManyStrings,
    #[error("too many globals (max 65535)")]
    TooManyGlobals,
    #[error("section overflow")]
    SectionOverflow,
}

impl EmitError {
    pub fn unresolvable(desc: impl Into<Box<str>>) -> Self {
        Self::UnresolvableType { desc: desc.into() }
    }

    pub fn overflow(desc: impl Into<Box<str>>) -> Self {
        Self::OperandOverflow { desc: desc.into() }
    }
}
