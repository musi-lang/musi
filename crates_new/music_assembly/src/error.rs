#[derive(Debug, thiserror::Error)]
pub enum CodecError {
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
    #[error("invalid string offset {offset}")]
    InvalidStringOffset { offset: u32 },
    #[error("invalid method name reference {reference}")]
    InvalidMethodName { reference: u32 },
    #[error("invalid mnemonic `{mnemonic}`")]
    InvalidMnemonic { mnemonic: String },
    #[error("invalid operand for `{mnemonic}`")]
    InvalidOperand { mnemonic: String },
    #[error("module too large to encode")]
    ModuleTooLarge,
}
