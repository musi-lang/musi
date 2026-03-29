mod binary;
mod error;
mod text;
mod validate;

pub use binary::{decode_binary, encode_binary};
pub use error::CodecError;
pub use text::{assemble_method, disassemble_method, format_instruction, parse_instruction};
pub use validate::{validate_binary, validate_module};

pub type AssemblyResult<T> = Result<T, CodecError>;
