mod artifact;
mod binary;
pub mod descriptor;
mod error;
mod instruction;
mod opcode;
mod text;
mod types;

pub use artifact::{
    Artifact, ArtifactError, BINARY_VERSION, ClassId, ConstantId, DataId, EffectId, ExportId,
    ForeignId, GlobalId, MetaId, MethodId, SEAM_MAGIC, SectionTag, StringId, StringRecord, Table,
    TypeId,
};
pub use binary::{decode_binary, encode_binary, validate_binary};
pub use error::AssemblyError;
pub use instruction::{CodeEntry, Instruction, Label, LabelId, Operand, OperandShape};
pub use opcode::{Opcode, OpcodeFamily};
pub use text::{format_text, parse_text, validate_text};
pub use types::AssemblyResult;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod assembly_tests;
