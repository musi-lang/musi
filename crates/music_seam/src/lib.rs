mod artifact;
mod binary;
pub mod descriptor;
mod diag;
mod error;
mod hil;
mod instruction;
mod opcode;
mod text;
mod types;

pub use artifact::{
    Artifact, ArtifactError, BINARY_MAJOR_VERSION, BINARY_MINOR_VERSION, BINARY_VERSION,
    ConstantId, DataId, EffectId, ExportId, ForeignId, GlobalId, MetaId, ProcedureId, SEAM_MAGIC,
    SectionTag, ShapeId, StringId, StringRecord, Table, TypeId,
};
pub use binary::{decode_binary, encode_binary, validate_binary};
pub use diag::SeamDiagKind;
pub use error::AssemblyError;
pub use hil::{
    HilBinaryOp, HilBlock, HilFunction, HilInstruction, HilModule, HilParam, HilShape,
    HilTerminator, HilType, HilValueId, HilVerifyError, HilVerifyResult, format_hil, parse_hil,
};
pub use instruction::{CodeEntry, Instruction, Label, LabelId, Operand, OperandShape};
pub use opcode::{Opcode, OpcodeFamily};
pub use text::{format_hil_projection, format_text, parse_text, validate_text};
pub use types::AssemblyResult;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod assembly_tests;

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod hil_tests;
