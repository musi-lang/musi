mod artifact;
mod binary;
pub mod descriptor;
mod diag;
mod error;
mod hil;
mod instruction;
mod mar;
mod opcode;
mod text;
mod types;

pub use artifact::{
    Artifact, ArtifactError, BINARY_MAJOR_VERSION, BINARY_MINOR_VERSION, BINARY_VERSION,
    BlockSignatureId, ClosureId, ConstantId, DataId, ExportId, ForeignId, GlobalId, MetaId,
    ProcedureId, RootMapId, SEAM_MAGIC, SectionTag, ShapeId, StackEffectId, StringId, StringRecord,
    Table, TypeId,
};
pub use binary::{decode_binary, encode_binary, validate_binary};
pub use diag::SeamDiagKind;
pub use error::AssemblyError;
pub use hil::{
    HilBinaryOp, HilBlock, HilFunction, HilInstruction, HilModule, HilParam, HilShape,
    HilTerminator, HilType, HilValueId, HilVerifyError, HilVerifyResult, format_hil, parse_hil,
};
pub use instruction::{CodeEntry, Instruction, Label, LabelId, Operand, OperandShape};
pub use mar::{
    MAR_BINARY_MAJOR_VERSION, MAR_BINARY_MINOR_VERSION, MAR_BINARY_VERSION, MAR_MAGIC, MarArchive,
    MarError, MarManifest, MarModuleEntry, MarModuleEntryList, MarOptimizationPolicy,
    MarPackageKind, MarProfile, MarResult, decode_mar_archive, encode_mar_archive,
    validate_mar_archive,
};
pub use opcode::{Opcode, OpcodeFamily, OpcodeVisibility};
pub use text::{format_debug_hil, format_decomp, format_disasm, parse_disasm, validate_disasm};
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
