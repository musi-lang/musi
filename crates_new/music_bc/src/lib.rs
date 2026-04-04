mod artifact;
pub mod descriptor;
mod instruction;
mod opcode;

pub use artifact::{
    Artifact, ArtifactError, BINARY_VERSION, ClassId, ConstantId, EffectId, ForeignId, GlobalId,
    MethodId, SEAM_MAGIC, SectionTag, StringId, StringRecord, Table, TypeId,
};
pub use instruction::{CodeEntry, Instruction, Label, LabelId, Operand, OperandShape};
pub use opcode::{Opcode, OpcodeFamily};

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
