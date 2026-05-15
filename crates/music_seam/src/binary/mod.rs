use crate::descriptor::{
    BlockSignatureDescriptor, ClosureDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor,
    DataFieldDescriptor, DataVariantDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor,
    GlobalDescriptor, ImportDescriptor, ManifestDescriptor, MetaDescriptor, ObjectHeaderDescriptor,
    ProcedureCallingConvention, ProcedureDescriptor, ProcedureVisibility, RootMapDescriptor,
    SafePointKind, ShapeDescriptor, StackEffectDescriptor, TypeDescriptor,
};
use crate::{
    Artifact, BINARY_MAJOR_VERSION, BINARY_MINOR_VERSION, CodeEntry, Instruction, Label, Opcode,
    Operand, SEAM_MAGIC, SectionTag,
};
use crate::{AssemblyError, AssemblyResult};
use music_arena::Idx;
use music_term::SyntaxShape;

mod decode;
mod encode;
mod io;

pub use decode::{decode_binary, validate_binary};
pub use encode::encode_binary;
use io::*;
