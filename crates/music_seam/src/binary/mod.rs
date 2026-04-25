use crate::descriptor::{
    ConstantDescriptor, ConstantValue, DataDescriptor, DataVariantDescriptor, EffectDescriptor,
    EffectOpDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor, GlobalDescriptor,
    MetaDescriptor, ProcedureDescriptor, ShapeDescriptor, TypeDescriptor,
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
