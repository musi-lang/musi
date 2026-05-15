use std::collections::HashMap;

use crate::descriptor::{
    BlockSignatureDescriptor, ClosureDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor,
    DataFieldDescriptor, DataVariantDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor,
    GlobalDescriptor, ImportDescriptor, ManifestDescriptor, MetaDescriptor, ObjectHeaderDescriptor,
    ProcedureCallingConvention, ProcedureDescriptor, ProcedureVisibility, RootMapDescriptor,
    SafePointKind, ShapeDescriptor, StackEffectDescriptor, TypeDescriptor,
};
use crate::{
    Artifact, AssemblyError, AssemblyResult, ClosureId, CodeEntry, ConstantId, DataId, ExportId,
    ForeignId, GlobalId, Instruction, Label, Opcode, Operand, OperandShape, ProcedureId, ShapeId,
    StackEffectId, StringId, TypeId,
};
use music_arena::Idx;
use music_term::SyntaxShape;

mod builder;
mod format;
mod parse;

pub use format::{format_debug_hil, format_decomp, format_disasm};
pub use parse::{parse_disasm, validate_disasm};

type LabelIdMap = HashMap<String, u16>;

#[derive(Default)]
struct TextBuilder {
    artifact: Artifact,
    closures: HashMap<String, ClosureId>,
    types: HashMap<String, TypeId>,
    constants: HashMap<String, ConstantId>,
    globals: HashMap<String, GlobalId>,
    procedures: HashMap<String, ProcedureId>,
    shapes: HashMap<String, ShapeId>,
    foreigns: HashMap<String, ForeignId>,
    exports: HashMap<String, ExportId>,
    data: HashMap<String, DataId>,
    stack_effects: HashMap<String, StackEffectId>,
    strings: HashMap<String, StringId>,
}
