use std::collections::HashMap;

use crate::descriptor::{
    ConstantDescriptor, ConstantValue, DataDescriptor, DataVariantDescriptor, EffectDescriptor,
    EffectOpDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor, GlobalDescriptor,
    MetaDescriptor, ProcedureDescriptor, ShapeDescriptor, TypeDescriptor,
};
use crate::{
    Artifact, AssemblyError, AssemblyResult, CodeEntry, ConstantId, DataId, EffectId, ExportId,
    ForeignId, GlobalId, Instruction, Label, Opcode, Operand, OperandShape, ProcedureId, ShapeId,
    StringId, TypeId,
};
use music_term::SyntaxShape;

mod builder;
mod format;
mod parse;

pub use format::{format_hil_projection, format_text};
pub use parse::{parse_text, validate_text};

type LabelIdMap = HashMap<String, u16>;

#[derive(Default)]
struct TextBuilder {
    artifact: Artifact,
    types: HashMap<String, TypeId>,
    constants: HashMap<String, ConstantId>,
    globals: HashMap<String, GlobalId>,
    procedures: HashMap<String, ProcedureId>,
    effects: HashMap<String, EffectId>,
    shapes: HashMap<String, ShapeId>,
    foreigns: HashMap<String, ForeignId>,
    exports: HashMap<String, ExportId>,
    data: HashMap<String, DataId>,
    strings: HashMap<String, StringId>,
}
