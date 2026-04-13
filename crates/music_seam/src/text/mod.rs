use std::collections::HashMap;

use crate::descriptor::{
    ClassDescriptor, ConstantDescriptor, ConstantValue, DataDescriptor, DataVariantDescriptor,
    EffectDescriptor, EffectOpDescriptor, ExportDescriptor, ExportTarget, ForeignDescriptor,
    GlobalDescriptor, MetaDescriptor, MethodDescriptor, TypeDescriptor,
};
use crate::{
    Artifact, AssemblyError, AssemblyResult, ClassId, CodeEntry, ConstantId, DataId, EffectId,
    ExportId, ForeignId, GlobalId, Instruction, Label, MethodId, Opcode, Operand, OperandShape,
    StringId, TypeId,
};
use music_term::SyntaxShape;

mod builder;
mod format;
mod parse;

pub use format::format_text;
pub use parse::{parse_text, validate_text};

type LabelIdMap = HashMap<String, u16>;

#[derive(Default)]
struct TextBuilder {
    artifact: Artifact,
    types: HashMap<String, TypeId>,
    constants: HashMap<String, ConstantId>,
    globals: HashMap<String, GlobalId>,
    methods: HashMap<String, MethodId>,
    effects: HashMap<String, EffectId>,
    classes: HashMap<String, ClassId>,
    foreigns: HashMap<String, ForeignId>,
    exports: HashMap<String, ExportId>,
    data: HashMap<String, DataId>,
    strings: HashMap<String, StringId>,
}
