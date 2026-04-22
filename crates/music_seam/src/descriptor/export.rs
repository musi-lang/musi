use music_arena::Idx;

use crate::artifact::StringId;
use crate::descriptor::{
    EffectDescriptor, ForeignDescriptor, GlobalDescriptor, ProcedureDescriptor, ShapeDescriptor,
    TypeDescriptor,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportTarget {
    Procedure(Idx<ProcedureDescriptor>),
    Global(Idx<GlobalDescriptor>),
    Foreign(Idx<ForeignDescriptor>),
    Type(Idx<TypeDescriptor>),
    Effect(Idx<EffectDescriptor>),
    Shape(Idx<ShapeDescriptor>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportDescriptor {
    pub name: StringId,
    pub opaque: bool,
    pub target: ExportTarget,
}

impl ExportDescriptor {
    #[must_use]
    pub const fn new(name: StringId, opaque: bool, target: ExportTarget) -> Self {
        Self {
            name,
            opaque,
            target,
        }
    }
}
