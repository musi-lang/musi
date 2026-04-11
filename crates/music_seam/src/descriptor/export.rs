use music_arena::Idx;

use crate::artifact::StringId;
use crate::descriptor::{
    ClassDescriptor, EffectDescriptor, ForeignDescriptor, GlobalDescriptor, MethodDescriptor,
    TypeDescriptor,
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExportTarget {
    Method(Idx<MethodDescriptor>),
    Global(Idx<GlobalDescriptor>),
    Foreign(Idx<ForeignDescriptor>),
    Type(Idx<TypeDescriptor>),
    Effect(Idx<EffectDescriptor>),
    Class(Idx<ClassDescriptor>),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportDescriptor {
    pub name: StringId,
    pub opaque: bool,
    pub target: ExportTarget,
}
