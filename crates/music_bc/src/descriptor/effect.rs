use music_arena::Idx;

use crate::artifact::{StringRecord, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpDescriptor {
    pub name: Idx<StringRecord>,
    pub param_tys: Box<[TypeId]>,
    pub result_ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectDescriptor {
    pub name: Idx<StringRecord>,
    pub ops: Box<[EffectOpDescriptor]>,
}
