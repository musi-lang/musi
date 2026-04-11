use crate::artifact::{StringId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpDescriptor {
    pub name: StringId,
    pub param_tys: Box<[TypeId]>,
    pub result_ty: TypeId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectDescriptor {
    pub name: StringId,
    pub ops: Box<[EffectOpDescriptor]>,
}
