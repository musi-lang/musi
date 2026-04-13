use crate::artifact::{StringId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpDescriptor {
    pub name: StringId,
    pub param_tys: Box<[TypeId]>,
    pub result_ty: TypeId,
}

impl EffectOpDescriptor {
    #[must_use]
    pub const fn new(name: StringId, param_tys: Box<[TypeId]>, result_ty: TypeId) -> Self {
        Self {
            name,
            param_tys,
            result_ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectDescriptor {
    pub name: StringId,
    pub ops: Box<[EffectOpDescriptor]>,
}

impl EffectDescriptor {
    #[must_use]
    pub const fn new(name: StringId, ops: Box<[EffectOpDescriptor]>) -> Self {
        Self { name, ops }
    }
}
