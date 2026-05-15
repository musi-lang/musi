use crate::artifact::{StringId, TypeId};

pub type StackEffectTypeIdList = Box<[TypeId]>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct StackEffectDescriptor {
    pub name: StringId,
    pub input_tys: StackEffectTypeIdList,
    pub output_tys: StackEffectTypeIdList,
}

impl StackEffectDescriptor {
    #[must_use]
    pub const fn new(
        name: StringId,
        input_tys: StackEffectTypeIdList,
        output_tys: StackEffectTypeIdList,
    ) -> Self {
        Self {
            name,
            input_tys,
            output_tys,
        }
    }

    #[must_use]
    pub fn input_top(&self) -> Option<TypeId> {
        self.input_tys.last().copied()
    }

    #[must_use]
    pub fn output_top(&self) -> Option<TypeId> {
        self.output_tys.last().copied()
    }
}
