use music_arena::Idx;

use crate::artifact::StringRecord;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpDescriptor {
    pub name: Idx<StringRecord>,
    pub params: u16,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectDescriptor {
    pub name: Idx<StringRecord>,
    pub ops: Box<[EffectOpDescriptor]>,
}
