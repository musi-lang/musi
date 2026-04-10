use music_arena::Idx;

use crate::artifact::{StringRecord, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignDescriptor {
    pub name: Idx<StringRecord>,
    pub param_tys: Box<[TypeId]>,
    pub result_ty: TypeId,
    pub abi: Idx<StringRecord>,
    pub symbol: Idx<StringRecord>,
    pub link: Option<Idx<StringRecord>>,
    pub export: bool,
}
