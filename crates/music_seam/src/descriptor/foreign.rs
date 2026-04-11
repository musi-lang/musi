use crate::artifact::{StringId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignDescriptor {
    pub name: StringId,
    pub param_tys: Box<[TypeId]>,
    pub result_ty: TypeId,
    pub abi: StringId,
    pub symbol: StringId,
    pub link: Option<StringId>,
    pub export: bool,
}
