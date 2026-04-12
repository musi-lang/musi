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

impl ForeignDescriptor {
    #[must_use]
    pub const fn new(
        name: StringId,
        param_tys: Box<[TypeId]>,
        result_ty: TypeId,
        abi: StringId,
        symbol: StringId,
    ) -> Self {
        Self {
            name,
            param_tys,
            result_ty,
            abi,
            symbol,
            link: None,
            export: false,
        }
    }

    #[must_use]
    pub const fn with_link(mut self, link: StringId) -> Self {
        self.link = Some(link);
        self
    }

    #[must_use]
    pub const fn with_export(mut self, export: bool) -> Self {
        self.export = export;
        self
    }
}
