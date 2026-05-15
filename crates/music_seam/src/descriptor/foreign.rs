use crate::artifact::{StringId, TypeId};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ForeignDescriptor {
    pub name: StringId,
    pub param_tys: Box<[TypeId]>,
    pub result_ty: TypeId,
    pub abi: StringId,
    pub symbol: StringId,
    pub link: Option<StringId>,
    pub domain: Option<StringId>,
    pub pinned_params: Box<[u16]>,
    pub nullable_params: Box<[u16]>,
    pub behavior: ForeignBehavior,
    pub lifetime: Option<StringId>,
    pub cold: bool,
}

#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct ForeignBehavior {
    pub nullable_result: bool,
    pub export: bool,
    pub hot: bool,
}

impl ForeignDescriptor {
    #[must_use]
    pub fn new(
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
            domain: None,
            pinned_params: Box::new([]),
            nullable_params: Box::new([]),
            behavior: ForeignBehavior::default(),
            lifetime: None,
            cold: false,
        }
    }

    #[must_use]
    pub const fn with_link(mut self, link: StringId) -> Self {
        self.link = Some(link);
        self
    }

    #[must_use]
    pub const fn with_domain(mut self, domain: StringId) -> Self {
        self.domain = Some(domain);
        self
    }

    #[must_use]
    pub fn with_pinned_params(mut self, pinned_params: Box<[u16]>) -> Self {
        self.pinned_params = pinned_params;
        self
    }

    #[must_use]
    pub fn with_nullable_params(mut self, nullable_params: Box<[u16]>) -> Self {
        self.nullable_params = nullable_params;
        self
    }

    #[must_use]
    pub const fn with_nullable_result(mut self, nullable_result: bool) -> Self {
        self.behavior.nullable_result = nullable_result;
        self
    }

    #[must_use]
    pub const fn with_lifetime(mut self, lifetime: StringId) -> Self {
        self.lifetime = Some(lifetime);
        self
    }

    #[must_use]
    pub const fn with_export(mut self, export: bool) -> Self {
        self.behavior.export = export;
        self
    }

    #[must_use]
    pub const fn with_hot(mut self, hot: bool) -> Self {
        self.behavior.hot = hot;
        self
    }

    #[must_use]
    pub const fn with_cold(mut self, cold: bool) -> Self {
        self.cold = cold;
        self
    }
}
