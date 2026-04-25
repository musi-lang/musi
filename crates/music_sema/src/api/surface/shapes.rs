use super::{
    super::{Attr, AttrList, DefinitionKey, NameList, SurfaceTyIdList},
    ConstraintSurface, SurfaceTyId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShapeMemberSurface {
    pub name: Box<str>,
    pub params: Box<[SurfaceTyId]>,
    pub result: SurfaceTyId,
}

impl ShapeMemberSurface {
    #[must_use]
    pub fn new<Name, Params>(name: Name, params: Params, result: SurfaceTyId) -> Self
    where
        Name: Into<Box<str>>,
        Params: Into<SurfaceTyIdList>,
    {
        Self {
            name: name.into(),
            params: params.into(),
            result,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawParamSurface {
    pub name: Box<str>,
    pub ty: SurfaceTyId,
}

impl LawParamSurface {
    #[must_use]
    pub fn new<Name>(name: Name, ty: SurfaceTyId) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            ty,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawSurface {
    pub name: Box<str>,
    pub params: Box<[LawParamSurface]>,
}

impl LawSurface {
    #[must_use]
    pub fn new<Name, Params>(name: Name, params: Params) -> Self
    where
        Name: Into<Box<str>>,
        Params: Into<Box<[LawParamSurface]>>,
    {
        Self {
            name: name.into(),
            params: params.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShapeSurface {
    pub key: DefinitionKey,
    pub type_params: Box<[Box<str>]>,
    pub type_param_kinds: SurfaceTyIdList,
    pub constraints: Box<[ConstraintSurface]>,
    pub members: Box<[ShapeMemberSurface]>,
    pub laws: Box<[LawSurface]>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

impl ShapeSurface {
    #[must_use]
    pub fn new(
        key: DefinitionKey,
        members: impl Into<Box<[ShapeMemberSurface]>>,
        laws: impl Into<Box<[LawSurface]>>,
    ) -> Self {
        Self {
            key,
            type_params: Box::default(),
            type_param_kinds: Box::default(),
            constraints: Box::default(),
            members: members.into(),
            laws: laws.into(),
            inert_attrs: Box::default(),
            musi_attrs: Box::default(),
        }
    }

    #[must_use]
    pub fn with_type_params(mut self, type_params: impl Into<NameList>) -> Self {
        self.type_params = type_params.into();
        self
    }

    #[must_use]
    pub fn with_type_param_kinds<TypeParamKinds>(mut self, type_param_kinds: TypeParamKinds) -> Self
    where
        TypeParamKinds: Into<SurfaceTyIdList>,
    {
        self.type_param_kinds = type_param_kinds.into();
        self
    }

    #[must_use]
    pub fn with_constraints(mut self, constraints: impl Into<Box<[ConstraintSurface]>>) -> Self {
        self.constraints = constraints.into();
        self
    }

    #[must_use]
    pub fn with_inert_attrs(mut self, inert_attrs: impl Into<AttrList>) -> Self {
        self.inert_attrs = inert_attrs.into();
        self
    }

    #[must_use]
    pub fn with_musi_attrs(mut self, musi_attrs: impl Into<AttrList>) -> Self {
        self.musi_attrs = musi_attrs.into();
        self
    }
}
