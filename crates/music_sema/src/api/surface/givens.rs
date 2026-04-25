use super::{
    super::{Attr, AttrList, DefinitionKey, NameList, SurfaceTyIdList},
    ConstraintSurface, SurfaceTyId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GivenSurface {
    pub type_params: Box<[Box<str>]>,
    pub type_param_kinds: SurfaceTyIdList,
    pub shape_key: DefinitionKey,
    pub shape_args: Box<[SurfaceTyId]>,
    pub constraints: Box<[ConstraintSurface]>,
    pub member_names: Box<[Box<str>]>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

impl GivenSurface {
    #[must_use]
    pub fn new<ShapeArgs>(
        shape_key: DefinitionKey,
        shape_args: ShapeArgs,
        member_names: impl Into<NameList>,
    ) -> Self
    where
        ShapeArgs: Into<SurfaceTyIdList>,
    {
        Self {
            type_params: Box::default(),
            type_param_kinds: Box::default(),
            shape_key,
            shape_args: shape_args.into(),
            constraints: Box::default(),
            member_names: member_names.into(),
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
