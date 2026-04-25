use super::{
    super::{Attr, AttrList, DefinitionKey, NameList, SurfaceTyIdList},
    LawSurface, SurfaceTyId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceEffectItem {
    pub name: Box<str>,
    pub arg: Option<SurfaceTyId>,
}

impl SurfaceEffectItem {
    #[must_use]
    pub fn new<Name>(name: Name, arg: Option<SurfaceTyId>) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            arg,
        }
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SurfaceEffectRow {
    pub items: Box<[SurfaceEffectItem]>,
    pub open: Option<Box<str>>,
}

impl SurfaceEffectRow {
    #[must_use]
    pub fn new(items: impl Into<Box<[SurfaceEffectItem]>>) -> Self {
        Self {
            items: items.into(),
            open: None,
        }
    }

    #[must_use]
    pub fn with_open<Open>(mut self, open: Open) -> Self
    where
        Open: Into<Box<str>>,
    {
        self.open = Some(open.into());
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpSurface {
    pub name: Box<str>,
    pub params: Box<[SurfaceTyId]>,
    pub param_names: NameList,
    pub result: SurfaceTyId,
    pub is_comptime_safe: bool,
}

impl EffectOpSurface {
    #[must_use]
    pub fn new<Name, Params, ParamNames>(
        name: Name,
        params: Params,
        param_names: ParamNames,
        result: SurfaceTyId,
    ) -> Self
    where
        Name: Into<Box<str>>,
        Params: Into<SurfaceTyIdList>,
        ParamNames: Into<NameList>,
    {
        Self {
            name: name.into(),
            params: params.into(),
            param_names: param_names.into(),
            result,
            is_comptime_safe: false,
        }
    }

    #[must_use]
    pub const fn with_comptime_safe(mut self, is_comptime_safe: bool) -> Self {
        self.is_comptime_safe = is_comptime_safe;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectSurface {
    pub key: DefinitionKey,
    pub ops: Box<[EffectOpSurface]>,
    pub laws: Box<[LawSurface]>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

impl EffectSurface {
    #[must_use]
    pub fn new(
        key: DefinitionKey,
        ops: impl Into<Box<[EffectOpSurface]>>,
        laws: impl Into<Box<[LawSurface]>>,
    ) -> Self {
        Self {
            key,
            ops: ops.into(),
            laws: laws.into(),
            inert_attrs: Box::default(),
            musi_attrs: Box::default(),
        }
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
