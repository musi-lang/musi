use super::{
    super::{Attr, AttrList, DefinitionKey, NameList, SurfaceTyIdList},
    SurfaceTyId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataVariantSurface {
    pub name: Box<str>,
    pub tag: i64,
    pub payload: Option<SurfaceTyId>,
    pub result: Option<SurfaceTyId>,
    pub field_tys: SurfaceTyIdList,
    pub field_names: Box<[Option<Box<str>>]>,
}

impl DataVariantSurface {
    #[must_use]
    pub fn new<Name, FieldTys>(name: Name, field_tys: FieldTys) -> Self
    where
        Name: Into<Box<str>>,
        FieldTys: Into<SurfaceTyIdList>,
    {
        Self {
            name: name.into(),
            tag: 0,
            payload: None,
            result: None,
            field_tys: field_tys.into(),
            field_names: Box::default(),
        }
    }

    #[must_use]
    pub const fn with_tag(mut self, tag: i64) -> Self {
        self.tag = tag;
        self
    }

    #[must_use]
    pub const fn with_payload(mut self, payload: SurfaceTyId) -> Self {
        self.payload = Some(payload);
        self
    }

    #[must_use]
    pub const fn with_result(mut self, result: SurfaceTyId) -> Self {
        self.result = Some(result);
        self
    }

    #[must_use]
    pub fn with_field_names(mut self, field_names: impl Into<Box<[Option<Box<str>>]>>) -> Self {
        self.field_names = field_names.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataSurface {
    pub key: DefinitionKey,
    pub type_params: NameList,
    pub type_param_kinds: SurfaceTyIdList,
    pub variants: Box<[DataVariantSurface]>,
    pub is_record_shape: bool,
    pub repr_kind: Option<Box<str>>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
    pub frozen: bool,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

impl DataSurface {
    #[must_use]
    pub fn new(key: DefinitionKey, variants: impl Into<Box<[DataVariantSurface]>>) -> Self {
        Self {
            key,
            type_params: Box::default(),
            type_param_kinds: Box::default(),
            variants: variants.into(),
            is_record_shape: false,
            repr_kind: None,
            layout_align: None,
            layout_pack: None,
            frozen: false,
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
    pub fn with_repr_kind<ReprKind>(mut self, repr_kind: ReprKind) -> Self
    where
        ReprKind: Into<Box<str>>,
    {
        self.repr_kind = Some(repr_kind.into());
        self
    }

    #[must_use]
    pub const fn with_layout_align(mut self, layout_align: u32) -> Self {
        self.layout_align = Some(layout_align);
        self
    }

    #[must_use]
    pub const fn with_layout_pack(mut self, layout_pack: u32) -> Self {
        self.layout_pack = Some(layout_pack);
        self
    }

    #[must_use]
    pub const fn with_frozen(mut self, frozen: bool) -> Self {
        self.frozen = frozen;
        self
    }

    #[must_use]
    pub const fn with_record_shape(mut self, is_record_shape: bool) -> Self {
        self.is_record_shape = is_record_shape;
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
