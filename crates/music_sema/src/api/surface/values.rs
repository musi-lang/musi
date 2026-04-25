use music_module::ModuleKey;

use super::{
    super::{
        AttrList, ComptimeParamList, ConstraintSurfaceList, DefinitionKey, NameList,
        SurfaceTyIdList,
    },
    ComptimeValue, SurfaceEffectRow, SurfaceTyId,
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportedValue {
    pub name: Box<str>,
    pub ty: SurfaceTyId,
    pub type_params: NameList,
    pub type_param_kinds: SurfaceTyIdList,
    pub param_names: NameList,
    pub comptime_params: ComptimeParamList,
    pub constraints: ConstraintSurfaceList,
    pub effects: SurfaceEffectRow,
    pub opaque: bool,
    pub import_record_target: Option<ModuleKey>,
    pub shape_key: Option<DefinitionKey>,
    pub effect_key: Option<DefinitionKey>,
    pub data_key: Option<DefinitionKey>,
    pub is_attached_method: bool,
    pub const_int: Option<i64>,
    pub comptime_value: Option<ComptimeValue>,
    pub inert_attrs: AttrList,
    pub musi_attrs: AttrList,
}

impl ExportedValue {
    #[must_use]
    pub fn new<Name>(name: Name, ty: SurfaceTyId) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            ty,
            type_params: Box::default(),
            type_param_kinds: Box::default(),
            param_names: Box::default(),
            comptime_params: Box::default(),
            constraints: Box::default(),
            effects: SurfaceEffectRow::default(),
            opaque: false,
            import_record_target: None,
            shape_key: None,
            effect_key: None,
            data_key: None,
            is_attached_method: false,
            const_int: None,
            comptime_value: None,
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
    pub fn with_param_names(mut self, param_names: impl Into<NameList>) -> Self {
        self.param_names = param_names.into();
        self
    }

    #[must_use]
    pub fn with_comptime_params(mut self, params: impl Into<ComptimeParamList>) -> Self {
        self.comptime_params = params.into();
        self
    }

    #[must_use]
    pub fn with_constraints(mut self, constraints: impl Into<ConstraintSurfaceList>) -> Self {
        self.constraints = constraints.into();
        self
    }

    #[must_use]
    pub fn with_effects(mut self, effects: SurfaceEffectRow) -> Self {
        self.effects = effects;
        self
    }

    #[must_use]
    pub const fn with_opaque(mut self, opaque: bool) -> Self {
        self.opaque = opaque;
        self
    }

    #[must_use]
    pub fn with_import_record_target(mut self, import_record_target: ModuleKey) -> Self {
        self.import_record_target = Some(import_record_target);
        self
    }

    #[must_use]
    pub fn with_shape_key(mut self, shape_key: DefinitionKey) -> Self {
        self.shape_key = Some(shape_key);
        self
    }

    #[must_use]
    pub fn with_effect_key(mut self, effect_key: DefinitionKey) -> Self {
        self.effect_key = Some(effect_key);
        self
    }

    #[must_use]
    pub fn with_data_key(mut self, data_key: DefinitionKey) -> Self {
        self.data_key = Some(data_key);
        self
    }

    #[must_use]
    pub const fn with_attached_method(mut self) -> Self {
        self.is_attached_method = true;
        self
    }

    #[must_use]
    pub const fn with_const_int(mut self, value: i64) -> Self {
        self.const_int = Some(value);
        self
    }

    #[must_use]
    pub fn with_comptime_value(mut self, value: ComptimeValue) -> Self {
        if let ComptimeValue::Int(int) = &value {
            self.const_int = Some(*int);
        }
        self.comptime_value = Some(value);
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
