use std::collections::BTreeMap;

use music_hir::{HirOrigin, HirTyId};
use music_module::ModuleKey;
use music_names::{NameBindingId, Symbol};

use crate::effects::EffectRow;

use super::{DefinitionKey, HirTyIdList, SymbolList};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprFacts {
    pub ty: HirTyId,
    pub effects: EffectRow,
}

impl ExprFacts {
    #[must_use]
    pub const fn new(ty: HirTyId, effects: EffectRow) -> Self {
        Self { ty, effects }
    }
}

/// Semantic kind for a resolved member access expression.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ExprMemberKind {
    /// Field provided by a record-like value.
    RecordField,
    /// Value exported by a module.
    ImportRecordExport,
    /// Callable resolved from receiver-first visible binding lookup.
    DotCallable,
    /// Callable declared with receiver-pattern method syntax.
    AttachedMethod,
    /// Attached method reached through receiver type namespace.
    AttachedMethodNamespace,
    /// Member projected from contextual answer lookup.
    ShapeMember,
    /// Export reached through std FFI pointer namespace support.
    FfiPointerExport,
    /// Operation exposed by an effect value.
    EffectOperation,
}

/// Resolved target information for a member access expression.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExprMemberFact {
    pub kind: ExprMemberKind,
    pub name: Symbol,
    pub ty: HirTyId,
    pub binding: Option<NameBindingId>,
    pub import_record_target: Option<ModuleKey>,
    pub index: Option<u16>,
}

impl ExprMemberFact {
    #[must_use]
    pub const fn new(kind: ExprMemberKind, name: Symbol, ty: HirTyId) -> Self {
        Self {
            kind,
            name,
            ty,
            binding: None,
            import_record_target: None,
            index: None,
        }
    }

    #[must_use]
    pub const fn with_binding(mut self, binding: NameBindingId) -> Self {
        self.binding = Some(binding);
        self
    }

    #[must_use]
    pub fn with_import_record_target(mut self, target: ModuleKey) -> Self {
        self.import_record_target = Some(target);
        self
    }

    #[must_use]
    pub const fn with_index(mut self, index: u16) -> Self {
        self.index = Some(index);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaEffectOpDef {
    params: HirTyIdList,
    param_names: Box<[Symbol]>,
    result: HirTyId,
    is_comptime_safe: bool,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaEffectDef {
    key: DefinitionKey,
    ops: BTreeMap<Box<str>, SemaEffectOpDef>,
    laws: Box<[LawFacts]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaDataVariantDef {
    tag: i64,
    payload: Option<HirTyId>,
    result: Option<HirTyId>,
    field_tys: HirTyIdList,
    field_names: Box<[Option<Box<str>>]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaDataDef {
    key: DefinitionKey,
    type_params: Box<[Symbol]>,
    type_param_kinds: HirTyIdList,
    variants: BTreeMap<Box<str>, SemaDataVariantDef>,
    is_record_shape: bool,
    repr_kind: Option<Box<str>>,
    layout_align: Option<u32>,
    layout_pack: Option<u32>,
    frozen: bool,
}

impl SemaEffectOpDef {
    #[must_use]
    pub(crate) fn new<Params, Names>(params: Params, param_names: Names, result: HirTyId) -> Self
    where
        Params: Into<HirTyIdList>,
        Names: Into<Box<[Symbol]>>,
    {
        Self {
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

    #[must_use]
    pub fn params(&self) -> &[HirTyId] {
        &self.params
    }

    #[must_use]
    pub fn param_names(&self) -> &[Symbol] {
        &self.param_names
    }

    #[must_use]
    pub const fn result(&self) -> HirTyId {
        self.result
    }

    #[must_use]
    pub const fn is_comptime_safe(&self) -> bool {
        self.is_comptime_safe
    }
}

impl SemaEffectDef {
    #[must_use]
    pub(crate) fn new(
        key: DefinitionKey,
        ops: impl Into<BTreeMap<Box<str>, SemaEffectOpDef>>,
        laws: impl Into<Box<[LawFacts]>>,
    ) -> Self {
        Self {
            key,
            ops: ops.into(),
            laws: laws.into(),
        }
    }

    #[must_use]
    pub const fn key(&self) -> &DefinitionKey {
        &self.key
    }

    #[must_use]
    pub fn op(&self, name: &str) -> Option<&SemaEffectOpDef> {
        self.ops.get(name)
    }

    #[must_use]
    pub fn op_index(&self, name: &str) -> Option<u16> {
        self.ops
            .keys()
            .position(|entry| entry.as_ref() == name)
            .and_then(|index| u16::try_from(index).ok())
    }

    #[must_use]
    pub fn op_count(&self) -> usize {
        self.ops.len()
    }

    pub fn ops(&self) -> impl Iterator<Item = (&str, &SemaEffectOpDef)> {
        self.ops.iter().map(|(name, def)| (name.as_ref(), def))
    }

    #[must_use]
    pub fn laws(&self) -> &[LawFacts] {
        &self.laws
    }
}

impl SemaDataVariantDef {
    #[must_use]
    pub(crate) fn new<FieldTys>(
        tag: i64,
        payload: Option<HirTyId>,
        result: Option<HirTyId>,
        field_tys: FieldTys,
        field_names: impl Into<Box<[Option<Box<str>>]>>,
    ) -> Self
    where
        FieldTys: Into<HirTyIdList>,
    {
        Self {
            tag,
            payload,
            result,
            field_tys: field_tys.into(),
            field_names: field_names.into(),
        }
    }

    #[must_use]
    pub const fn tag(&self) -> i64 {
        self.tag
    }

    #[must_use]
    pub const fn payload(&self) -> Option<HirTyId> {
        self.payload
    }

    #[must_use]
    pub const fn result(&self) -> Option<HirTyId> {
        self.result
    }

    #[must_use]
    pub fn field_tys(&self) -> &[HirTyId] {
        &self.field_tys
    }

    #[must_use]
    pub fn field_names(&self) -> &[Option<Box<str>>] {
        &self.field_names
    }
}

impl SemaDataDef {
    #[must_use]
    pub(crate) fn new(
        key: DefinitionKey,
        variants: impl Into<BTreeMap<Box<str>, SemaDataVariantDef>>,
        repr_kind: Option<Box<str>>,
        layout_align: Option<u32>,
        layout_pack: Option<u32>,
        frozen: bool,
    ) -> Self {
        Self {
            key,
            type_params: Box::default(),
            type_param_kinds: Box::default(),
            variants: variants.into(),
            is_record_shape: false,
            repr_kind,
            layout_align,
            layout_pack,
            frozen,
        }
    }

    #[must_use]
    pub(crate) fn with_type_params<TypeParams, TypeParamKinds>(
        mut self,
        type_params: TypeParams,
        type_param_kinds: TypeParamKinds,
    ) -> Self
    where
        TypeParams: Into<Box<[Symbol]>>,
        TypeParamKinds: Into<HirTyIdList>,
    {
        self.type_params = type_params.into();
        self.type_param_kinds = type_param_kinds.into();
        self
    }

    #[must_use]
    pub const fn with_record_shape(mut self, is_record_shape: bool) -> Self {
        self.is_record_shape = is_record_shape;
        self
    }

    #[must_use]
    pub const fn key(&self) -> &DefinitionKey {
        &self.key
    }

    #[must_use]
    pub fn type_params(&self) -> &[Symbol] {
        &self.type_params
    }

    #[must_use]
    pub fn type_param_kinds(&self) -> &[HirTyId] {
        &self.type_param_kinds
    }

    #[must_use]
    pub fn variant(&self, name: &str) -> Option<&SemaDataVariantDef> {
        self.variants.get(name)
    }

    #[must_use]
    pub fn variant_index(&self, name: &str) -> Option<u16> {
        self.variants
            .keys()
            .position(|entry| entry.as_ref() == name)
            .and_then(|index| u16::try_from(index).ok())
    }

    #[must_use]
    pub fn variant_count(&self) -> usize {
        self.variants.len()
    }

    pub fn variants(&self) -> impl Iterator<Item = (&str, &SemaDataVariantDef)> {
        self.variants.iter().map(|(name, def)| (name.as_ref(), def))
    }

    #[must_use]
    pub const fn is_record_shape(&self) -> bool {
        self.is_record_shape
    }

    #[must_use]
    pub fn record_shape_variant(&self) -> Option<&SemaDataVariantDef> {
        self.is_record_shape
            .then(|| self.variants.get(self.key.name.as_ref()))
            .flatten()
    }

    #[must_use]
    pub fn repr_kind(&self) -> Option<&str> {
        self.repr_kind.as_deref()
    }

    #[must_use]
    pub const fn layout_align(&self) -> Option<u32> {
        self.layout_align
    }

    #[must_use]
    pub const fn layout_pack(&self) -> Option<u32> {
        self.layout_pack
    }

    #[must_use]
    pub const fn frozen(&self) -> bool {
        self.frozen
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatFacts {
    pub ty: HirTyId,
}

impl PatFacts {
    #[must_use]
    pub const fn new(ty: HirTyId) -> Self {
        Self { ty }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum ConstraintKind {
    Subtype,
    Implements,
    TypeEq,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintFacts {
    pub name: Symbol,
    pub kind: ConstraintKind,
    pub value: HirTyId,
    pub shape_key: Option<DefinitionKey>,
}

impl ConstraintFacts {
    #[must_use]
    pub const fn new(name: Symbol, kind: ConstraintKind, value: HirTyId) -> Self {
        Self {
            name,
            kind,
            value,
            shape_key: None,
        }
    }

    #[must_use]
    pub fn with_shape_key(mut self, shape_key: DefinitionKey) -> Self {
        self.shape_key = Some(shape_key);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstraintKey {
    pub kind: ConstraintKind,
    pub subject: HirTyId,
    pub value: HirTyId,
    pub shape_key: Option<DefinitionKey>,
}

impl ConstraintKey {
    #[must_use]
    pub const fn new(
        kind: ConstraintKind,
        subject: HirTyId,
        value: HirTyId,
        shape_key: Option<DefinitionKey>,
    ) -> Self {
        Self {
            kind,
            subject,
            value,
            shape_key,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintAnswer {
    Param {
        key: ConstraintKey,
    },
    Provider {
        module: ModuleKey,
        name: Box<str>,
        args: Box<[Self]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShapeMemberFacts {
    pub name: Symbol,
    pub params: HirTyIdList,
    pub result: HirTyId,
}

impl ShapeMemberFacts {
    #[must_use]
    pub fn new<Params>(name: Symbol, params: Params, result: HirTyId) -> Self
    where
        Params: Into<HirTyIdList>,
    {
        Self {
            name,
            params: params.into(),
            result,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawParamFacts {
    pub name: Symbol,
    pub ty: HirTyId,
}

impl LawParamFacts {
    #[must_use]
    pub const fn new(name: Symbol, ty: HirTyId) -> Self {
        Self { name, ty }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct LawFacts {
    pub name: Symbol,
    pub params: Box<[LawParamFacts]>,
}

impl LawFacts {
    #[must_use]
    pub fn new(name: Symbol, params: impl Into<Box<[LawParamFacts]>>) -> Self {
        Self {
            name,
            params: params.into(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ShapeFacts {
    pub key: DefinitionKey,
    pub name: Symbol,
    pub type_params: Box<[Symbol]>,
    pub type_param_kinds: HirTyIdList,
    pub constraints: Box<[ConstraintFacts]>,
    pub members: Box<[ShapeMemberFacts]>,
    pub laws: Box<[LawFacts]>,
}

impl ShapeFacts {
    #[must_use]
    pub fn new(
        key: DefinitionKey,
        name: Symbol,
        members: impl Into<Box<[ShapeMemberFacts]>>,
        laws: impl Into<Box<[LawFacts]>>,
    ) -> Self {
        Self {
            key,
            name,
            type_params: Box::default(),
            type_param_kinds: Box::default(),
            constraints: Box::default(),
            members: members.into(),
            laws: laws.into(),
        }
    }

    #[must_use]
    pub fn with_type_params(mut self, type_params: impl Into<SymbolList>) -> Self {
        self.type_params = type_params.into();
        self
    }

    #[must_use]
    pub fn with_type_param_kinds<TypeParamKinds>(mut self, type_param_kinds: TypeParamKinds) -> Self
    where
        TypeParamKinds: Into<HirTyIdList>,
    {
        self.type_param_kinds = type_param_kinds.into();
        self
    }

    #[must_use]
    pub fn with_constraints(mut self, constraints: impl Into<Box<[ConstraintFacts]>>) -> Self {
        self.constraints = constraints.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct GivenFacts {
    pub origin: HirOrigin,
    pub type_params: Box<[Symbol]>,
    pub type_param_kinds: HirTyIdList,
    pub shape_key: DefinitionKey,
    pub shape_name: Symbol,
    pub shape_args: HirTyIdList,
    pub constraints: Box<[ConstraintFacts]>,
    pub evidence_keys: Box<[ConstraintKey]>,
    pub member_names: Box<[Symbol]>,
}

impl GivenFacts {
    #[must_use]
    pub fn new<ShapeArgs>(
        origin: HirOrigin,
        shape_key: DefinitionKey,
        shape_name: Symbol,
        shape_args: ShapeArgs,
        member_names: impl Into<SymbolList>,
    ) -> Self
    where
        ShapeArgs: Into<HirTyIdList>,
    {
        Self {
            origin,
            type_params: Box::default(),
            type_param_kinds: Box::default(),
            shape_key,
            shape_name,
            shape_args: shape_args.into(),
            constraints: Box::default(),
            evidence_keys: Box::default(),
            member_names: member_names.into(),
        }
    }

    #[must_use]
    pub fn with_type_params(mut self, type_params: impl Into<SymbolList>) -> Self {
        self.type_params = type_params.into();
        self
    }

    #[must_use]
    pub fn with_type_param_kinds<TypeParamKinds>(mut self, type_param_kinds: TypeParamKinds) -> Self
    where
        TypeParamKinds: Into<HirTyIdList>,
    {
        self.type_param_kinds = type_param_kinds.into();
        self
    }

    #[must_use]
    pub fn with_constraints(mut self, constraints: impl Into<Box<[ConstraintFacts]>>) -> Self {
        self.constraints = constraints.into();
        self
    }

    #[must_use]
    pub fn with_evidence_keys(mut self, evidence_keys: impl Into<Box<[ConstraintKey]>>) -> Self {
        self.evidence_keys = evidence_keys.into();
        self
    }
}
