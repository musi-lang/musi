use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use music_arena::Idx;
use music_base::diag::Diag;
use music_hir::{HirExprId, HirModule, HirOrigin, HirPatId, HirTy, HirTyId};
use music_module::ModuleKey;
use music_names::{NameBindingId, Symbol};
use music_resolve::ResolvedModule;

use crate::BindingScheme;
use crate::SemaModuleBuild;
use crate::diag::SemaDiagKind;
use crate::effects::EffectRow;

pub type SemaDiagList = Vec<Diag>;
pub type NameList = Box<[Box<str>]>;
pub type SymbolList = Box<[Symbol]>;
pub type AttrList = Box<[Attr]>;
pub type ConstraintSurfaceList = Box<[ConstraintSurface]>;
pub type HirTyIdList = Box<[HirTyId]>;
pub type SurfaceTyIdList = Box<[SurfaceTyId]>;

#[must_use]
pub fn sema_diag_kind(diag: &Diag) -> Option<SemaDiagKind> {
    SemaDiagKind::from_diag(diag)
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ForeignLinkInfo {
    pub name: Option<Box<str>>,
    pub symbol: Option<Box<str>>,
}

impl ForeignLinkInfo {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            name: None,
            symbol: None,
        }
    }

    #[must_use]
    pub fn with_name<Name>(mut self, name: Name) -> Self
    where
        Name: Into<Box<str>>,
    {
        self.name = Some(name.into());
        self
    }

    #[must_use]
    pub fn with_symbol<SymbolName>(mut self, symbol: SymbolName) -> Self
    where
        SymbolName: Into<Box<str>>,
    {
        self.symbol = Some(symbol.into());
        self
    }
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TargetInfo {
    pub os: Option<Box<str>>,
    pub arch: Option<Box<str>>,
    pub env: Option<Box<str>>,
    pub abi: Option<Box<str>>,
    pub vendor: Option<Box<str>>,
    pub features: BTreeSet<Box<str>>,
}

impl TargetInfo {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            os: None,
            arch: None,
            env: None,
            abi: None,
            vendor: None,
            features: BTreeSet::new(),
        }
    }

    #[must_use]
    pub fn with_os<Os>(mut self, os: Os) -> Self
    where
        Os: Into<Box<str>>,
    {
        self.os = Some(os.into());
        self
    }

    #[must_use]
    pub fn with_arch<Arch>(mut self, arch: Arch) -> Self
    where
        Arch: Into<Box<str>>,
    {
        self.arch = Some(arch.into());
        self
    }

    #[must_use]
    pub fn with_env<Env>(mut self, env: Env) -> Self
    where
        Env: Into<Box<str>>,
    {
        self.env = Some(env.into());
        self
    }

    #[must_use]
    pub fn with_abi<Abi>(mut self, abi: Abi) -> Self
    where
        Abi: Into<Box<str>>,
    {
        self.abi = Some(abi.into());
        self
    }

    #[must_use]
    pub fn with_vendor<Vendor>(mut self, vendor: Vendor) -> Self
    where
        Vendor: Into<Box<str>>,
    {
        self.vendor = Some(vendor.into());
        self
    }

    #[must_use]
    pub fn with_features(mut self, features: BTreeSet<Box<str>>) -> Self {
        self.features = features;
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Attr {
    pub path: Box<[Box<str>]>,
    pub args: Box<[AttrArg]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrArg {
    pub name: Option<Box<str>>,
    pub value: AttrValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AttrRecordField {
    pub name: Box<str>,
    pub value: AttrValue,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum AttrValue {
    String(Box<str>),
    Int(Box<str>),
    Rune(u32),
    Variant { tag: Box<str>, args: Box<[Self]> },
    Array { items: Box<[Self]> },
    Record { fields: Box<[AttrRecordField]> },
}

pub trait SemaEnv {
    fn module_surface(&self, key: &ModuleKey) -> Option<ModuleSurface>;
}

#[derive(Clone, Default)]
pub struct SemaOptions<'env> {
    pub target: Option<TargetInfo>,
    pub env: Option<&'env dyn SemaEnv>,
    pub prelude: Option<&'env ModuleSurface>,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefinitionKey {
    pub module: ModuleKey,
    pub name: Box<str>,
}

impl DefinitionKey {
    #[must_use]
    pub fn new<Name>(module: ModuleKey, name: Name) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            module,
            name: name.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SurfaceTyId(u32);

impl SurfaceTyId {
    #[must_use]
    pub const fn new(raw: u32) -> Self {
        Self(raw)
    }

    #[must_use]
    pub const fn raw(self) -> u32 {
        self.0
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceTy {
    pub kind: SurfaceTyKind,
}

impl SurfaceTy {
    #[must_use]
    pub const fn new(kind: SurfaceTyKind) -> Self {
        Self { kind }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SurfaceTyKind {
    Error,
    Unknown,
    Type,
    Syntax,
    Any,
    Empty,
    Unit,
    Bool,
    Nat,
    Int,
    Float,
    String,
    CString,
    CPtr,
    Module,
    NatLit(u64),
    Named {
        name: Box<str>,
        args: Box<[SurfaceTyId]>,
    },
    Pi {
        binder: Box<str>,
        binder_ty: SurfaceTyId,
        body: SurfaceTyId,
        is_effectful: bool,
    },
    Arrow {
        params: Box<[SurfaceTyId]>,
        ret: SurfaceTyId,
        is_effectful: bool,
    },
    Sum {
        left: SurfaceTyId,
        right: SurfaceTyId,
    },
    Tuple {
        items: Box<[SurfaceTyId]>,
    },
    Seq {
        item: SurfaceTyId,
    },
    Array {
        dims: Box<[SurfaceDim]>,
        item: SurfaceTyId,
    },
    Range {
        bound: SurfaceTyId,
    },
    ClosedRange {
        bound: SurfaceTyId,
    },
    PartialRangeFrom {
        bound: SurfaceTyId,
    },
    PartialRangeUpTo {
        bound: SurfaceTyId,
    },
    PartialRangeThru {
        bound: SurfaceTyId,
    },
    Handler {
        effect: SurfaceTyId,
        input: SurfaceTyId,
        output: SurfaceTyId,
    },
    Mut {
        inner: SurfaceTyId,
    },
    Record {
        fields: Box<[SurfaceTyField]>,
    },
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum SurfaceDim {
    Unknown,
    Name(Box<str>),
    Int(u32),
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceTyField {
    pub name: Box<str>,
    pub ty: SurfaceTyId,
}

impl SurfaceTyField {
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
pub struct ExportedValue {
    pub name: Box<str>,
    pub ty: SurfaceTyId,
    pub receiver_ty: Option<SurfaceTyId>,
    pub receiver_mut: bool,
    pub type_params: NameList,
    pub constraints: ConstraintSurfaceList,
    pub effects: SurfaceEffectRow,
    pub opaque: bool,
    pub module_target: Option<ModuleKey>,
    pub class_key: Option<DefinitionKey>,
    pub effect_key: Option<DefinitionKey>,
    pub data_key: Option<DefinitionKey>,
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
            receiver_ty: None,
            receiver_mut: false,
            type_params: Box::default(),
            constraints: Box::default(),
            effects: SurfaceEffectRow::default(),
            opaque: false,
            module_target: None,
            class_key: None,
            effect_key: None,
            data_key: None,
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
    pub const fn with_receiver(mut self, receiver_ty: SurfaceTyId, receiver_mut: bool) -> Self {
        self.receiver_ty = Some(receiver_ty);
        self.receiver_mut = receiver_mut;
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
    pub fn with_module_target(mut self, module_target: ModuleKey) -> Self {
        self.module_target = Some(module_target);
        self
    }

    #[must_use]
    pub fn with_class_key(mut self, class_key: DefinitionKey) -> Self {
        self.class_key = Some(class_key);
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataVariantSurface {
    pub name: Box<str>,
    pub payload: Option<SurfaceTyId>,
    pub field_tys: SurfaceTyIdList,
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
            payload: None,
            field_tys: field_tys.into(),
        }
    }

    #[must_use]
    pub const fn with_payload(mut self, payload: SurfaceTyId) -> Self {
        self.payload = Some(payload);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataSurface {
    pub key: DefinitionKey,
    pub variants: Box<[DataVariantSurface]>,
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
            variants: variants.into(),
            repr_kind: None,
            layout_align: None,
            layout_pack: None,
            frozen: false,
            inert_attrs: Box::default(),
            musi_attrs: Box::default(),
        }
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintSurface {
    pub name: Box<str>,
    pub kind: ConstraintKind,
    pub value: SurfaceTyId,
    pub class_key: Option<DefinitionKey>,
}

impl ConstraintSurface {
    #[must_use]
    pub fn new<Name>(name: Name, kind: ConstraintKind, value: SurfaceTyId) -> Self
    where
        Name: Into<Box<str>>,
    {
        Self {
            name: name.into(),
            kind,
            value,
            class_key: None,
        }
    }

    #[must_use]
    pub fn with_class_key(mut self, class_key: DefinitionKey) -> Self {
        self.class_key = Some(class_key);
        self
    }
}

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
pub struct ClassMemberSurface {
    pub name: Box<str>,
    pub params: Box<[SurfaceTyId]>,
    pub result: SurfaceTyId,
}

impl ClassMemberSurface {
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
pub struct ClassSurface {
    pub key: DefinitionKey,
    pub type_params: Box<[Box<str>]>,
    pub constraints: Box<[ConstraintSurface]>,
    pub members: Box<[ClassMemberSurface]>,
    pub laws: Box<[LawSurface]>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

impl ClassSurface {
    #[must_use]
    pub fn new(
        key: DefinitionKey,
        members: impl Into<Box<[ClassMemberSurface]>>,
        laws: impl Into<Box<[LawSurface]>>,
    ) -> Self {
        Self {
            key,
            type_params: Box::default(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpSurface {
    pub name: Box<str>,
    pub params: Box<[SurfaceTyId]>,
    pub result: SurfaceTyId,
}

impl EffectOpSurface {
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceSurface {
    pub type_params: Box<[Box<str>]>,
    pub class_key: DefinitionKey,
    pub class_args: Box<[SurfaceTyId]>,
    pub constraints: Box<[ConstraintSurface]>,
    pub member_names: Box<[Box<str>]>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

impl InstanceSurface {
    #[must_use]
    pub fn new<ClassArgs>(
        class_key: DefinitionKey,
        class_args: ClassArgs,
        member_names: impl Into<NameList>,
    ) -> Self
    where
        ClassArgs: Into<SurfaceTyIdList>,
    {
        Self {
            type_params: Box::default(),
            class_key,
            class_args: class_args.into(),
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSurface {
    module_key: ModuleKey,
    static_imports: Box<[ModuleKey]>,
    tys: Box<[SurfaceTy]>,
    exported_values: Box<[ExportedValue]>,
    exported_data: Box<[DataSurface]>,
    exported_classes: Box<[ClassSurface]>,
    exported_effects: Box<[EffectSurface]>,
    exported_instances: Box<[InstanceSurface]>,
}

type ModuleSurfaceExports = (
    Box<[ExportedValue]>,
    Box<[DataSurface]>,
    Box<[ClassSurface]>,
    Box<[EffectSurface]>,
    Box<[InstanceSurface]>,
);

impl ModuleSurface {
    pub(super) fn from_collected(
        module_key: ModuleKey,
        static_imports: Box<[ModuleKey]>,
        tys: Box<[SurfaceTy]>,
        exports: ModuleSurfaceExports,
    ) -> Self {
        Self {
            module_key,
            static_imports,
            tys,
            exported_values: exports.0,
            exported_data: exports.1,
            exported_classes: exports.2,
            exported_effects: exports.3,
            exported_instances: exports.4,
        }
    }

    #[must_use]
    pub const fn module_key(&self) -> &ModuleKey {
        &self.module_key
    }

    #[must_use]
    pub fn static_imports(&self) -> &[ModuleKey] {
        &self.static_imports
    }

    #[must_use]
    pub fn types(&self) -> &[SurfaceTy] {
        &self.tys
    }

    #[must_use]
    pub fn exported_values(&self) -> &[ExportedValue] {
        &self.exported_values
    }

    #[must_use]
    pub fn exported_data_defs(&self) -> &[DataSurface] {
        &self.exported_data
    }

    #[must_use]
    pub fn exported_classes(&self) -> &[ClassSurface] {
        &self.exported_classes
    }

    #[must_use]
    pub fn exported_effects(&self) -> &[EffectSurface] {
        &self.exported_effects
    }

    #[must_use]
    pub fn exported_instances(&self) -> &[InstanceSurface] {
        &self.exported_instances
    }

    #[must_use]
    pub fn try_ty(&self, id: SurfaceTyId) -> Option<&SurfaceTy> {
        self.tys
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
    }

    #[must_use]
    pub fn exported_value(&self, name: &str) -> Option<&ExportedValue> {
        self.exported_values
            .iter()
            .find(|value| value.name.as_ref() == name)
    }

    #[must_use]
    pub fn exported_class(&self, key: &DefinitionKey) -> Option<&ClassSurface> {
        self.exported_classes.iter().find(|class| &class.key == key)
    }

    #[must_use]
    pub fn exported_data(&self, key: &DefinitionKey) -> Option<&DataSurface> {
        self.exported_data.iter().find(|data| &data.key == key)
    }

    #[must_use]
    pub fn exported_effect(&self, key: &DefinitionKey) -> Option<&EffectSurface> {
        self.exported_effects
            .iter()
            .find(|effect| &effect.key == key)
    }
}

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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaEffectOpDef {
    params: HirTyIdList,
    result: HirTyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaEffectDef {
    key: DefinitionKey,
    ops: BTreeMap<Box<str>, SemaEffectOpDef>,
    laws: Box<[LawFacts]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaDataVariantDef {
    payload: Option<HirTyId>,
    field_tys: HirTyIdList,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaDataDef {
    key: DefinitionKey,
    variants: BTreeMap<Box<str>, SemaDataVariantDef>,
    repr_kind: Option<Box<str>>,
    layout_align: Option<u32>,
    layout_pack: Option<u32>,
    frozen: bool,
}

impl SemaEffectOpDef {
    #[must_use]
    pub(crate) fn new<Params>(params: Params, result: HirTyId) -> Self
    where
        Params: Into<HirTyIdList>,
    {
        Self {
            params: params.into(),
            result,
        }
    }

    #[must_use]
    pub fn params(&self) -> &[HirTyId] {
        &self.params
    }

    #[must_use]
    pub const fn result(&self) -> HirTyId {
        self.result
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
    pub(crate) fn new<FieldTys>(payload: Option<HirTyId>, field_tys: FieldTys) -> Self
    where
        FieldTys: Into<HirTyIdList>,
    {
        Self {
            payload,
            field_tys: field_tys.into(),
        }
    }

    #[must_use]
    pub const fn payload(&self) -> Option<HirTyId> {
        self.payload
    }

    #[must_use]
    pub fn field_tys(&self) -> &[HirTyId] {
        &self.field_tys
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
            variants: variants.into(),
            repr_kind,
            layout_align,
            layout_pack,
            frozen,
        }
    }

    #[must_use]
    pub const fn key(&self) -> &DefinitionKey {
        &self.key
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintFacts {
    pub name: Symbol,
    pub kind: ConstraintKind,
    pub value: HirTyId,
    pub class_key: Option<DefinitionKey>,
}

impl ConstraintFacts {
    #[must_use]
    pub const fn new(name: Symbol, kind: ConstraintKind, value: HirTyId) -> Self {
        Self {
            name,
            kind,
            value,
            class_key: None,
        }
    }

    #[must_use]
    pub fn with_class_key(mut self, class_key: DefinitionKey) -> Self {
        self.class_key = Some(class_key);
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct ConstraintKey {
    pub kind: ConstraintKind,
    pub subject: HirTyId,
    pub value: HirTyId,
    pub class_key: Option<DefinitionKey>,
}

impl ConstraintKey {
    #[must_use]
    pub const fn new(
        kind: ConstraintKind,
        subject: HirTyId,
        value: HirTyId,
        class_key: Option<DefinitionKey>,
    ) -> Self {
        Self {
            kind,
            subject,
            value,
            class_key,
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ConstraintEvidence {
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
pub struct ClassMemberFacts {
    pub name: Symbol,
    pub params: HirTyIdList,
    pub result: HirTyId,
}

impl ClassMemberFacts {
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
pub struct ClassFacts {
    pub key: DefinitionKey,
    pub name: Symbol,
    pub type_params: Box<[Symbol]>,
    pub constraints: Box<[ConstraintFacts]>,
    pub members: Box<[ClassMemberFacts]>,
    pub laws: Box<[LawFacts]>,
}

impl ClassFacts {
    #[must_use]
    pub fn new(
        key: DefinitionKey,
        name: Symbol,
        members: impl Into<Box<[ClassMemberFacts]>>,
        laws: impl Into<Box<[LawFacts]>>,
    ) -> Self {
        Self {
            key,
            name,
            type_params: Box::default(),
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
    pub fn with_constraints(mut self, constraints: impl Into<Box<[ConstraintFacts]>>) -> Self {
        self.constraints = constraints.into();
        self
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceFacts {
    pub origin: HirOrigin,
    pub type_params: Box<[Symbol]>,
    pub class_key: DefinitionKey,
    pub class_name: Symbol,
    pub class_args: HirTyIdList,
    pub constraints: Box<[ConstraintFacts]>,
    pub evidence_keys: Box<[ConstraintKey]>,
    pub member_names: Box<[Symbol]>,
}

impl InstanceFacts {
    #[must_use]
    pub fn new<ClassArgs>(
        origin: HirOrigin,
        class_key: DefinitionKey,
        class_name: Symbol,
        class_args: ClassArgs,
        member_names: impl Into<SymbolList>,
    ) -> Self
    where
        ClassArgs: Into<HirTyIdList>,
    {
        Self {
            origin,
            type_params: Box::default(),
            class_key,
            class_name,
            class_args: class_args.into(),
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

#[derive(Debug)]
pub struct SemaModule {
    resolved: ResolvedModule,
    target: Option<TargetInfo>,
    gated_bindings: HashSet<NameBindingId>,
    foreign_links: HashMap<NameBindingId, ForeignLinkInfo>,
    binding_types: HashMap<NameBindingId, HirTyId>,
    binding_schemes: HashMap<NameBindingId, BindingScheme>,
    binding_evidence_keys: HashMap<NameBindingId, Box<[ConstraintKey]>>,
    expr_facts: Box<[ExprFacts]>,
    pat_facts: Box<[PatFacts]>,
    expr_module_targets: HashMap<HirExprId, ModuleKey>,
    type_test_targets: HashMap<HirExprId, HirTyId>,
    expr_evidence: HashMap<HirExprId, Box<[ConstraintEvidence]>>,
    expr_attached_bindings: HashMap<HirExprId, NameBindingId>,
    effect_defs: HashMap<Box<str>, SemaEffectDef>,
    data_defs: HashMap<Box<str>, SemaDataDef>,
    class_facts: HashMap<HirExprId, ClassFacts>,
    instance_facts: HashMap<HirExprId, InstanceFacts>,
    surface: ModuleSurface,
    diags: SemaDiagList,
}

struct SemaContextTables {
    target: Option<TargetInfo>,
    gated_bindings: HashSet<NameBindingId>,
    foreign_links: HashMap<NameBindingId, ForeignLinkInfo>,
    binding_types: HashMap<NameBindingId, HirTyId>,
    binding_schemes: HashMap<NameBindingId, BindingScheme>,
    binding_evidence_keys: HashMap<NameBindingId, Box<[ConstraintKey]>>,
}

struct SemaFactTables {
    expr_facts: Vec<ExprFacts>,
    pat_facts: Vec<PatFacts>,
    expr_module_targets: HashMap<HirExprId, ModuleKey>,
    type_test_targets: HashMap<HirExprId, HirTyId>,
    expr_evidence: HashMap<HirExprId, Box<[ConstraintEvidence]>>,
    expr_attached_bindings: HashMap<HirExprId, NameBindingId>,
}

struct SemaDeclTables {
    effect_defs: HashMap<Box<str>, SemaEffectDef>,
    data_defs: HashMap<Box<str>, SemaDataDef>,
    class_facts: HashMap<HirExprId, ClassFacts>,
    instance_facts: HashMap<HirExprId, InstanceFacts>,
}

impl From<SemaModuleBuild> for SemaModule {
    fn from(build: SemaModuleBuild) -> Self {
        let crate::SemaModuleBuild {
            resolved,
            context: build_context,
            facts: build_facts,
            decls: build_decls,
            surface,
            diags,
        } = build;
        let crate::SemaFactsBuild {
            expr_facts,
            pat_facts,
            expr_module_targets,
            type_test_targets,
            expr_evidence,
            expr_attached_bindings,
        } = build_facts;
        let context = SemaContextTables {
            target: build_context.target,
            gated_bindings: build_context.gated_bindings,
            foreign_links: build_context.foreign_links,
            binding_types: build_context.binding_types,
            binding_schemes: build_context.binding_schemes,
            binding_evidence_keys: build_context.binding_evidence_keys,
        };
        let facts = SemaFactTables {
            expr_facts,
            pat_facts,
            expr_module_targets,
            type_test_targets,
            expr_evidence,
            expr_attached_bindings,
        };
        let decls = SemaDeclTables {
            effect_defs: build_decls.effect_defs,
            data_defs: build_decls.data_defs,
            class_facts: build_decls.class_facts,
            instance_facts: build_decls.instance_facts,
        };
        Self::from_parts(resolved, context, facts, decls, surface, diags)
    }
}

impl SemaModule {
    #[must_use]
    pub const fn resolved(&self) -> &ResolvedModule {
        &self.resolved
    }

    #[must_use]
    pub const fn module(&self) -> &HirModule {
        &self.resolved.module
    }

    #[must_use]
    pub fn diags(&self) -> &[Diag] {
        &self.diags
    }

    #[must_use]
    pub fn ty(&self, id: HirTyId) -> &HirTy {
        self.module().store.tys.get(id)
    }

    #[must_use]
    pub fn try_expr_ty(&self, id: HirExprId) -> Option<HirTyId> {
        self.expr_facts.get(idx_to_usize(id)).map(|facts| facts.ty)
    }

    #[must_use]
    pub fn try_expr_effects(&self, id: HirExprId) -> Option<&EffectRow> {
        self.expr_facts
            .get(idx_to_usize(id))
            .map(|facts| &facts.effects)
    }

    #[must_use]
    pub fn expr_module_target(&self, id: HirExprId) -> Option<&ModuleKey> {
        self.expr_module_targets.get(&id)
    }

    #[must_use]
    pub fn type_test_target(&self, id: HirExprId) -> Option<HirTyId> {
        self.type_test_targets.get(&id).copied()
    }

    #[must_use]
    pub fn expr_evidence(&self, id: HirExprId) -> Option<&[ConstraintEvidence]> {
        self.expr_evidence.get(&id).map(Box::as_ref)
    }

    #[must_use]
    pub fn expr_attached_binding(&self, id: HirExprId) -> Option<NameBindingId> {
        self.expr_attached_bindings.get(&id).copied()
    }

    #[must_use]
    pub fn is_gated_binding(&self, binding: NameBindingId) -> bool {
        self.gated_bindings.contains(&binding)
    }

    #[must_use]
    pub fn foreign_link(&self, binding: NameBindingId) -> Option<&ForeignLinkInfo> {
        self.foreign_links.get(&binding)
    }

    #[must_use]
    pub fn binding_type(&self, binding: NameBindingId) -> Option<HirTyId> {
        self.binding_types.get(&binding).copied()
    }

    #[must_use]
    pub fn binding_scheme(&self, binding: NameBindingId) -> Option<&BindingScheme> {
        self.binding_schemes.get(&binding)
    }

    #[must_use]
    pub fn binding_evidence_keys(&self, binding: NameBindingId) -> Option<&[ConstraintKey]> {
        self.binding_evidence_keys.get(&binding).map(Box::as_ref)
    }

    #[must_use]
    pub fn try_pat_ty(&self, id: HirPatId) -> Option<HirTyId> {
        self.pat_facts.get(idx_to_usize(id)).map(|facts| facts.ty)
    }

    #[must_use]
    pub fn class_facts(&self, id: HirExprId) -> Option<&ClassFacts> {
        self.class_facts.get(&id)
    }

    #[must_use]
    pub fn class_facts_by_name(&self, name: Symbol) -> Option<&ClassFacts> {
        self.class_facts.values().find(|facts| facts.name == name)
    }

    #[must_use]
    pub fn effect_def(&self, name: &str) -> Option<&SemaEffectDef> {
        self.effect_defs.get(name)
    }

    pub fn effect_defs(&self) -> impl Iterator<Item = &SemaEffectDef> {
        self.effect_defs.values()
    }

    #[must_use]
    pub fn data_def(&self, name: &str) -> Option<&SemaDataDef> {
        self.data_defs.get(name)
    }

    pub fn data_defs(&self) -> impl Iterator<Item = &SemaDataDef> {
        self.data_defs.values()
    }

    #[must_use]
    pub fn instance_facts(&self, id: HirExprId) -> Option<&InstanceFacts> {
        self.instance_facts.get(&id)
    }

    #[must_use]
    pub const fn surface(&self) -> &ModuleSurface {
        &self.surface
    }
}

impl SemaModule {
    fn from_parts(
        resolved: ResolvedModule,
        context: SemaContextTables,
        facts: SemaFactTables,
        decls: SemaDeclTables,
        surface: ModuleSurface,
        diags: SemaDiagList,
    ) -> Self {
        Self {
            resolved,
            target: context.target,
            gated_bindings: context.gated_bindings,
            foreign_links: context.foreign_links,
            binding_types: context.binding_types,
            binding_schemes: context.binding_schemes,
            binding_evidence_keys: context.binding_evidence_keys,
            expr_facts: facts.expr_facts.into_boxed_slice(),
            pat_facts: facts.pat_facts.into_boxed_slice(),
            expr_module_targets: facts.expr_module_targets,
            type_test_targets: facts.type_test_targets,
            expr_evidence: facts.expr_evidence,
            expr_attached_bindings: facts.expr_attached_bindings,
            effect_defs: decls.effect_defs,
            data_defs: decls.data_defs,
            class_facts: decls.class_facts,
            instance_facts: decls.instance_facts,
            surface,
            diags,
        }
    }

    #[must_use]
    pub const fn target(&self) -> Option<&TargetInfo> {
        self.target.as_ref()
    }
}

fn idx_to_usize<T>(idx: Idx<T>) -> usize {
    usize::try_from(idx.raw()).unwrap_or(usize::MAX)
}
