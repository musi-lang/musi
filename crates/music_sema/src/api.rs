use std::collections::{BTreeMap, BTreeSet, HashMap, HashSet};

use music_arena::Idx;
use music_base::diag::Diag;
use music_hir::{HirExprId, HirModule, HirOrigin, HirPatId, HirTy, HirTyId};
use music_module::ModuleKey;
use music_names::{NameBindingId, Symbol};
use music_resolve::ResolvedModule;

use crate::diag::SemaDiagKind;
use crate::effects::EffectRow;

pub type SemaDiagList = Vec<Diag>;

#[must_use]
pub fn sema_diag_kind(diag: &Diag) -> Option<SemaDiagKind> {
    SemaDiagKind::from_diag(diag)
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct ForeignLinkInfo {
    pub name: Option<Box<str>>,
    pub symbol: Option<Box<str>>,
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
}

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct DefinitionKey {
    pub module: ModuleKey,
    pub name: Box<str>,
}

impl DefinitionKey {
    #[must_use]
    pub fn new(module: ModuleKey, name: impl Into<Box<str>>) -> Self {
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
    Array {
        dims: Box<[SurfaceDim]>,
        item: SurfaceTyId,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ExportedValue {
    pub name: Box<str>,
    pub ty: SurfaceTyId,
    pub type_params: Box<[Box<str>]>,
    pub constraints: Box<[ConstraintSurface]>,
    pub effects: SurfaceEffectRow,
    pub opaque: bool,
    pub module_target: Option<ModuleKey>,
    pub class_key: Option<DefinitionKey>,
    pub effect_key: Option<DefinitionKey>,
    pub data_key: Option<DefinitionKey>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataVariantSurface {
    pub name: Box<str>,
    pub payload: Option<SurfaceTyId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct DataSurface {
    pub key: DefinitionKey,
    pub variants: Box<[DataVariantSurface]>,
    pub repr_kind: Option<Box<str>>,
    pub layout_align: Option<u32>,
    pub layout_pack: Option<u32>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintSurface {
    pub name: Box<str>,
    pub kind: ConstraintKind,
    pub value: SurfaceTyId,
    pub class_key: Option<DefinitionKey>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SurfaceEffectItem {
    pub name: Box<str>,
    pub arg: Option<SurfaceTyId>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct SurfaceEffectRow {
    pub items: Box<[SurfaceEffectItem]>,
    pub open: Option<Box<str>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMemberSurface {
    pub name: Box<str>,
    pub params: Box<[SurfaceTyId]>,
    pub result: SurfaceTyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassSurface {
    pub key: DefinitionKey,
    pub constraints: Box<[ConstraintSurface]>,
    pub members: Box<[ClassMemberSurface]>,
    pub laws: Box<[Box<str>]>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectOpSurface {
    pub name: Box<str>,
    pub params: Box<[SurfaceTyId]>,
    pub result: SurfaceTyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct EffectSurface {
    pub key: DefinitionKey,
    pub ops: Box<[EffectOpSurface]>,
    pub laws: Box<[Box<str>]>,
    pub inert_attrs: Box<[Attr]>,
    pub musi_attrs: Box<[Attr]>,
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaEffectOpDef {
    params: Box<[HirTyId]>,
    result: HirTyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaEffectDef {
    key: DefinitionKey,
    ops: BTreeMap<Box<str>, SemaEffectOpDef>,
    laws: Box<[Symbol]>,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct SemaDataVariantDef {
    payload: Option<HirTyId>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct SemaDataDef {
    key: DefinitionKey,
    variants: BTreeMap<Box<str>, SemaDataVariantDef>,
    repr_kind: Option<Box<str>>,
    layout_align: Option<u32>,
    layout_pack: Option<u32>,
}

impl SemaEffectOpDef {
    #[must_use]
    pub(crate) fn new(params: impl Into<Box<[HirTyId]>>, result: HirTyId) -> Self {
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
        laws: impl Into<Box<[Symbol]>>,
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
    pub fn laws(&self) -> &[Symbol] {
        &self.laws
    }
}

impl SemaDataVariantDef {
    #[must_use]
    pub(crate) const fn new(payload: Option<HirTyId>) -> Self {
        Self { payload }
    }

    #[must_use]
    pub const fn payload(&self) -> Option<HirTyId> {
        self.payload
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
    ) -> Self {
        Self {
            key,
            variants: variants.into(),
            repr_kind,
            layout_align,
            layout_pack,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct PatFacts {
    pub ty: HirTyId,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
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

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassMemberFacts {
    pub name: Symbol,
    pub params: Box<[HirTyId]>,
    pub result: HirTyId,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ClassFacts {
    pub key: DefinitionKey,
    pub name: Symbol,
    pub constraints: Box<[ConstraintFacts]>,
    pub members: Box<[ClassMemberFacts]>,
    pub laws: Box<[Symbol]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceFacts {
    pub origin: HirOrigin,
    pub type_params: Box<[Symbol]>,
    pub class_key: DefinitionKey,
    pub class_name: Symbol,
    pub class_args: Box<[HirTyId]>,
    pub constraints: Box<[ConstraintFacts]>,
    pub member_names: Box<[Symbol]>,
}

#[derive(Debug)]
pub struct SemaModule {
    resolved: ResolvedModule,
    target: Option<TargetInfo>,
    gated_bindings: HashSet<NameBindingId>,
    foreign_links: HashMap<NameBindingId, ForeignLinkInfo>,
    expr_facts: Box<[ExprFacts]>,
    pat_facts: Box<[PatFacts]>,
    expr_module_targets: HashMap<HirExprId, ModuleKey>,
    type_test_targets: HashMap<HirExprId, HirTyId>,
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
}

struct SemaFactTables {
    expr_facts: Vec<ExprFacts>,
    pat_facts: Vec<PatFacts>,
    expr_module_targets: HashMap<HirExprId, ModuleKey>,
    type_test_targets: HashMap<HirExprId, HirTyId>,
}

struct SemaDeclTables {
    effect_defs: HashMap<Box<str>, SemaEffectDef>,
    data_defs: HashMap<Box<str>, SemaDataDef>,
    class_facts: HashMap<HirExprId, ClassFacts>,
    instance_facts: HashMap<HirExprId, InstanceFacts>,
}

impl From<crate::SemaModuleBuild> for SemaModule {
    fn from(build: crate::SemaModuleBuild) -> Self {
        let context = SemaContextTables {
            target: build.context.target,
            gated_bindings: build.context.gated_bindings,
            foreign_links: build.context.foreign_links,
        };
        let facts = SemaFactTables {
            expr_facts: build.facts.expr_facts,
            pat_facts: build.facts.pat_facts,
            expr_module_targets: build.facts.expr_module_targets,
            type_test_targets: build.facts.type_test_targets,
        };
        let decls = SemaDeclTables {
            effect_defs: build.decls.effect_defs,
            data_defs: build.decls.data_defs,
            class_facts: build.decls.class_facts,
            instance_facts: build.decls.instance_facts,
        };
        Self::from_parts(
            build.resolved,
            context,
            facts,
            decls,
            build.surface,
            build.diags,
        )
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
    pub fn is_gated_binding(&self, binding: NameBindingId) -> bool {
        self.gated_bindings.contains(&binding)
    }

    #[must_use]
    pub fn foreign_link(&self, binding: NameBindingId) -> Option<&ForeignLinkInfo> {
        self.foreign_links.get(&binding)
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
            expr_facts: facts.expr_facts.into_boxed_slice(),
            pat_facts: facts.pat_facts.into_boxed_slice(),
            expr_module_targets: facts.expr_module_targets,
            type_test_targets: facts.type_test_targets,
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
