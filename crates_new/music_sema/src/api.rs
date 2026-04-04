use std::collections::{BTreeSet, HashMap};

use music_arena::Idx;
use music_base::diag::Diag;
use music_hir::{HirExprId, HirModule, HirOrigin, HirPatId, HirTy, HirTyId};
use music_module::ModuleKey;
use music_names::Symbol;
use music_resolve::ResolvedModule;

use crate::effects::EffectRow;

pub type SemaDiagList = Vec<Diag>;

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct TargetInfo {
    pub os: Option<Box<str>>,
    pub arch: Option<Box<str>>,
    pub env: Option<Box<str>>,
    pub abi: Option<Box<str>>,
    pub vendor: Option<Box<str>>,
    pub features: BTreeSet<Box<str>>,
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
    Int,
    Float,
    String,
    CString,
    CPtr,
    Module,
    Named {
        name: Box<str>,
        args: Box<[SurfaceTyId]>,
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
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstanceSurface {
    pub type_params: Box<[Box<str>]>,
    pub class_key: DefinitionKey,
    pub class_args: Box<[SurfaceTyId]>,
    pub constraints: Box<[ConstraintSurface]>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleSurface {
    pub module_key: ModuleKey,
    pub static_imports: Box<[ModuleKey]>,
    pub tys: Box<[SurfaceTy]>,
    pub exported_values: Box<[ExportedValue]>,
    pub exported_classes: Box<[ClassSurface]>,
    pub exported_effects: Box<[EffectSurface]>,
    pub exported_instances: Box<[InstanceSurface]>,
}

impl ModuleSurface {
    /// # Panics
    ///
    /// Panics if `id` does not refer to a type in this surface.
    #[must_use]
    pub fn ty(&self, id: SurfaceTyId) -> &SurfaceTy {
        self.tys
            .get(usize::try_from(id.raw()).unwrap_or(usize::MAX))
            .expect("surface type id out of bounds")
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
    expr_facts: Box<[ExprFacts]>,
    pat_facts: Box<[PatFacts]>,
    expr_module_targets: HashMap<HirExprId, ModuleKey>,
    class_facts: HashMap<HirExprId, ClassFacts>,
    instance_facts: HashMap<HirExprId, InstanceFacts>,
    surface: ModuleSurface,
    diags: SemaDiagList,
}

pub struct SemaModuleParts {
    pub resolved: ResolvedModule,
    pub expr_facts: Vec<ExprFacts>,
    pub pat_facts: Vec<PatFacts>,
    pub expr_module_targets: HashMap<HirExprId, ModuleKey>,
    pub class_facts: HashMap<HirExprId, ClassFacts>,
    pub instance_facts: HashMap<HirExprId, InstanceFacts>,
    pub surface: ModuleSurface,
    pub diags: SemaDiagList,
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

    /// # Panics
    /// Panics if `id` does not refer to an expression in this module.
    #[must_use]
    pub fn expr_ty(&self, id: HirExprId) -> HirTyId {
        self.expr_facts
            .get(idx_to_usize(id))
            .expect("expr facts missing for HIR expr id")
            .ty
    }

    /// # Panics
    /// Panics if `id` does not refer to an expression in this module.
    #[must_use]
    pub fn expr_effects(&self, id: HirExprId) -> &EffectRow {
        &self
            .expr_facts
            .get(idx_to_usize(id))
            .expect("expr facts missing for HIR expr id")
            .effects
    }

    #[must_use]
    pub fn expr_module_target(&self, id: HirExprId) -> Option<&ModuleKey> {
        self.expr_module_targets.get(&id)
    }

    /// # Panics
    /// Panics if `id` does not refer to a pattern in this module.
    #[must_use]
    pub fn pat_ty(&self, id: HirPatId) -> HirTyId {
        self.pat_facts
            .get(idx_to_usize(id))
            .expect("pat facts missing for HIR pat id")
            .ty
    }

    #[must_use]
    pub fn class_facts(&self, id: HirExprId) -> Option<&ClassFacts> {
        self.class_facts.get(&id)
    }

    #[must_use]
    pub fn instance_facts(&self, id: HirExprId) -> Option<&InstanceFacts> {
        self.instance_facts.get(&id)
    }

    #[must_use]
    pub const fn surface(&self) -> &ModuleSurface {
        &self.surface
    }

    #[must_use]
    pub(crate) fn from_parts(parts: SemaModuleParts) -> Self {
        Self {
            resolved: parts.resolved,
            expr_facts: parts.expr_facts.into_boxed_slice(),
            pat_facts: parts.pat_facts.into_boxed_slice(),
            expr_module_targets: parts.expr_module_targets,
            class_facts: parts.class_facts,
            instance_facts: parts.instance_facts,
            surface: parts.surface,
            diags: parts.diags,
        }
    }
}

fn idx_to_usize<T>(idx: Idx<T>) -> usize {
    usize::try_from(idx.raw()).unwrap_or(usize::MAX)
}
