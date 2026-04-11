pub(crate) mod api;
mod checker;
mod diag;
mod effects;

use crate::api::{ClassFacts, ExprFacts, ForeignLinkInfo, InstanceFacts, PatFacts};
use music_hir::{HirExprId, HirTyId};
use music_module::ModuleKey;
use music_names::NameBindingId;
use music_resolve::ResolvedModule;
use std::collections::{HashMap, HashSet};

pub use api::{
    Attr, AttrArg, AttrRecordField, AttrValue, ClassMemberSurface, ClassSurface, ConstraintKind,
    ConstraintSurface, DataSurface, DataVariantSurface, DefinitionKey, EffectOpSurface,
    EffectSurface, ExportedValue, InstanceSurface, ModuleSurface, SemaDataDef, SemaDataVariantDef,
    SemaDiagList, SemaEffectDef, SemaEffectOpDef, SemaEnv, SemaModule, SemaOptions, SurfaceDim,
    SurfaceEffectItem, SurfaceEffectRow, SurfaceTy, SurfaceTyField, SurfaceTyId, SurfaceTyKind,
    TargetInfo, sema_diag_kind,
};
pub use checker::check_module;
pub use diag::SemaDiagKind;
pub use effects::{EffectKey, EffectRow};

pub(crate) struct SemaContextBuild {
    pub target: Option<TargetInfo>,
    pub gated_bindings: HashSet<NameBindingId>,
    pub foreign_links: HashMap<NameBindingId, ForeignLinkInfo>,
    pub binding_types: HashMap<NameBindingId, HirTyId>,
}

pub(crate) struct SemaFactsBuild {
    pub expr_facts: Vec<ExprFacts>,
    pub pat_facts: Vec<PatFacts>,
    pub expr_module_targets: HashMap<HirExprId, ModuleKey>,
    pub type_test_targets: HashMap<HirExprId, HirTyId>,
}

pub(crate) struct SemaDeclsBuild {
    pub effect_defs: HashMap<Box<str>, SemaEffectDef>,
    pub data_defs: HashMap<Box<str>, SemaDataDef>,
    pub class_facts: HashMap<HirExprId, ClassFacts>,
    pub instance_facts: HashMap<HirExprId, InstanceFacts>,
}

pub(crate) struct SemaModuleBuild {
    pub resolved: ResolvedModule,
    pub context: SemaContextBuild,
    pub facts: SemaFactsBuild,
    pub decls: SemaDeclsBuild,
    pub surface: ModuleSurface,
    pub diags: SemaDiagList,
}

pub(crate) fn build_sema_module(build: SemaModuleBuild) -> SemaModule {
    build.into()
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
