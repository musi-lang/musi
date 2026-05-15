pub(crate) mod api;
mod checker;
mod diag;

use crate::api::{ExprFacts, ForeignLinkInfo, PatFacts, ShapeFacts};
use music_hir::{HirExprId, HirTyId};
use music_module::ModuleKey;
use music_names::NameBindingId;
use music_resolve::ResolvedModule;
use std::collections::{HashMap, HashSet};

pub use api::{
    Attr, AttrArg, AttrRecordField, AttrValue, ComptimeClosureValue, ComptimeDataValue,
    ComptimeForeignValue, ComptimeImportRecordValue, ComptimeSeqValue, ComptimeShapeValue,
    ComptimeTypeValue, ComptimeValue, ComptimeValueList, ConstraintEvidence, ConstraintKey,
    ConstraintKind, ConstraintSurface, DataSurface, DataVariantSurface, DefinitionKey,
    ExportedValue, ExprMemberFact, ExprMemberKind, LawFacts, LawParamFacts, LawParamSurface,
    LawSurface, ModuleSurface, SemaDataDef, SemaDataVariantDef, SemaDiagList, SemaEnv, SemaModule,
    SemaOptions, ShapeMemberSurface, ShapeSurface, SurfaceDim, SurfaceTy, SurfaceTyField,
    SurfaceTyId, SurfaceTyKind, TargetInfo, sema_diag_kind,
};
pub use checker::check_module;
pub use checker::schemes::BindingScheme;
pub use diag::SemaDiagKind;

pub(crate) struct SemaContextBuild {
    pub target: Option<TargetInfo>,
    pub gated_bindings: HashSet<NameBindingId>,
    pub foreign_links: HashMap<NameBindingId, ForeignLinkInfo>,
    pub binding_types: HashMap<NameBindingId, HirTyId>,
    pub binding_schemes: HashMap<NameBindingId, BindingScheme>,
    pub binding_constraint_keys: HashMap<NameBindingId, Box<[ConstraintKey]>>,
    pub binding_import_record_targets: HashMap<NameBindingId, ModuleKey>,
    pub binding_comptime_values: HashMap<NameBindingId, ComptimeValue>,
}

pub(crate) struct SemaFactsBuild {
    pub expr_facts: Vec<ExprFacts>,
    pub pat_facts: Vec<PatFacts>,
    pub expr_import_record_targets: HashMap<HirExprId, ModuleKey>,
    pub type_test_targets: HashMap<HirExprId, HirTyId>,
    pub expr_constraint_evidence: HashMap<HirExprId, Box<[ConstraintEvidence]>>,
    pub expr_dot_callable_bindings: HashMap<HirExprId, NameBindingId>,
    pub expr_member_facts: HashMap<HirExprId, ExprMemberFact>,
    pub expr_comptime_values: HashMap<HirExprId, ComptimeValue>,
}

pub(crate) struct SemaDeclsBuild {
    pub data_defs: HashMap<Box<str>, SemaDataDef>,
    pub shape_facts: HashMap<HirExprId, ShapeFacts>,
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
