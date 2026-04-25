use std::collections::HashMap;

use music_hir::{HirExprId, HirTyId};
use music_module::ModuleKey;
use music_names::{NameBindingId, Symbol};

use crate::api::{ComptimeValue, ConstraintKey, GivenFacts, SemaDiagList, ShapeFacts};
use crate::checker::schemes::BindingScheme;

use super::aliases::{
    AttachedMethodMap, BindingComptimeValueMap, BindingConstIntMap, BindingConstraintKeyMap,
    BindingEffectsMap, BindingImportRecordTargetMap, BindingSchemeMap, BindingTypeMap, DataDefMap,
    EffectDefMap, ExprCallableEffectsMap, ExprComptimeValueMap, ExprConstraintAnswerMap,
    ExprDotCallableBindingMap, ExprFactsList, ExprImportRecordTargetMap, ExprMemberFactMap,
    ForeignLinkMap, GatedBindingSet, GivenFactsMap, PatFactsList, ResumeCtxList, SealedShapeSet,
    ShapeFactsByNameMap, ShapeFactsMap, ShapeIndexMap, TypeParamKindScopeList, TypeTestTargetMap,
    UnsafeBindingSet,
};

use super::{DataDef, EffectDef};

#[derive(Debug, Clone)]
pub struct ResumeCtx {
    pub arg: HirTyId,
    pub result: HirTyId,
}

impl ResumeCtx {
    #[must_use]
    pub const fn new(arg: HirTyId, result: HirTyId) -> Self {
        Self { arg, result }
    }
}

#[derive(Default)]
pub struct TypingState {
    pub binding_types: BindingTypeMap,
    pub binding_effects: BindingEffectsMap,
    pub binding_schemes: BindingSchemeMap,
    pub type_param_kind_scopes: TypeParamKindScopeList,
    pub binding_constraint_keys: BindingConstraintKeyMap,
    pub binding_import_record_targets: BindingImportRecordTargetMap,
    pub binding_const_ints: BindingConstIntMap,
    pub binding_comptime_values: BindingComptimeValueMap,
    pub sealed_shapes: SealedShapeSet,
    pub gated_bindings: GatedBindingSet,
    pub foreign_links: ForeignLinkMap,
    pub unsafe_bindings: UnsafeBindingSet,
    pub attached_methods: AttachedMethodMap,
    pub next_open_row_id: u32,
}

impl TypingState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

#[derive(Default)]
pub struct DeclState {
    pub effect_defs: EffectDefMap,
    pub data_defs: DataDefMap,
    pub shape_index: ShapeIndexMap,
    pub shape_facts_by_name: ShapeFactsByNameMap,
    pub shape_facts: ShapeFactsMap,
    pub given_facts: GivenFactsMap,
}

impl DeclState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

pub struct FactState {
    pub diags: SemaDiagList,
    pub expr_facts: ExprFactsList,
    pub pat_facts: PatFactsList,
    pub expr_callable_effects: ExprCallableEffectsMap,
    pub expr_import_record_targets: ExprImportRecordTargetMap,
    pub type_test_targets: TypeTestTargetMap,
    pub expr_constraint_answers: ExprConstraintAnswerMap,
    pub expr_dot_callable_bindings: ExprDotCallableBindingMap,
    pub expr_member_facts: ExprMemberFactMap,
    pub expr_comptime_values: ExprComptimeValueMap,
}

impl FactState {
    #[must_use]
    pub fn new(expr_facts: ExprFactsList, pat_facts: PatFactsList) -> Self {
        Self {
            diags: Vec::new(),
            expr_facts,
            pat_facts,
            expr_callable_effects: HashMap::new(),
            expr_import_record_targets: HashMap::new(),
            type_test_targets: HashMap::new(),
            expr_constraint_answers: HashMap::new(),
            expr_dot_callable_bindings: HashMap::new(),
            expr_member_facts: HashMap::new(),
            expr_comptime_values: HashMap::new(),
        }
    }
}

#[derive(Default)]
pub struct ResumeState {
    pub stack: ResumeCtxList,
}

impl ResumeState {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }
}

impl TypingState {
    pub const fn binding_types(&self) -> &HashMap<NameBindingId, HirTyId> {
        &self.binding_types
    }

    pub const fn binding_schemes(&self) -> &HashMap<NameBindingId, BindingScheme> {
        &self.binding_schemes
    }

    pub const fn binding_constraint_keys(&self) -> &HashMap<NameBindingId, Box<[ConstraintKey]>> {
        &self.binding_constraint_keys
    }

    pub const fn binding_import_record_targets(&self) -> &HashMap<NameBindingId, ModuleKey> {
        &self.binding_import_record_targets
    }

    pub const fn binding_const_ints(&self) -> &HashMap<NameBindingId, i64> {
        &self.binding_const_ints
    }

    pub const fn binding_comptime_values(&self) -> &HashMap<NameBindingId, ComptimeValue> {
        &self.binding_comptime_values
    }

    pub fn is_gated_binding(&self, id: NameBindingId) -> bool {
        self.gated_bindings.contains(&id)
    }
}

impl DeclState {
    pub fn effect_def(&self, name: &str) -> Option<&EffectDef> {
        self.effect_defs.get(name)
    }

    pub fn data_def(&self, name: &str) -> Option<&DataDef> {
        self.data_defs.get(name)
    }

    pub const fn shape_facts_by_name(&self) -> &HashMap<Symbol, ShapeFacts> {
        &self.shape_facts_by_name
    }

    pub const fn given_facts(&self) -> &HashMap<HirExprId, GivenFacts> {
        &self.given_facts
    }
}
