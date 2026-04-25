use std::collections::{HashMap, HashSet};

use music_arena::Idx;
use music_base::diag::Diag;
use music_hir::{HirExprId, HirModule, HirPatId, HirTy, HirTyId};
use music_module::ModuleKey;
use music_names::{NameBindingId, Symbol};
use music_resolve::ResolvedModule;

use crate::effects::EffectRow;
use crate::{BindingScheme, SemaModuleBuild};

use super::{
    ComptimeValue, ConstraintAnswer, ConstraintKey, ExprFacts, ExprMemberFact, ForeignLinkInfo,
    GivenFacts, ModuleSurface, PatFacts, SemaDataDef, SemaDiagList, SemaEffectDef, ShapeFacts,
    TargetInfo,
};

#[derive(Debug)]
pub struct SemaModule {
    resolved: ResolvedModule,
    target: Option<TargetInfo>,
    gated_bindings: HashSet<NameBindingId>,
    foreign_links: HashMap<NameBindingId, ForeignLinkInfo>,
    binding_types: HashMap<NameBindingId, HirTyId>,
    binding_schemes: HashMap<NameBindingId, BindingScheme>,
    binding_constraint_keys: HashMap<NameBindingId, Box<[ConstraintKey]>>,
    binding_import_record_targets: HashMap<NameBindingId, ModuleKey>,
    binding_comptime_values: HashMap<NameBindingId, ComptimeValue>,
    expr_facts: Box<[ExprFacts]>,
    pat_facts: Box<[PatFacts]>,
    expr_import_record_targets: HashMap<HirExprId, ModuleKey>,
    type_test_targets: HashMap<HirExprId, HirTyId>,
    expr_constraint_answers: HashMap<HirExprId, Box<[ConstraintAnswer]>>,
    expr_dot_callable_bindings: HashMap<HirExprId, NameBindingId>,
    expr_member_facts: HashMap<HirExprId, ExprMemberFact>,
    expr_comptime_values: HashMap<HirExprId, ComptimeValue>,
    effect_defs: HashMap<Box<str>, SemaEffectDef>,
    data_defs: HashMap<Box<str>, SemaDataDef>,
    shape_facts: HashMap<HirExprId, ShapeFacts>,
    given_facts: HashMap<HirExprId, GivenFacts>,
    surface: ModuleSurface,
    diags: SemaDiagList,
}

struct SemaContextTables {
    target: Option<TargetInfo>,
    gated_bindings: HashSet<NameBindingId>,
    foreign_links: HashMap<NameBindingId, ForeignLinkInfo>,
    binding_types: HashMap<NameBindingId, HirTyId>,
    binding_schemes: HashMap<NameBindingId, BindingScheme>,
    binding_constraint_keys: HashMap<NameBindingId, Box<[ConstraintKey]>>,
    binding_import_record_targets: HashMap<NameBindingId, ModuleKey>,
    binding_comptime_values: HashMap<NameBindingId, ComptimeValue>,
}

struct SemaFactTables {
    expr_facts: Vec<ExprFacts>,
    pat_facts: Vec<PatFacts>,
    expr_import_record_targets: HashMap<HirExprId, ModuleKey>,
    type_test_targets: HashMap<HirExprId, HirTyId>,
    expr_constraint_answers: HashMap<HirExprId, Box<[ConstraintAnswer]>>,
    expr_dot_callable_bindings: HashMap<HirExprId, NameBindingId>,
    expr_member_facts: HashMap<HirExprId, ExprMemberFact>,
    expr_comptime_values: HashMap<HirExprId, ComptimeValue>,
}

struct SemaDeclTables {
    effect_defs: HashMap<Box<str>, SemaEffectDef>,
    data_defs: HashMap<Box<str>, SemaDataDef>,
    shape_facts: HashMap<HirExprId, ShapeFacts>,
    given_facts: HashMap<HirExprId, GivenFacts>,
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
            expr_import_record_targets,
            type_test_targets,
            expr_constraint_answers,
            expr_dot_callable_bindings,
            expr_member_facts,
            expr_comptime_values,
        } = build_facts;
        let context = SemaContextTables {
            target: build_context.target,
            gated_bindings: build_context.gated_bindings,
            foreign_links: build_context.foreign_links,
            binding_types: build_context.binding_types,
            binding_schemes: build_context.binding_schemes,
            binding_constraint_keys: build_context.binding_constraint_keys,
            binding_import_record_targets: build_context.binding_import_record_targets,
            binding_comptime_values: build_context.binding_comptime_values,
        };
        let facts = SemaFactTables {
            expr_facts,
            pat_facts,
            expr_import_record_targets,
            type_test_targets,
            expr_constraint_answers,
            expr_dot_callable_bindings,
            expr_member_facts,
            expr_comptime_values,
        };
        let decls = SemaDeclTables {
            effect_defs: build_decls.effect_defs,
            data_defs: build_decls.data_defs,
            shape_facts: build_decls.shape_facts,
            given_facts: build_decls.given_facts,
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
    pub fn expr_import_record_target(&self, id: HirExprId) -> Option<&ModuleKey> {
        self.expr_import_record_targets.get(&id)
    }

    #[must_use]
    pub fn type_test_target(&self, id: HirExprId) -> Option<HirTyId> {
        self.type_test_targets.get(&id).copied()
    }

    #[must_use]
    pub fn expr_constraint_answers(&self, id: HirExprId) -> Option<&[ConstraintAnswer]> {
        self.expr_constraint_answers.get(&id).map(Box::as_ref)
    }

    #[must_use]
    pub fn expr_dot_callable_binding(&self, id: HirExprId) -> Option<NameBindingId> {
        self.expr_dot_callable_bindings.get(&id).copied()
    }

    #[must_use]
    pub fn expr_member_fact(&self, id: HirExprId) -> Option<&ExprMemberFact> {
        self.expr_member_facts.get(&id)
    }

    #[must_use]
    pub fn expr_comptime_value(&self, id: HirExprId) -> Option<&ComptimeValue> {
        self.expr_comptime_values.get(&id)
    }

    pub fn set_expr_comptime_value(&mut self, id: HirExprId, value: ComptimeValue) {
        let _prev = self.expr_comptime_values.insert(id, value);
    }
}

impl SemaModule {
    #[must_use]
    pub fn is_gated_binding(&self, binding: NameBindingId) -> bool {
        self.gated_bindings.contains(&binding)
    }

    #[must_use]
    pub fn foreign_link(&self, binding: NameBindingId) -> Option<&ForeignLinkInfo> {
        self.foreign_links.get(&binding)
    }

    #[must_use]
    pub fn binding_import_record_target(&self, binding: NameBindingId) -> Option<&ModuleKey> {
        self.binding_import_record_targets.get(&binding)
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
    pub fn binding_constraint_keys(&self, binding: NameBindingId) -> Option<&[ConstraintKey]> {
        self.binding_constraint_keys.get(&binding).map(Box::as_ref)
    }

    #[must_use]
    pub fn binding_comptime_value(&self, binding: NameBindingId) -> Option<&ComptimeValue> {
        self.binding_comptime_values.get(&binding)
    }
}

impl SemaModule {
    #[must_use]
    pub fn try_pat_ty(&self, id: HirPatId) -> Option<HirTyId> {
        self.pat_facts.get(idx_to_usize(id)).map(|facts| facts.ty)
    }

    #[must_use]
    pub fn shape_facts(&self, id: HirExprId) -> Option<&ShapeFacts> {
        self.shape_facts.get(&id)
    }

    #[must_use]
    pub fn shape_facts_by_name(&self, name: Symbol) -> Option<&ShapeFacts> {
        self.shape_facts.values().find(|facts| facts.name == name)
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
    pub fn given_facts(&self, id: HirExprId) -> Option<&GivenFacts> {
        self.given_facts.get(&id)
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
            binding_constraint_keys: context.binding_constraint_keys,
            binding_import_record_targets: context.binding_import_record_targets,
            binding_comptime_values: context.binding_comptime_values,
            expr_facts: facts.expr_facts.into_boxed_slice(),
            pat_facts: facts.pat_facts.into_boxed_slice(),
            expr_import_record_targets: facts.expr_import_record_targets,
            type_test_targets: facts.type_test_targets,
            expr_constraint_answers: facts.expr_constraint_answers,
            expr_dot_callable_bindings: facts.expr_dot_callable_bindings,
            expr_member_facts: facts.expr_member_facts,
            expr_comptime_values: facts.expr_comptime_values,
            effect_defs: decls.effect_defs,
            data_defs: decls.data_defs,
            shape_facts: decls.shape_facts,
            given_facts: decls.given_facts,
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
