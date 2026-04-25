use super::arena::idx_to_usize;
use music_hir::{HirExprId, HirPatId, HirTyId};
use music_module::ModuleKey;
use music_names::NameBindingId;

use crate::api::{ComptimeValue, ConstraintAnswer, ExprFacts, ExprMemberFact, PatFacts};
use crate::checker::state::PassBase;
use crate::effects::EffectRow;

impl PassBase<'_, '_, '_> {
    pub fn set_expr_facts(&mut self, id: HirExprId, facts: ExprFacts) {
        let slot = self
            .facts
            .expr_facts
            .get_mut(idx_to_usize(id))
            .expect("expr facts slot missing");
        *slot = facts;
    }

    pub fn expr_import_record_target(&self, id: HirExprId) -> Option<&ModuleKey> {
        self.facts.expr_import_record_targets.get(&id)
    }

    pub fn set_expr_import_record_target(&mut self, id: HirExprId, target: ModuleKey) {
        let _prev = self.facts.expr_import_record_targets.insert(id, target);
    }

    pub fn expr_callable_effects(&self, id: HirExprId) -> Option<EffectRow> {
        self.facts.expr_callable_effects.get(&id).cloned()
    }

    pub fn set_expr_callable_effects(&mut self, id: HirExprId, effects: EffectRow) {
        let _prev = self.facts.expr_callable_effects.insert(id, effects);
    }

    pub fn set_type_test_target(&mut self, id: HirExprId, target: HirTyId) {
        let _prev = self.facts.type_test_targets.insert(id, target);
    }

    pub fn set_expr_constraint_answers(
        &mut self,
        id: HirExprId,
        answers: impl Into<Box<[ConstraintAnswer]>>,
    ) {
        let _prev = self
            .facts
            .expr_constraint_answers
            .insert(id, answers.into());
    }

    pub fn set_expr_dot_callable_binding(&mut self, id: HirExprId, binding: NameBindingId) {
        let _prev = self.facts.expr_dot_callable_bindings.insert(id, binding);
    }

    pub fn set_expr_member_fact(&mut self, id: HirExprId, fact: ExprMemberFact) {
        let _prev = self.facts.expr_member_facts.insert(id, fact);
    }

    pub fn expr_member_fact(&self, id: HirExprId) -> Option<&ExprMemberFact> {
        self.facts.expr_member_facts.get(&id)
    }

    pub fn set_expr_comptime_value(&mut self, id: HirExprId, value: ComptimeValue) {
        let _prev = self.facts.expr_comptime_values.insert(id, value);
    }

    pub fn expr_dot_callable_binding(&self, id: HirExprId) -> Option<NameBindingId> {
        self.facts.expr_dot_callable_bindings.get(&id).copied()
    }

    pub fn set_pat_facts(&mut self, id: HirPatId, facts: PatFacts) {
        let slot = self
            .facts
            .pat_facts
            .get_mut(idx_to_usize(id))
            .expect("pat facts slot missing");
        *slot = facts;
    }
}
