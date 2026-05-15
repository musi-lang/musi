use crate::api::{ConstraintEvidence, ConstraintKey};
use crate::checker::CheckPass;

impl CheckPass<'_, '_, '_> {
    pub(super) fn resolve_available_evidence(
        &self,
        key: &ConstraintKey,
    ) -> Option<ConstraintEvidence> {
        self.resolve_in_scope_evidence(key)
            .or_else(|| self.resolve_equivalent_in_scope_evidence(key))
    }

    pub(super) fn resolve_equivalent_in_scope_evidence(
        &self,
        key: &ConstraintKey,
    ) -> Option<ConstraintEvidence> {
        self.evidence_entries_in_scope()
            .into_iter()
            .find_map(|(candidate, evidence)| {
                (candidate.kind == key.kind
                    && candidate.shape_key == key.shape_key
                    && self.ty_matches(candidate.subject, key.subject)
                    && self.ty_matches(candidate.value, key.value))
                .then_some(evidence)
            })
    }
}
