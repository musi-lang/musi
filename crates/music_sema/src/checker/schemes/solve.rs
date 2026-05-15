use std::collections::HashMap;

use music_hir::{HirOrigin, HirTyId};
use music_names::Symbol;

use crate::api::{ConstraintEvidence, ConstraintKind};
use crate::checker::{CheckPass, DiagKind};

use super::ConstraintObligation;

impl CheckPass<'_, '_, '_> {
    pub(in crate::checker) fn unify_ty_for_type_params(
        &mut self,
        type_params: &[Symbol],
        pattern: HirTyId,
        actual: HirTyId,
        subst: &mut HashMap<Symbol, HirTyId>,
    ) -> bool {
        self.unify_ty(type_params, pattern, actual, subst)
    }

    pub(super) fn solve_obligation(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
    ) -> bool {
        let ctx = self;
        match obligation.kind {
            ConstraintKind::TypeEq => {
                if ctx.ty_matches(obligation.subject, obligation.value)
                    && ctx.ty_matches(obligation.value, obligation.subject)
                {
                    return true;
                }
                ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
                false
            }
            ConstraintKind::Implements => ctx.solve_implements(origin, obligation),
        }
    }

    pub(super) fn resolve_obligation_evidence(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
    ) -> Option<ConstraintEvidence> {
        match obligation.kind {
            ConstraintKind::TypeEq => {
                self.solve_obligation(origin, obligation)
                    .then_some(ConstraintEvidence::Param {
                        key: obligation.key(),
                    })
            }
            ConstraintKind::Implements => self.resolve_implements_evidence(origin, obligation),
        }
    }

    pub(super) fn solve_implements(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
    ) -> bool {
        if self.resolve_available_evidence(&obligation.key()).is_some() {
            return true;
        }
        self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
        false
    }

    pub(super) fn resolve_implements_evidence(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
    ) -> Option<ConstraintEvidence> {
        let key = obligation.key();
        if let Some(evidence) = self.resolve_available_evidence(&key) {
            return Some(evidence);
        }
        self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
        None
    }
}
