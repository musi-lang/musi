use music_arena::SliceRange;
use music_hir::{HirConstraint, HirConstraintKind, HirTyId, HirTyKind};

use crate::api::{ConstraintFacts, ConstraintKind, DefinitionKey};
use crate::checker::PassBase;
use crate::checker::surface::surface_key;

impl PassBase<'_, '_, '_> {
    pub fn lower_constraints(
        &mut self,
        constraints: SliceRange<HirConstraint>,
    ) -> Box<[ConstraintFacts]> {
        self.constraints(constraints)
            .into_iter()
            .map(|constraint| {
                let kind = match constraint.kind {
                    HirConstraintKind::Subtype => ConstraintKind::Subtype,
                    HirConstraintKind::Implements => ConstraintKind::Implements,
                    HirConstraintKind::TypeEq => ConstraintKind::TypeEq,
                };
                let constraint_value = {
                    let origin = self.expr(constraint.value).origin;
                    self.lower_type_expr(constraint.value, origin)
                };
                {
                    let lowered =
                        ConstraintFacts::new(constraint.name.name, kind, constraint_value);
                    if let Some(shape_key) = self.constraint_shape_key(kind, constraint_value) {
                        lowered.with_shape_key(shape_key)
                    } else {
                        lowered
                    }
                }
            })
            .collect::<Vec<_>>()
            .into_boxed_slice()
    }

    pub(super) fn constraint_shape_key(
        &self,
        kind: ConstraintKind,
        value: HirTyId,
    ) -> Option<DefinitionKey> {
        if kind != ConstraintKind::Implements {
            return None;
        }
        let HirTyKind::Named { name, .. } = self.ty(value).kind else {
            return None;
        };
        Some(self.shape_facts_by_name(name).map_or_else(
            || surface_key(self.module_key(), self.interner(), name),
            |facts| facts.key.clone(),
        ))
    }
}
