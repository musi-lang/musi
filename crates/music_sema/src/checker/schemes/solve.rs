use std::collections::{BTreeSet, HashMap};

use music_hir::{HirOrigin, HirTyId};
use music_names::Symbol;

use crate::api::{ConstraintAnswer, ConstraintKind, DefinitionKey, ModuleSurface};
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
        stack: &mut Vec<String>,
    ) -> bool {
        let ctx = self;
        match obligation.kind {
            ConstraintKind::Subtype => {
                if ctx.ty_matches(obligation.value, obligation.subject) {
                    return true;
                }
                ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
                false
            }
            ConstraintKind::TypeEq => {
                if ctx.ty_matches(obligation.subject, obligation.value)
                    && ctx.ty_matches(obligation.value, obligation.subject)
                {
                    return true;
                }
                ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
                false
            }
            ConstraintKind::Implements => ctx.solve_implements(origin, obligation, stack),
        }
    }

    pub(super) fn resolve_obligation_answer(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
        stack: &mut Vec<String>,
    ) -> Option<ConstraintAnswer> {
        match obligation.kind {
            ConstraintKind::Subtype | ConstraintKind::TypeEq => self
                .solve_obligation(origin, obligation, stack)
                .then_some(ConstraintAnswer::Param {
                    key: obligation.key(),
                }),
            ConstraintKind::Implements => self.resolve_implements_answer(origin, obligation, stack),
        }
    }

    pub(super) fn solve_implements(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
        stack: &mut Vec<String>,
    ) -> bool {
        let Some((shape_key, shape_args)) = self.obligation_shape_target(obligation) else {
            self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return false;
        };
        let frame = self.constraint_stack_frame(&shape_key, &shape_args);
        if stack.contains(&frame) {
            return true;
        }
        stack.push(frame);
        let matches = self.count_matching_givens(origin, &shape_key, &shape_args, stack);
        let _ = stack.pop();
        self.finish_instance_match_count(origin, matches)
    }

    pub(super) fn count_matching_givens(
        &mut self,
        origin: HirOrigin,
        shape_key: &DefinitionKey,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> usize {
        let mut matches = self.count_local_given_matches(origin, shape_key, shape_args, stack);
        matches += self.count_imported_given_matches(origin, shape_key, shape_args, stack);
        matches
    }

    pub(super) fn count_local_given_matches(
        &mut self,
        origin: HirOrigin,
        shape_key: &DefinitionKey,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> usize {
        let local_givens = self.given_facts().values().cloned().collect::<Vec<_>>();
        local_givens
            .iter()
            .filter(|given| given.shape_key == *shape_key)
            .filter(|given| self.given_matches(origin, given, shape_args, stack))
            .count()
    }

    pub(super) fn count_imported_given_matches(
        &mut self,
        origin: HirOrigin,
        shape_key: &DefinitionKey,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> usize {
        let Some(env) = self.sema_env() else {
            return 0;
        };
        let mut matches = 0usize;
        let mut visited = BTreeSet::new();
        let mut pending = self.static_imports();
        while let Some(module_key) = pending.pop() {
            if let Some(surface) = env
                .module_surface(&module_key)
                .filter(|_| visited.insert(module_key.clone()))
            {
                pending.extend(surface.static_imports().iter().cloned());
                matches += self
                    .count_surface_given_matches(origin, shape_key, shape_args, stack, &surface);
            }
        }
        matches
    }

    pub(super) fn count_surface_given_matches(
        &mut self,
        origin: HirOrigin,
        shape_key: &DefinitionKey,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
        surface: &ModuleSurface,
    ) -> usize {
        surface
            .exported_givens()
            .iter()
            .filter(|given| given.shape_key == *shape_key)
            .filter(|given| {
                let imported = self.given_facts_from_surface(surface, given);
                self.given_matches(origin, &imported, shape_args, stack)
            })
            .count()
    }

    pub(super) fn finish_instance_match_count(
        &mut self,
        origin: HirOrigin,
        matches: usize,
    ) -> bool {
        if matches == 0 {
            self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return false;
        }
        if matches > 1 {
            self.diag(origin.span, DiagKind::AmbiguousGivenMatch, "");
            return false;
        }
        true
    }

    pub(super) fn resolve_implements_answer(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
        stack: &mut Vec<String>,
    ) -> Option<ConstraintAnswer> {
        let key = obligation.key();
        if let Some(answer) = self.resolve_available_answer(&key) {
            return Some(answer);
        }
        let Some((shape_key, shape_args)) = self.obligation_shape_target(obligation) else {
            self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return None;
        };
        let frame = self.constraint_stack_frame(&shape_key, &shape_args);
        if stack.contains(&frame) {
            self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return None;
        }
        stack.push(frame);
        let mut matches = self.collect_local_given_answers(origin, &shape_key, &shape_args, stack);
        self.collect_imported_given_answers(origin, &shape_key, &shape_args, stack, &mut matches);
        let _ = stack.pop();
        self.finish_given_answers(origin, matches)
    }
}
