use std::collections::BTreeSet;

use music_hir::{HirOrigin, HirTyId, HirTyKind};
use music_module::ModuleKey;

use crate::api::{ConstraintAnswer, ConstraintKey, DefinitionKey, ModuleSurface};
use crate::checker::surface::surface_key;
use crate::checker::{CheckPass, DiagKind};

use super::ConstraintObligation;

struct SurfaceGivenEvidenceScan<'a> {
    origin: HirOrigin,
    shape_key: &'a DefinitionKey,
    shape_args: &'a [HirTyId],
    stack: &'a mut Vec<String>,
    matches: &'a mut Vec<ConstraintAnswer>,
    module_key: &'a ModuleKey,
    surface: &'a ModuleSurface,
}

impl CheckPass<'_, '_, '_> {
    pub(super) fn resolve_available_answer(&self, key: &ConstraintKey) -> Option<ConstraintAnswer> {
        self.resolve_in_scope_answer(key)
            .or_else(|| self.resolve_equivalent_in_scope_answer(key))
    }

    pub(super) fn constraint_stack_frame(
        &self,
        shape_key: &DefinitionKey,
        shape_args: &[HirTyId],
    ) -> String {
        format!(
            "{}:{}",
            shape_key.name,
            shape_args
                .iter()
                .copied()
                .map(|arg| self.render_ty(arg))
                .collect::<Vec<_>>()
                .join(", ")
        )
    }

    pub(super) fn collect_local_given_answers(
        &mut self,
        origin: HirOrigin,
        shape_key: &DefinitionKey,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> Vec<ConstraintAnswer> {
        let local_givens = self.given_facts().values().cloned().collect::<Vec<_>>();
        local_givens
            .iter()
            .filter(|given| given.shape_key == *shape_key)
            .filter_map(|given| {
                self.given_provider_answer(
                    origin,
                    given,
                    self.module_key().clone(),
                    shape_args,
                    stack,
                )
            })
            .collect()
    }

    pub(super) fn collect_imported_given_answers(
        &mut self,
        origin: HirOrigin,
        shape_key: &DefinitionKey,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
        matches: &mut Vec<ConstraintAnswer>,
    ) {
        let Some(env) = self.sema_env() else {
            return;
        };
        let mut visited = BTreeSet::new();
        let mut pending = self.static_imports();
        while let Some(module_key) = pending.pop() {
            if !visited.insert(module_key.clone()) {
                continue;
            }
            let Some(surface) = env.module_surface(&module_key) else {
                continue;
            };
            pending.extend(surface.static_imports().iter().cloned());
            self.collect_surface_given_answers(SurfaceGivenEvidenceScan {
                origin,
                shape_key,
                shape_args,
                stack,
                matches,
                module_key: &module_key,
                surface: &surface,
            });
        }
    }

    fn collect_surface_given_answers(&mut self, scan: SurfaceGivenEvidenceScan<'_>) {
        let SurfaceGivenEvidenceScan {
            origin,
            shape_key,
            shape_args,
            stack,
            matches,
            module_key,
            surface,
        } = scan;
        for given in surface.exported_givens() {
            if given.shape_key != *shape_key {
                continue;
            }
            let imported = self.given_facts_from_surface(surface, given);
            if let Some(answer) =
                self.given_provider_answer(origin, &imported, module_key.clone(), shape_args, stack)
            {
                matches.push(answer);
            }
        }
    }

    pub(super) fn finish_given_answers(
        &mut self,
        origin: HirOrigin,
        mut matches: Vec<ConstraintAnswer>,
    ) -> Option<ConstraintAnswer> {
        match matches.len() {
            0 => {
                self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
                None
            }
            1 => matches.pop(),
            _ => {
                self.diag(origin.span, DiagKind::AmbiguousGivenMatch, "");
                None
            }
        }
    }

    pub(super) fn resolve_equivalent_in_scope_answer(
        &self,
        key: &ConstraintKey,
    ) -> Option<ConstraintAnswer> {
        self.answer_entries_in_scope()
            .into_iter()
            .find_map(|(candidate, answer)| {
                (candidate.kind == key.kind
                    && candidate.shape_key == key.shape_key
                    && self.ty_matches(candidate.subject, key.subject)
                    && self.ty_matches(candidate.value, key.value))
                .then_some(answer)
            })
    }

    pub(super) fn obligation_shape_target(
        &self,
        obligation: &ConstraintObligation,
    ) -> Option<(DefinitionKey, Box<[HirTyId]>)> {
        let ctx = self;
        let HirTyKind::Named { name, args } = ctx.ty(obligation.value).kind else {
            return None;
        };
        let shape_key = obligation.shape_key.clone().unwrap_or_else(|| {
            ctx.shape_facts_by_name(name).map_or_else(
                || surface_key(ctx.module_key(), ctx.interner(), name),
                |facts| facts.key.clone(),
            )
        });
        let shape_args = if ctx.ty_ids(args).is_empty() {
            vec![obligation.subject]
        } else {
            ctx.ty_ids(args)
        };
        Some((shape_key, shape_args.into_boxed_slice()))
    }
}
