use std::collections::{BTreeSet, HashMap};

use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{
    HirBinder, HirConstraint, HirExprId, HirMemberDef, HirMemberKind, HirOrigin, HirTyId, HirTyKind,
};
use music_module::ModuleKey;
use music_names::Symbol;

use super::super::exprs::check_expr;
use super::super::surface::{canonical_surface_ty, surface_key};
use super::super::{CheckPass, DiagKind};
use super::member_signature;
use crate::api::{DefinitionKey, ExprFacts, GivenFacts, ShapeMemberFacts};
use crate::effects::EffectRow;

type MemberDefRange = SliceRange<HirMemberDef>;

impl CheckPass<'_, '_, '_> {
    fn shape_member_map(
        &mut self,
        shape_name: Symbol,
        shape_args: &[HirTyId],
    ) -> HashMap<Symbol, ShapeMemberFacts> {
        let Some(facts) = self.shape_facts_by_name(shape_name).cloned() else {
            return HashMap::default();
        };
        let subst = facts
            .type_params
            .iter()
            .copied()
            .zip(shape_args.iter().copied())
            .collect::<HashMap<_, _>>();
        facts
            .members
            .iter()
            .map(|member| {
                let params = member
                    .params
                    .iter()
                    .copied()
                    .map(|param| self.substitute_ty(param, &subst))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let result = self.substitute_ty(member.result, &subst);
                (
                    member.name,
                    ShapeMemberFacts::new(member.name, params, result),
                )
            })
            .collect::<HashMap<_, _>>()
    }

    fn check_given_member(
        &mut self,
        member: &HirMemberDef,
        expected_members: &HashMap<Symbol, ShapeMemberFacts>,
    ) {
        let signature = member_signature(self, member, true);
        if let Some(expected) = expected_members.get(&member.name.name) {
            if expected.params.len() != signature.params.len() {
                let member_name = self.resolve_symbol(member.name.name).to_owned();
                let expected_count = expected.params.len();
                let found_count = signature.params.len();
                self.diag_with(
                    member.origin.span,
                    DiagKind::GivenMemberArityMismatch,
                    DiagContext::new()
                        .with("member", member_name)
                        .with("expected", expected_count)
                        .with("found", found_count),
                );
            }
            for (expected_param, actual_param) in expected
                .params
                .iter()
                .copied()
                .zip(signature.params.iter().copied())
            {
                self.type_mismatch(member.origin, expected_param, actual_param);
            }
            self.type_mismatch(member.origin, expected.result, signature.result);
        } else {
            let member_name = self.resolve_symbol(member.name.name).to_owned();
            self.diag_with(
                member.origin.span,
                DiagKind::UnknownGivenMember,
                DiagContext::new().with("member", member_name),
            );
        }
        if let Some(value) = member.value {
            let facts = check_expr(self, value);
            let origin = self.expr(value).origin;
            self.type_mismatch(origin, signature.result, facts.ty);
        } else {
            let member_name = self.resolve_symbol(member.name.name).to_owned();
            self.diag_with(
                member.origin.span,
                DiagKind::GivenMemberValueRequired,
                DiagContext::new().with("member", member_name),
            );
        }
    }

    fn check_given_member_set(
        &mut self,
        origin: HirOrigin,
        members: &[HirMemberDef],
        expected_members: &HashMap<Symbol, ShapeMemberFacts>,
    ) -> Box<[Symbol]> {
        let member_names = members
            .iter()
            .filter(|member| member.kind == HirMemberKind::Let)
            .map(|member| member.name.name)
            .collect::<Vec<_>>();
        let mut seen_members = BTreeSet::new();
        for member in members {
            if member.kind != HirMemberKind::Let {
                continue;
            }
            if !seen_members.insert(member.name.name) {
                let member_name = self.resolve_symbol(member.name.name).to_owned();
                self.diag_with(
                    member.origin.span,
                    DiagKind::DuplicateGivenMember,
                    DiagContext::new().with("member", member_name),
                );
            }
            self.check_given_member(member, expected_members);
        }
        for expected in expected_members.keys() {
            if !seen_members.contains(expected) {
                let member_name = self.resolve_symbol(*expected).to_owned();
                self.diag_with(
                    origin.span,
                    DiagKind::MissingGivenMember,
                    DiagContext::new().with("member", member_name),
                );
            }
        }
        member_names.into_boxed_slice()
    }

    pub(in super::super) fn check_given_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        type_params: SliceRange<HirBinder>,
        constraints: SliceRange<HirConstraint>,
        shape: HirExprId,
        members: &MemberDefRange,
    ) -> ExprFacts {
        let shape_origin = self.expr(shape).origin;
        let shape_ty = self.lower_type_expr(shape, shape_origin);
        let (shape_name, shape_args) =
            if let HirTyKind::Named { name, args } = self.ty(shape_ty).kind {
                (name, self.ty_ids(args).into_boxed_slice())
            } else {
                let target = self.render_ty(shape_ty);
                self.diag_with(
                    origin.span,
                    DiagKind::InvalidGivenTarget,
                    DiagContext::new().with("target", target),
                );
                (
                    self.known().unknown,
                    Vec::<HirTyId>::new().into_boxed_slice(),
                )
            };

        let shape_key = self.shape_facts_by_name(shape_name).map_or_else(
            || surface_key(self.module_key(), self.interner(), shape_name),
            |facts| facts.key.clone(),
        );
        if self.is_sealed_shape(&shape_key) && shape_key.module != *self.module_key() {
            let shape_name = self.resolve_symbol(shape_name).to_owned();
            self.diag_with(
                origin.span,
                DiagKind::SealedShape,
                DiagContext::new().with("shape", shape_name),
            );
        }

        if self.shape_id(shape_name).is_none() && self.shape_facts_by_name(shape_name).is_none() {
            let shape_name = self.resolve_symbol(shape_name).to_owned();
            self.diag_with(
                origin.span,
                DiagKind::UnknownShape,
                DiagContext::new().with("shape", shape_name),
            );
        }

        let members_vec = self.members((*members).clone());
        let expected_members = self.shape_member_map(shape_name, &shape_args);
        let member_names = self.check_given_member_set(origin, &members_vec, &expected_members);
        let type_param_kinds = self.lower_type_param_kinds(type_params);
        self.push_type_param_kinds(&type_param_kinds);
        let type_params = type_param_kinds
            .iter()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let type_param_kind_tys = type_param_kinds
            .iter()
            .map(|(_, kind)| *kind)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let constraints = self.lower_constraints(constraints);
        self.pop_type_param_kinds();
        let evidence_keys = constraints
            .iter()
            .filter_map(|constraint| self.constraint_key_for_facts(constraint))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        self.insert_given_facts(
            expr_id,
            GivenFacts::new(origin, shape_key, shape_name, shape_args, member_names)
                .with_type_params(type_params)
                .with_type_param_kinds(type_param_kind_tys)
                .with_constraints(constraints)
                .with_evidence_keys(evidence_keys),
        );
        ExprFacts::new(shape_ty, EffectRow::empty())
    }

    pub(in super::super) fn check_instance_coherence(&mut self) {
        let Some(env) = self.sema_env() else {
            return;
        };
        let mut seen = HashMap::<(DefinitionKey, Box<[String]>), ModuleKey>::new();
        let local_givens = self.given_facts().values().cloned().collect::<Vec<_>>();

        for facts in local_givens {
            let args = facts
                .shape_args
                .iter()
                .copied()
                .map(|ty| self.render_ty(ty))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let key = (facts.shape_key.clone(), args);
            if seen.insert(key, self.module_key().clone()).is_some() {
                let shape_name = self.resolve_symbol(facts.shape_name).to_owned();
                self.diag_with(
                    facts.origin.span,
                    DiagKind::DuplicateGiven,
                    DiagContext::new().with("shape", shape_name),
                );
            }
        }

        let mut visited = BTreeSet::new();
        let mut stack = self.static_imports();
        let root_span = self.expr(self.root_expr_id()).origin.span;
        while let Some(module) = stack.pop() {
            if !visited.insert(module.clone()) {
                continue;
            }
            let Some(surface) = env.module_surface(&module) else {
                continue;
            };
            stack.extend(surface.static_imports().iter().cloned());
            for given in surface.exported_givens() {
                let args = given
                    .shape_args
                    .iter()
                    .copied()
                    .map(|ty| canonical_surface_ty(&surface, ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let key = (given.shape_key.clone(), args);
                if seen.insert(key, module.clone()).is_some() {
                    let shape_name = given.shape_key.name.clone();
                    self.diag_with(
                        root_span,
                        DiagKind::DuplicateGiven,
                        DiagContext::new().with("shape", shape_name),
                    );
                }
            }
        }
    }
}
