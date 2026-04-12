use std::collections::{BTreeSet, HashMap};

use music_arena::SliceRange;
use music_hir::{
    HirBinder, HirConstraint, HirExprId, HirMemberDef, HirMemberKind, HirOrigin, HirTyId, HirTyKind,
};
use music_module::ModuleKey;
use music_names::Symbol;

use super::super::exprs::check_expr;
use super::super::surface::{canonical_surface_ty, surface_key};
use super::super::{CheckPass, DiagKind};
use super::declarations::member_signature;
use crate::api::{ClassMemberFacts, DefinitionKey, ExprFacts, InstanceFacts};
use crate::effects::EffectRow;

type MemberDefRange = SliceRange<HirMemberDef>;

impl CheckPass<'_, '_, '_> {
    fn class_member_map(&self, class_name: Symbol) -> HashMap<Symbol, ClassMemberFacts> {
        self.class_facts_by_name(class_name)
            .map(|facts| {
                facts
                    .members
                    .iter()
                    .map(|member| (member.name, member.clone()))
                    .collect::<HashMap<_, _>>()
            })
            .unwrap_or_default()
    }

    fn check_instance_member(
        &mut self,
        member: &HirMemberDef,
        expected_members: &HashMap<Symbol, ClassMemberFacts>,
    ) {
        let signature = member_signature(self, member, true);
        if let Some(expected) = expected_members.get(&member.name.name) {
            if expected.params.len() != signature.params.len() {
                self.diag(
                    member.origin.span,
                    DiagKind::InstanceMemberArityMismatch,
                    "",
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
            self.diag(member.origin.span, DiagKind::UnknownInstanceMember, "");
        }
        if let Some(value) = member.value {
            let facts = check_expr(self, value);
            let origin = self.expr(value).origin;
            self.type_mismatch(origin, signature.result, facts.ty);
        } else {
            self.diag(
                member.origin.span,
                DiagKind::InstanceMemberValueRequired,
                "",
            );
        }
    }

    fn check_instance_member_set(
        &mut self,
        origin: HirOrigin,
        members: &[HirMemberDef],
        expected_members: &HashMap<Symbol, ClassMemberFacts>,
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
                self.diag(member.origin.span, DiagKind::DuplicateInstanceMember, "");
            }
            self.check_instance_member(member, expected_members);
        }
        for expected in expected_members.keys() {
            if !seen_members.contains(expected) {
                self.diag(origin.span, DiagKind::MissingInstanceMember, "");
            }
        }
        member_names.into_boxed_slice()
    }

    pub(in super::super) fn check_instance_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        type_params: SliceRange<HirBinder>,
        constraints: SliceRange<HirConstraint>,
        class: HirExprId,
        members: &MemberDefRange,
    ) -> ExprFacts {
        let class_origin = self.expr(class).origin;
        let class_ty = self.lower_type_expr(class, class_origin);
        let (class_name, class_args) =
            if let HirTyKind::Named { name, args } = self.ty(class_ty).kind {
                (name, self.ty_ids(args).into_boxed_slice())
            } else {
                self.diag(origin.span, DiagKind::InvalidInstanceTarget, "");
                (
                    self.known().unknown,
                    Vec::<HirTyId>::new().into_boxed_slice(),
                )
            };

        let class_key = self.class_facts_by_name(class_name).map_or_else(
            || surface_key(self.module_key(), self.interner(), class_name),
            |facts| facts.key.clone(),
        );
        if self.is_sealed_class(&class_key) && class_key.module != *self.module_key() {
            self.diag(origin.span, DiagKind::SealedClass, "");
        }

        if self.class_id(class_name).is_none() && self.class_facts_by_name(class_name).is_none() {
            self.diag(origin.span, DiagKind::UnknownClass, "");
        }

        let members_vec = self.members((*members).clone());
        let expected_members = self.class_member_map(class_name);
        let member_names = self.check_instance_member_set(origin, &members_vec, &expected_members);
        let type_params = self
            .binders(type_params)
            .into_iter()
            .map(|binder| binder.name.name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let constraints = self.lower_constraints(constraints);
        self.insert_instance_facts(
            expr_id,
            InstanceFacts::new(origin, class_key, class_name, class_args, member_names)
                .with_type_params(type_params)
                .with_constraints(constraints),
        );
        ExprFacts::new(class_ty, EffectRow::empty())
    }

    pub(in super::super) fn check_instance_coherence(&mut self) {
        let Some(env) = self.sema_env() else {
            return;
        };
        let mut seen = HashMap::<(DefinitionKey, Box<[String]>), ModuleKey>::new();
        let local_instances = self.instance_facts().values().cloned().collect::<Vec<_>>();

        for facts in local_instances {
            let args = facts
                .class_args
                .iter()
                .copied()
                .map(|ty| self.render_ty(ty))
                .collect::<Vec<_>>()
                .into_boxed_slice();
            let key = (facts.class_key.clone(), args);
            if seen.insert(key, self.module_key().clone()).is_some() {
                self.diag(facts.origin.span, DiagKind::DuplicateInstance, "");
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
            for instance in surface.exported_instances() {
                let args = instance
                    .class_args
                    .iter()
                    .copied()
                    .map(|ty| canonical_surface_ty(&surface, ty))
                    .collect::<Vec<_>>()
                    .into_boxed_slice();
                let key = (instance.class_key.clone(), args);
                if seen.insert(key, module.clone()).is_some() {
                    self.diag(root_span, DiagKind::DuplicateInstance, "");
                }
            }
        }
    }
}
