use std::collections::{BTreeSet, HashMap};
use std::iter::repeat;

use music_arena::SliceRange;
use music_hir::{HirDim, HirOrigin, HirTyField, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::Symbol;

use crate::api::{
    ConstraintEvidence, ConstraintFacts, ConstraintKey, ConstraintKind, ConstraintSurface,
    DefinitionKey, ExportedValue, InstanceFacts, InstanceSurface, ModuleSurface, SurfaceEffectRow,
    SurfaceTyId,
};
use crate::effects::{EffectKey, EffectRow};

use super::surface::{import_surface_ty, surface_key};
use super::{CheckPass, DiagKind, PassBase};

type TypeSubstMap = HashMap<Symbol, HirTyId>;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct BindingScheme {
    pub type_params: Box<[Symbol]>,
    pub type_param_kinds: Box<[HirTyId]>,
    pub param_names: Box<[Symbol]>,
    pub comptime_params: Box<[bool]>,
    pub constraints: Box<[ConstraintFacts]>,
    pub ty: HirTyId,
    pub effects: EffectRow,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ConstraintObligation {
    pub kind: ConstraintKind,
    pub subject: HirTyId,
    pub value: HirTyId,
    pub class_key: Option<DefinitionKey>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct InstantiatedBinding {
    pub ty: HirTyId,
    pub effects: EffectRow,
    pub obligations: Box<[ConstraintObligation]>,
}

impl ConstraintObligation {
    #[must_use]
    pub fn key(&self) -> ConstraintKey {
        ConstraintKey::new(self.kind, self.subject, self.value, self.class_key.clone())
    }
}

impl PassBase<'_, '_, '_> {
    pub fn scheme_from_export(
        &mut self,
        surface: &ModuleSurface,
        export: &ExportedValue,
    ) -> BindingScheme {
        let ctx = self;
        BindingScheme {
            type_params: export
                .type_params
                .iter()
                .map(|name| ctx.intern(name))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            type_param_kinds: export
                .type_param_kinds
                .iter()
                .copied()
                .map(|ty| import_surface_ty(ctx, surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            param_names: export
                .param_names
                .iter()
                .map(|name| ctx.intern(name))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            comptime_params: export.comptime_params.clone(),
            constraints: export
                .constraints
                .iter()
                .map(|constraint| ctx.import_constraint_surface(surface, constraint))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            ty: import_surface_ty(ctx, surface, export.ty),
            effects: ctx.import_surface_effect_row(surface, &export.effects),
        }
    }

    pub fn instance_facts_from_surface(
        &mut self,
        surface: &ModuleSurface,
        instance: &InstanceSurface,
    ) -> InstanceFacts {
        let ctx = self;
        let class_name =
            ctx.class_name_from_surface(surface, instance.class_args.as_ref(), &instance.class_key);
        InstanceFacts::new(
            HirOrigin::dummy(),
            instance.class_key.clone(),
            class_name,
            instance
                .class_args
                .iter()
                .copied()
                .map(|ty| import_surface_ty(ctx, surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            Vec::<Symbol>::new().into_boxed_slice(),
        )
        .with_type_params(
            instance
                .type_params
                .iter()
                .map(|name| ctx.intern(name))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
        .with_type_param_kinds(
            instance
                .type_param_kinds
                .iter()
                .copied()
                .map(|ty| import_surface_ty(ctx, surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
        .with_constraints(
            instance
                .constraints
                .iter()
                .map(|constraint| ctx.import_constraint_surface(surface, constraint))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
    }
}

impl CheckPass<'_, '_, '_> {
    pub fn scheme_value_ty(&mut self, scheme: &BindingScheme) -> HirTyId {
        if !matches!(self.ty(scheme.ty).kind, HirTyKind::Arrow { .. }) {
            return scheme.ty;
        }
        let mut ty = scheme.ty;
        let type_ty = self.builtins().type_;
        let kinds = scheme
            .type_param_kinds
            .iter()
            .copied()
            .chain(repeat(type_ty))
            .take(scheme.type_params.len())
            .collect::<Vec<_>>();
        for (binder, binder_ty) in scheme.type_params.iter().copied().zip(kinds).rev() {
            ty = self.alloc_ty(HirTyKind::Pi {
                binder,
                binder_ty,
                body: ty,
                is_effectful: false,
            });
        }
        ty
    }

    pub fn instantiate_pi_ty(
        &mut self,
        origin: HirOrigin,
        ty: HirTyId,
        args: &[HirTyId],
    ) -> Option<InstantiatedBinding> {
        let mut binders = Vec::new();
        let mut body = ty;
        while let HirTyKind::Pi {
            binder,
            binder_ty: _,
            body: next,
            is_effectful: _,
        } = self.ty(body).kind
        {
            binders.push(binder);
            body = next;
        }
        if binders.is_empty() {
            return None;
        }
        let scheme = BindingScheme {
            type_param_kinds: Box::default(),
            type_params: binders.into_boxed_slice(),
            param_names: Box::default(),
            comptime_params: Box::default(),
            constraints: Box::default(),
            ty: body,
            effects: EffectRow::empty(),
        };
        self.instantiate_binding_scheme(origin, &scheme, args)
    }

    pub fn instantiate_binding_scheme(
        &mut self,
        origin: HirOrigin,
        scheme: &BindingScheme,
        args: &[HirTyId],
    ) -> Option<InstantiatedBinding> {
        let ctx = self;
        if scheme.type_params.len() != args.len() {
            ctx.diag(origin.span, DiagKind::TypeApplicationArityMismatch, "");
            return None;
        }
        let subst = scheme
            .type_params
            .iter()
            .copied()
            .zip(args.iter().copied())
            .collect::<TypeSubstMap>();
        Some(ctx.instantiate_binding_with_subst(scheme, &subst))
    }

    pub fn instantiate_monomorphic_scheme(
        &mut self,
        scheme: &BindingScheme,
    ) -> InstantiatedBinding {
        self.instantiate_binding_with_subst(scheme, &TypeSubstMap::new())
    }

    pub fn resolve_obligations_to_evidence(
        &mut self,
        origin: HirOrigin,
        obligations: &[ConstraintObligation],
    ) -> Option<Box<[ConstraintEvidence]>> {
        let mut stack = Vec::<String>::new();
        let mut evidence = Vec::new();
        for obligation in obligations {
            if !matches!(obligation.kind, ConstraintKind::Implements) {
                let _ = self.solve_obligation(origin, obligation, &mut stack);
                continue;
            }
            evidence.push(self.resolve_obligation_evidence(origin, obligation, &mut stack)?);
        }
        Some(evidence.into_boxed_slice())
    }

    pub fn evidence_scope_for_constraints(
        &mut self,
        constraints: &[ConstraintFacts],
    ) -> HashMap<ConstraintKey, ConstraintEvidence> {
        constraints
            .iter()
            .filter_map(|constraint| {
                self.constraint_key_for_facts(constraint).map(|key| {
                    let evidence = ConstraintEvidence::Param { key: key.clone() };
                    (key, evidence)
                })
            })
            .collect()
    }

    pub(super) fn instantiate_binding_with_subst(
        &mut self,
        scheme: &BindingScheme,
        subst: &TypeSubstMap,
    ) -> InstantiatedBinding {
        let ctx = self;
        let ty = ctx.substitute_ty(scheme.ty, subst);
        let effects = ctx.substitute_effect_row(&scheme.effects, subst);
        let obligations = scheme
            .constraints
            .iter()
            .map(|constraint| ctx.instantiate_obligation(constraint, subst))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        InstantiatedBinding {
            ty,
            effects,
            obligations,
        }
    }

    fn instantiate_obligation(
        &mut self,
        constraint: &ConstraintFacts,
        subst: &TypeSubstMap,
    ) -> ConstraintObligation {
        let ctx = self;
        ConstraintObligation {
            kind: constraint.kind,
            subject: subst
                .get(&constraint.name)
                .copied()
                .unwrap_or_else(|| ctx.named_type_for_symbol(constraint.name)),
            value: ctx.substitute_ty(constraint.value, subst),
            class_key: constraint.class_key.clone(),
        }
    }

    pub(super) fn constraint_key_for_facts(
        &mut self,
        constraint: &ConstraintFacts,
    ) -> Option<ConstraintKey> {
        matches!(constraint.kind, ConstraintKind::Implements).then(|| {
            ConstraintKey::new(
                constraint.kind,
                self.named_type_for_symbol(constraint.name),
                constraint.value,
                constraint.class_key.clone(),
            )
        })
    }
}

impl PassBase<'_, '_, '_> {
    pub fn substitute_ty(&mut self, ty: HirTyId, subst: &TypeSubstMap) -> HirTyId {
        let ctx = self;
        let kind = ctx.ty(ty).kind;
        match kind {
            HirTyKind::Named { name, args } => ctx.substitute_named_ty(name, args, subst),
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => {
                let binder_ty = ctx.substitute_ty(binder_ty, subst);
                let mut next = subst.clone();
                let _ = next.remove(&binder);
                let body = ctx.substitute_ty(body, &next);
                ctx.alloc_ty(HirTyKind::Pi {
                    binder,
                    binder_ty,
                    body,
                    is_effectful,
                })
            }
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => {
                let params = ctx.substitute_ty_list(params, subst);
                let ret = ctx.substitute_ty(ret, subst);
                ctx.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret,
                    is_effectful,
                })
            }
            HirTyKind::Sum { left, right } => {
                let left = ctx.substitute_ty(left, subst);
                let right = ctx.substitute_ty(right, subst);
                ctx.alloc_ty(HirTyKind::Sum { left, right })
            }
            HirTyKind::Tuple { items } => ctx.substitute_tuple_ty(items, subst),
            HirTyKind::Seq { item } => {
                ctx.substitute_item_ty(item, subst, |item| HirTyKind::Seq { item })
            }
            HirTyKind::Array { dims, item } => ctx.substitute_array_ty(dims, item, subst),
            HirTyKind::Range { bound } => {
                ctx.substitute_item_ty(bound, subst, |bound| HirTyKind::Range { bound })
            }
            HirTyKind::ClosedRange { bound } => {
                ctx.substitute_item_ty(bound, subst, |bound| HirTyKind::ClosedRange { bound })
            }
            HirTyKind::PartialRangeFrom { bound } => {
                ctx.substitute_item_ty(bound, subst, |bound| HirTyKind::PartialRangeFrom { bound })
            }
            HirTyKind::PartialRangeUpTo { bound } => {
                ctx.substitute_item_ty(bound, subst, |bound| HirTyKind::PartialRangeUpTo { bound })
            }
            HirTyKind::PartialRangeThru { bound } => {
                ctx.substitute_item_ty(bound, subst, |bound| HirTyKind::PartialRangeThru { bound })
            }
            HirTyKind::Handler {
                effect,
                input,
                output,
            } => ctx.substitute_handler_ty(effect, input, output, subst),
            HirTyKind::Mut { inner } => ctx.substitute_mut_ty(inner, subst),
            HirTyKind::AnyClass { class } => {
                let class = ctx.substitute_ty(class, subst);
                ctx.alloc_ty(HirTyKind::AnyClass { class })
            }
            HirTyKind::SomeClass { class } => {
                let class = ctx.substitute_ty(class, subst);
                ctx.alloc_ty(HirTyKind::SomeClass { class })
            }
            HirTyKind::Record { fields } => {
                let fields = ctx
                    .ty_fields(fields)
                    .into_iter()
                    .map(|field| HirTyField::new(field.name, ctx.substitute_ty(field.ty, subst)))
                    .collect::<Vec<_>>();
                let fields = ctx.alloc_ty_fields(fields);
                ctx.alloc_ty(HirTyKind::Record { fields })
            }
            _ => ty,
        }
    }

    fn substitute_named_ty(
        &mut self,
        name: Symbol,
        args: SliceRange<HirTyId>,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        if let Some(found) = subst.get(&name).copied() {
            if self.ty_ids(args).is_empty() {
                return found;
            }
            let args = self.substitute_ty_list(args, subst);
            return self.apply_substituted_type_constructor(found, args);
        }
        let args = self.substitute_ty_list(args, subst);
        self.alloc_ty(HirTyKind::Named { name, args })
    }

    fn substitute_tuple_ty(&mut self, items: SliceRange<HirTyId>, subst: &TypeSubstMap) -> HirTyId {
        let items = self.substitute_ty_list(items, subst);
        self.alloc_ty(HirTyKind::Tuple { items })
    }

    fn substitute_item_ty(
        &mut self,
        item: HirTyId,
        subst: &TypeSubstMap,
        ctor: impl FnOnce(HirTyId) -> HirTyKind,
    ) -> HirTyId {
        let item = self.substitute_ty(item, subst);
        self.alloc_ty(ctor(item))
    }

    fn substitute_array_ty(
        &mut self,
        dims: SliceRange<HirDim>,
        item: HirTyId,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let item = self.substitute_ty(item, subst);
        self.alloc_ty(HirTyKind::Array { dims, item })
    }

    fn substitute_handler_ty(
        &mut self,
        effect: HirTyId,
        input: HirTyId,
        output: HirTyId,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let effect = self.substitute_ty(effect, subst);
        let input = self.substitute_ty(input, subst);
        let output = self.substitute_ty(output, subst);
        self.alloc_ty(HirTyKind::Handler {
            effect,
            input,
            output,
        })
    }

    fn substitute_mut_ty(&mut self, inner: HirTyId, subst: &TypeSubstMap) -> HirTyId {
        let inner = self.substitute_ty(inner, subst);
        self.alloc_ty(HirTyKind::Mut { inner })
    }

    fn substitute_ty_list(
        &mut self,
        tys: SliceRange<HirTyId>,
        subst: &TypeSubstMap,
    ) -> SliceRange<HirTyId> {
        let ctx = self;
        let tys = ctx
            .ty_ids(tys)
            .into_iter()
            .map(|ty| ctx.substitute_ty(ty, subst))
            .collect::<Vec<_>>();
        ctx.alloc_ty_list(tys)
    }

    fn apply_substituted_type_constructor(
        &mut self,
        constructor: HirTyId,
        args: SliceRange<HirTyId>,
    ) -> HirTyId {
        match self.ty(constructor).kind {
            HirTyKind::Named {
                name,
                args: existing_args,
            } => {
                let mut combined = self.ty_ids(existing_args);
                combined.extend(self.ty_ids(args));
                let args = self.alloc_ty_list(combined);
                self.alloc_ty(HirTyKind::Named { name, args })
            }
            _ if self.ty_ids(args).is_empty() => constructor,
            _ => constructor,
        }
    }

    pub fn substitute_effect_row(&mut self, row: &EffectRow, subst: &TypeSubstMap) -> EffectRow {
        let ctx = self;
        let mut items = BTreeSet::new();
        for item in &row.items {
            let _ = items.insert(EffectKey {
                name: item.name.clone(),
                arg: item.arg.map(|arg| ctx.substitute_ty(arg, subst)),
            });
        }
        EffectRow {
            items,
            open: row
                .open
                .as_deref()
                .map(|name| ctx.fresh_open_row_name(name)),
        }
    }

    fn import_constraint_surface(
        &mut self,
        surface: &ModuleSurface,
        constraint: &ConstraintSurface,
    ) -> ConstraintFacts {
        let ctx = self;
        let lowered = ConstraintFacts::new(
            ctx.intern(&constraint.name),
            constraint.kind,
            import_surface_ty(ctx, surface, constraint.value),
        );
        if let Some(class_key) = constraint.class_key.clone() {
            lowered.with_class_key(class_key)
        } else {
            lowered
        }
    }

    fn import_surface_effect_row(
        &mut self,
        surface: &ModuleSurface,
        row: &SurfaceEffectRow,
    ) -> EffectRow {
        let ctx = self;
        let mut out = EffectRow::empty();
        for item in &row.items {
            out.add(EffectKey {
                name: item.name.clone(),
                arg: item.arg.map(|arg| import_surface_ty(ctx, surface, arg)),
            });
        }
        out.open = row
            .open
            .as_deref()
            .map(|name| ctx.fresh_open_row_name(name));
        out
    }
}

impl CheckPass<'_, '_, '_> {
    pub(super) fn unify_ty_for_type_params(
        &mut self,
        type_params: &[Symbol],
        pattern: HirTyId,
        actual: HirTyId,
        subst: &mut HashMap<Symbol, HirTyId>,
    ) -> bool {
        self.unify_ty(type_params, pattern, actual, subst)
    }

    fn solve_obligation(
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

    fn resolve_obligation_evidence(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
        stack: &mut Vec<String>,
    ) -> Option<ConstraintEvidence> {
        match obligation.kind {
            ConstraintKind::Subtype | ConstraintKind::TypeEq => self
                .solve_obligation(origin, obligation, stack)
                .then_some(ConstraintEvidence::Param {
                    key: obligation.key(),
                }),
            ConstraintKind::Implements => {
                self.resolve_implements_evidence(origin, obligation, stack)
            }
        }
    }

    fn solve_implements(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
        stack: &mut Vec<String>,
    ) -> bool {
        let ctx = self;
        let Some((class_key, class_args)) = ctx.obligation_class_target(obligation) else {
            ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return false;
        };
        let frame = format!(
            "{}:{}",
            class_key.name,
            class_args
                .iter()
                .copied()
                .map(|arg| ctx.render_ty(arg))
                .collect::<Vec<_>>()
                .join(", ")
        );
        if stack.contains(&frame) {
            return true;
        }
        stack.push(frame);

        let mut matches = 0usize;
        let local_instances = ctx.instance_facts().values().cloned().collect::<Vec<_>>();
        for instance in &local_instances {
            if instance.class_key != class_key {
                continue;
            }
            if ctx.instance_matches(origin, instance, &class_args, stack) {
                matches = matches.saturating_add(1);
            }
        }

        let Some(env) = ctx.sema_env() else {
            let _ = stack.pop();
            if matches == 0 {
                ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
                return false;
            }
            if matches > 1 {
                ctx.diag(origin.span, DiagKind::AmbiguousInstanceMatch, "");
                return false;
            }
            return true;
        };

        let mut visited = BTreeSet::new();
        let mut pending = ctx.static_imports();
        while let Some(module_key) = pending.pop() {
            if !visited.insert(module_key.clone()) {
                continue;
            }
            let Some(surface) = env.module_surface(&module_key) else {
                continue;
            };
            pending.extend(surface.static_imports().iter().cloned());
            for instance in surface.exported_instances() {
                if instance.class_key != class_key {
                    continue;
                }
                let imported = ctx.instance_facts_from_surface(&surface, instance);
                if ctx.instance_matches(origin, &imported, &class_args, stack) {
                    matches = matches.saturating_add(1);
                }
            }
        }

        let _ = stack.pop();
        if matches == 0 {
            ctx.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return false;
        }
        if matches > 1 {
            ctx.diag(origin.span, DiagKind::AmbiguousInstanceMatch, "");
            return false;
        }
        true
    }

    fn resolve_implements_evidence(
        &mut self,
        origin: HirOrigin,
        obligation: &ConstraintObligation,
        stack: &mut Vec<String>,
    ) -> Option<ConstraintEvidence> {
        let key = obligation.key();
        if let Some(evidence) = self.resolve_in_scope_evidence(&key) {
            return Some(evidence);
        }
        if let Some(evidence) = self.resolve_equivalent_in_scope_evidence(&key) {
            return Some(evidence);
        }
        let Some((class_key, class_args)) = self.obligation_class_target(obligation) else {
            self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return None;
        };
        let frame = format!(
            "{}:{}",
            class_key.name,
            class_args
                .iter()
                .copied()
                .map(|arg| self.render_ty(arg))
                .collect::<Vec<_>>()
                .join(", ")
        );
        if stack.contains(&frame) {
            self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
            return None;
        }
        stack.push(frame);

        let mut matches = Vec::<ConstraintEvidence>::new();
        let local_instances = self.instance_facts().values().cloned().collect::<Vec<_>>();
        for instance in &local_instances {
            if instance.class_key != class_key {
                continue;
            }
            if let Some(evidence) = self.instance_provider_evidence(
                origin,
                instance,
                self.module_key().clone(),
                &class_args,
                stack,
            ) {
                matches.push(evidence);
            }
        }

        if let Some(env) = self.sema_env() {
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
                for instance in surface.exported_instances() {
                    if instance.class_key != class_key {
                        continue;
                    }
                    let imported = self.instance_facts_from_surface(&surface, instance);
                    if let Some(evidence) = self.instance_provider_evidence(
                        origin,
                        &imported,
                        module_key.clone(),
                        &class_args,
                        stack,
                    ) {
                        matches.push(evidence);
                    }
                }
            }
        }

        let _ = stack.pop();
        match matches.len() {
            0 => {
                self.diag(origin.span, DiagKind::UnsatisfiedConstraint, "");
                None
            }
            1 => matches.pop(),
            _ => {
                self.diag(origin.span, DiagKind::AmbiguousInstanceMatch, "");
                None
            }
        }
    }

    fn resolve_equivalent_in_scope_evidence(
        &self,
        key: &ConstraintKey,
    ) -> Option<ConstraintEvidence> {
        self.evidence_entries_in_scope()
            .into_iter()
            .find_map(|(candidate, evidence)| {
                (candidate.kind == key.kind
                    && candidate.class_key == key.class_key
                    && self.ty_matches(candidate.subject, key.subject)
                    && self.ty_matches(candidate.value, key.value))
                .then_some(evidence)
            })
    }

    fn obligation_class_target(
        &self,
        obligation: &ConstraintObligation,
    ) -> Option<(DefinitionKey, Box<[HirTyId]>)> {
        let ctx = self;
        let HirTyKind::Named { name, args } = ctx.ty(obligation.value).kind else {
            return None;
        };
        let class_key = obligation.class_key.clone().unwrap_or_else(|| {
            ctx.class_facts_by_name(name).map_or_else(
                || surface_key(ctx.module_key(), ctx.interner(), name),
                |facts| facts.key.clone(),
            )
        });
        let class_args = if ctx.ty_ids(args).is_empty() {
            vec![obligation.subject]
        } else {
            ctx.ty_ids(args)
        };
        Some((class_key, class_args.into_boxed_slice()))
    }

    fn instance_matches(
        &mut self,
        origin: HirOrigin,
        instance: &InstanceFacts,
        class_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> bool {
        let ctx = self;
        let Some(subst) =
            ctx.unify_instance_args(&instance.type_params, &instance.class_args, class_args)
        else {
            return false;
        };
        let obligations = instance
            .constraints
            .iter()
            .map(|constraint| ctx.instantiate_obligation(constraint, &subst))
            .collect::<Vec<_>>();
        obligations
            .iter()
            .all(|obligation| ctx.solve_obligation(origin, obligation, stack))
    }

    fn instance_provider_evidence(
        &mut self,
        origin: HirOrigin,
        instance: &InstanceFacts,
        module: ModuleKey,
        class_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> Option<ConstraintEvidence> {
        let subst =
            self.unify_instance_args(&instance.type_params, &instance.class_args, class_args)?;
        let obligations = instance
            .constraints
            .iter()
            .map(|constraint| self.instantiate_obligation(constraint, &subst))
            .collect::<Vec<_>>();
        let args = obligations
            .iter()
            .filter(|obligation| matches!(obligation.kind, ConstraintKind::Implements))
            .map(|obligation| self.resolve_obligation_evidence(origin, obligation, stack))
            .collect::<Option<Vec<_>>>()?
            .into_boxed_slice();
        Some(ConstraintEvidence::Provider {
            module,
            name: self.instance_provider_name(instance),
            args,
        })
    }

    fn unify_instance_args(
        &mut self,
        type_params: &[Symbol],
        pattern_args: &[HirTyId],
        actual_args: &[HirTyId],
    ) -> Option<TypeSubstMap> {
        let ctx = self;
        if pattern_args.len() != actual_args.len() {
            return None;
        }
        let mut subst = HashMap::new();
        for (pattern, actual) in pattern_args
            .iter()
            .copied()
            .zip(actual_args.iter().copied())
        {
            if !ctx.unify_ty(type_params, pattern, actual, &mut subst) {
                return None;
            }
        }
        Some(subst)
    }

    fn unify_ty(
        &mut self,
        type_params: &[Symbol],
        pattern: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let ctx = self;
        match ctx.ty(pattern).kind {
            HirTyKind::Named { name, args } if type_params.contains(&name) => {
                ctx.unify_type_constructor_param(type_params, name, args, actual, subst)
            }
            HirTyKind::Named {
                name: left_name,
                args: left_args,
            } => ctx.unify_named_ty(type_params, actual, subst, left_name, left_args),
            HirTyKind::Arrow {
                params: left_params,
                ret: left_ret,
                is_effectful: left_effectful,
            } => ctx.unify_arrow_ty(
                type_params,
                actual,
                subst,
                left_params,
                left_ret,
                left_effectful,
            ),
            HirTyKind::Sum { left, right } => {
                let HirTyKind::Sum {
                    left: actual_left,
                    right: actual_right,
                } = ctx.ty(actual).kind
                else {
                    return false;
                };
                ctx.unify_ty(type_params, left, actual_left, subst)
                    && ctx.unify_ty(type_params, right, actual_right, subst)
            }
            HirTyKind::Tuple { items } => {
                let HirTyKind::Tuple {
                    items: actual_items,
                } = ctx.ty(actual).kind
                else {
                    return false;
                };
                ctx.unify_ty_lists(type_params, items, actual_items, subst)
            }
            HirTyKind::Array { dims, item } => {
                let HirTyKind::Array {
                    dims: actual_dims,
                    item: actual_item,
                } = ctx.ty(actual).kind
                else {
                    return false;
                };
                ctx.dims(dims) == ctx.dims(actual_dims)
                    && ctx.unify_ty(type_params, item, actual_item, subst)
            }
            HirTyKind::Mut { inner } => {
                let HirTyKind::Mut {
                    inner: actual_inner,
                } = ctx.ty(actual).kind
                else {
                    return false;
                };
                ctx.unify_ty(type_params, inner, actual_inner, subst)
            }
            HirTyKind::AnyClass { class } => {
                let HirTyKind::AnyClass {
                    class: actual_class,
                } = ctx.ty(actual).kind
                else {
                    return false;
                };
                ctx.unify_ty(type_params, class, actual_class, subst)
            }
            HirTyKind::SomeClass { class } => {
                let HirTyKind::SomeClass {
                    class: actual_class,
                } = ctx.ty(actual).kind
                else {
                    return false;
                };
                ctx.unify_ty(type_params, class, actual_class, subst)
            }
            HirTyKind::Record { fields } => ctx.unify_record_ty(type_params, actual, subst, fields),
            _ => ctx.ty_matches(pattern, actual),
        }
    }

    fn unify_type_param(&self, name: Symbol, actual: HirTyId, subst: &mut TypeSubstMap) -> bool {
        let ctx = self;
        if let Some(bound) = subst.get(&name).copied() {
            return ctx.ty_matches(bound, actual);
        }
        let _prev = subst.insert(name, actual);
        true
    }

    fn unify_type_constructor_param(
        &mut self,
        type_params: &[Symbol],
        name: Symbol,
        args: SliceRange<HirTyId>,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let pattern_args = self.ty_ids(args);
        if pattern_args.is_empty() {
            return self.unify_type_param(name, actual, subst);
        }
        let HirTyKind::Named {
            name: actual_name,
            args: actual_args,
        } = self.ty(actual).kind
        else {
            return false;
        };
        let actual_args = self.ty_ids(actual_args);
        if actual_args.len() < pattern_args.len() {
            return false;
        }
        let prefix_len = actual_args.len() - pattern_args.len();
        let constructor_args = self.alloc_ty_list(actual_args[..prefix_len].iter().copied());
        let constructor = self.alloc_ty(HirTyKind::Named {
            name: actual_name,
            args: constructor_args,
        });
        if !self.unify_type_param(name, constructor, subst) {
            return false;
        }
        pattern_args
            .into_iter()
            .zip(actual_args[prefix_len..].iter().copied())
            .all(|(pattern, actual)| self.unify_ty(type_params, pattern, actual, subst))
    }

    fn unify_named_ty(
        &mut self,
        type_params: &[Symbol],
        actual: HirTyId,
        subst: &mut TypeSubstMap,
        left_name: Symbol,
        left_args: SliceRange<HirTyId>,
    ) -> bool {
        let ctx = self;
        let HirTyKind::Named {
            name: right_name,
            args: right_args,
        } = ctx.ty(actual).kind
        else {
            return false;
        };
        left_name == right_name && ctx.unify_ty_lists(type_params, left_args, right_args, subst)
    }

    fn unify_arrow_ty(
        &mut self,
        type_params: &[Symbol],
        actual: HirTyId,
        subst: &mut TypeSubstMap,
        left_params: SliceRange<HirTyId>,
        left_ret: HirTyId,
        left_effectful: bool,
    ) -> bool {
        let ctx = self;
        let HirTyKind::Arrow {
            params: right_params,
            ret: right_ret,
            is_effectful: right_effectful,
        } = ctx.ty(actual).kind
        else {
            return false;
        };
        left_effectful == right_effectful
            && ctx.unify_ty_lists(type_params, left_params, right_params, subst)
            && ctx.unify_ty(type_params, left_ret, right_ret, subst)
    }

    fn unify_record_ty(
        &mut self,
        type_params: &[Symbol],
        actual: HirTyId,
        subst: &mut TypeSubstMap,
        fields: SliceRange<HirTyField>,
    ) -> bool {
        let ctx = self;
        let HirTyKind::Record {
            fields: actual_fields,
        } = ctx.ty(actual).kind
        else {
            return false;
        };
        let pattern_fields = ctx.ty_fields(fields);
        let actual_fields = ctx.ty_fields(actual_fields);
        if pattern_fields.len() != actual_fields.len() {
            return false;
        }
        let actual_fields = actual_fields
            .into_iter()
            .map(|field| (field.name, field.ty))
            .collect::<HashMap<_, _>>();
        pattern_fields.into_iter().all(|field| {
            actual_fields
                .get(&field.name)
                .is_some_and(|ty| ctx.unify_ty(type_params, field.ty, *ty, subst))
        })
    }

    fn unify_ty_lists(
        &mut self,
        type_params: &[Symbol],
        pattern: SliceRange<HirTyId>,
        actual: SliceRange<HirTyId>,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let ctx = self;
        let pattern = ctx.ty_ids(pattern);
        let actual = ctx.ty_ids(actual);
        pattern.len() == actual.len()
            && pattern
                .into_iter()
                .zip(actual)
                .all(|(pattern, actual)| ctx.unify_ty(type_params, pattern, actual, subst))
    }
}

impl PassBase<'_, '_, '_> {
    fn class_name_from_surface(
        &mut self,
        surface: &ModuleSurface,
        class_args: &[SurfaceTyId],
        class_key: &DefinitionKey,
    ) -> Symbol {
        let ctx = self;
        for class in surface.exported_classes() {
            if &class.key == class_key {
                return ctx.intern(&class.key.name);
            }
        }
        let _ = class_args;
        ctx.intern(&class_key.name)
    }

    fn instance_provider_name(&self, instance: &InstanceFacts) -> Box<str> {
        let args = instance
            .class_args
            .iter()
            .copied()
            .map(|arg| self.render_ty(arg))
            .collect::<Vec<_>>()
            .join(",");
        format!(
            "__dict__::{}::{}[{args}]",
            instance.class_key.module.as_str(),
            instance.class_key.name
        )
        .into_boxed_str()
    }
}
