use std::collections::{BTreeSet, HashMap};
use std::iter::repeat;

use music_arena::SliceRange;
use music_hir::{HirDim, HirOrigin, HirTyField, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::Symbol;

use crate::api::{
    ConstraintAnswer, ConstraintFacts, ConstraintKey, ConstraintKind, ConstraintSurface,
    DefinitionKey, ExportedValue, GivenFacts, GivenSurface, ModuleSurface, SurfaceEffectRow,
    SurfaceTyId,
};
use crate::effects::{EffectKey, EffectRow};

use super::surface::{import_surface_ty, surface_key};
use super::{CheckPass, DiagKind, PassBase};

type TypeSubstMap = HashMap<Symbol, HirTyId>;

struct SurfaceGivenEvidenceScan<'a> {
    origin: HirOrigin,
    shape_key: &'a DefinitionKey,
    shape_args: &'a [HirTyId],
    stack: &'a mut Vec<String>,
    matches: &'a mut Vec<ConstraintAnswer>,
    module_key: &'a ModuleKey,
    surface: &'a ModuleSurface,
}

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
    pub shape_key: Option<DefinitionKey>,
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
        ConstraintKey::new(self.kind, self.subject, self.value, self.shape_key.clone())
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

    pub fn given_facts_from_surface(
        &mut self,
        surface: &ModuleSurface,
        given: &GivenSurface,
    ) -> GivenFacts {
        let ctx = self;
        let shape_name =
            ctx.shape_name_from_surface(surface, given.shape_args.as_ref(), &given.shape_key);
        GivenFacts::new(
            HirOrigin::dummy(),
            given.shape_key.clone(),
            shape_name,
            given
                .shape_args
                .iter()
                .copied()
                .map(|ty| import_surface_ty(ctx, surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
            Vec::<Symbol>::new().into_boxed_slice(),
        )
        .with_type_params(
            given
                .type_params
                .iter()
                .map(|name| ctx.intern(name))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
        .with_type_param_kinds(
            given
                .type_param_kinds
                .iter()
                .copied()
                .map(|ty| import_surface_ty(ctx, surface, ty))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        )
        .with_constraints(
            given
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

    pub fn resolve_obligations_to_answers(
        &mut self,
        origin: HirOrigin,
        obligations: &[ConstraintObligation],
    ) -> Option<Box<[ConstraintAnswer]>> {
        let mut stack = Vec::<String>::new();
        let mut answers = Vec::new();
        for obligation in obligations {
            if !matches!(obligation.kind, ConstraintKind::Implements) {
                let _ = self.solve_obligation(origin, obligation, &mut stack);
                continue;
            }
            answers.push(self.resolve_obligation_answer(origin, obligation, &mut stack)?);
        }
        Some(answers.into_boxed_slice())
    }

    pub fn answer_scope_for_constraints(
        &mut self,
        constraints: &[ConstraintFacts],
    ) -> HashMap<ConstraintKey, ConstraintAnswer> {
        constraints
            .iter()
            .filter_map(|constraint| {
                self.constraint_key_for_facts(constraint).map(|key| {
                    let answer = ConstraintAnswer::Param { key: key.clone() };
                    (key, answer)
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
            shape_key: constraint.shape_key.clone(),
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
                constraint.shape_key.clone(),
            )
        })
    }
}

impl PassBase<'_, '_, '_> {
    pub fn substitute_ty(&mut self, ty: HirTyId, subst: &TypeSubstMap) -> HirTyId {
        self.substitute_ty_kind(ty, self.ty(ty).kind, subst)
    }

    fn substitute_ty_kind(
        &mut self,
        ty: HirTyId,
        kind: HirTyKind,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        match kind {
            HirTyKind::Named { name, args } => self.substitute_named_ty(name, args, subst),
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                is_effectful,
            } => self.substitute_pi_ty(binder, binder_ty, body, is_effectful, subst),
            HirTyKind::Arrow {
                params,
                ret,
                is_effectful,
            } => self.substitute_arrow_ty(params, ret, is_effectful, subst),
            HirTyKind::Sum { left, right } => self.substitute_sum_ty(left, right, subst),
            HirTyKind::Tuple { items } => self.substitute_tuple_ty(items, subst),
            HirTyKind::Seq { item } => {
                self.substitute_item_ty(item, subst, |item| HirTyKind::Seq { item })
            }
            HirTyKind::Array { dims, item } => self.substitute_array_ty(dims, item, subst),
            HirTyKind::Range { bound } => {
                self.substitute_item_ty(bound, subst, |bound| HirTyKind::Range { bound })
            }
            HirTyKind::Handler {
                effect,
                input,
                output,
            } => self.substitute_handler_ty(effect, input, output, subst),
            HirTyKind::Mut { inner } => self.substitute_mut_ty(inner, subst),
            HirTyKind::AnyShape { capability } => self.substitute_shape_ty(capability, subst, true),
            HirTyKind::SomeShape { capability } => {
                self.substitute_shape_ty(capability, subst, false)
            }
            HirTyKind::Record { fields } => self.substitute_record_ty(fields, subst),
            _ => ty,
        }
    }

    fn substitute_pi_ty(
        &mut self,
        binder: Symbol,
        binder_ty: HirTyId,
        body: HirTyId,
        is_effectful: bool,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let binder_ty = self.substitute_ty(binder_ty, subst);
        let mut next = subst.clone();
        let _ = next.remove(&binder);
        let body = self.substitute_ty(body, &next);
        self.alloc_ty(HirTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        })
    }

    fn substitute_arrow_ty(
        &mut self,
        params: SliceRange<HirTyId>,
        ret: HirTyId,
        is_effectful: bool,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let params = self.substitute_ty_list(params, subst);
        let ret = self.substitute_ty(ret, subst);
        self.alloc_ty(HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        })
    }

    fn substitute_sum_ty(
        &mut self,
        left: HirTyId,
        right: HirTyId,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let left = self.substitute_ty(left, subst);
        let right = self.substitute_ty(right, subst);
        self.alloc_ty(HirTyKind::Sum { left, right })
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
        if name == self.known().bits {
            let arg_ids = self.ty_ids(args);
            if let [width_ty] = arg_ids.as_slice() {
                if let HirTyKind::NatLit(width) = self.ty(*width_ty).kind
                    && width > 0
                    && let Ok(width) = u32::try_from(width)
                {
                    return self.alloc_ty(HirTyKind::Bits { width });
                }
            }
        }
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
        let item_ty = self.substitute_ty(item, subst);
        self.alloc_ty(ctor(item_ty))
    }

    fn substitute_array_ty(
        &mut self,
        dims: SliceRange<HirDim>,
        item: HirTyId,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let item_ty = self.substitute_ty(item, subst);
        self.alloc_ty(HirTyKind::Array {
            dims,
            item: item_ty,
        })
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

    fn substitute_shape_ty(
        &mut self,
        shape: HirTyId,
        subst: &TypeSubstMap,
        is_any: bool,
    ) -> HirTyId {
        let shape = self.substitute_ty(shape, subst);
        if is_any {
            self.alloc_ty(HirTyKind::AnyShape { capability: shape })
        } else {
            self.alloc_ty(HirTyKind::SomeShape { capability: shape })
        }
    }

    fn substitute_record_ty(
        &mut self,
        fields: SliceRange<HirTyField>,
        subst: &TypeSubstMap,
    ) -> HirTyId {
        let fields = self
            .ty_fields(fields)
            .into_iter()
            .map(|field| HirTyField::new(field.name, self.substitute_ty(field.ty, subst)))
            .collect::<Vec<_>>();
        let fields = self.alloc_ty_fields(fields);
        self.alloc_ty(HirTyKind::Record { fields })
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
        if let Some(shape_key) = constraint.shape_key.clone() {
            lowered.with_shape_key(shape_key)
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

    fn resolve_obligation_answer(
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

    fn solve_implements(
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

    fn count_matching_givens(
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

    fn count_local_given_matches(
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

    fn count_imported_given_matches(
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

    fn count_surface_given_matches(
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

    fn finish_instance_match_count(&mut self, origin: HirOrigin, matches: usize) -> bool {
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

    fn resolve_implements_answer(
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

impl CheckPass<'_, '_, '_> {
    fn resolve_available_answer(&self, key: &ConstraintKey) -> Option<ConstraintAnswer> {
        self.resolve_in_scope_answer(key)
            .or_else(|| self.resolve_equivalent_in_scope_answer(key))
    }

    fn constraint_stack_frame(&self, shape_key: &DefinitionKey, shape_args: &[HirTyId]) -> String {
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

    fn collect_local_given_answers(
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

    fn collect_imported_given_answers(
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

    fn finish_given_answers(
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

    fn resolve_equivalent_in_scope_answer(&self, key: &ConstraintKey) -> Option<ConstraintAnswer> {
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

    fn obligation_shape_target(
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

impl CheckPass<'_, '_, '_> {
    fn given_matches(
        &mut self,
        origin: HirOrigin,
        given: &GivenFacts,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> bool {
        let ctx = self;
        let Some(subst) = ctx.unify_given_args(&given.type_params, &given.shape_args, shape_args)
        else {
            return false;
        };
        let obligations = given
            .constraints
            .iter()
            .map(|constraint| ctx.instantiate_obligation(constraint, &subst))
            .collect::<Vec<_>>();
        obligations
            .iter()
            .all(|obligation| ctx.solve_obligation(origin, obligation, stack))
    }

    fn given_provider_answer(
        &mut self,
        origin: HirOrigin,
        given: &GivenFacts,
        module: ModuleKey,
        shape_args: &[HirTyId],
        stack: &mut Vec<String>,
    ) -> Option<ConstraintAnswer> {
        let subst = self.unify_given_args(&given.type_params, &given.shape_args, shape_args)?;
        let obligations = given
            .constraints
            .iter()
            .map(|constraint| self.instantiate_obligation(constraint, &subst))
            .collect::<Vec<_>>();
        let args = obligations
            .iter()
            .filter(|obligation| matches!(obligation.kind, ConstraintKind::Implements))
            .map(|obligation| self.resolve_obligation_answer(origin, obligation, stack))
            .collect::<Option<Vec<_>>>()?
            .into_boxed_slice();
        Some(ConstraintAnswer::Provider {
            module,
            name: self.given_provider_name(given),
            args,
        })
    }

    fn unify_given_args(
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
}

impl CheckPass<'_, '_, '_> {
    fn unify_ty(
        &mut self,
        type_params: &[Symbol],
        pattern: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        match self.ty(pattern).kind {
            HirTyKind::Named { name, args } if type_params.contains(&name) => {
                self.unify_type_constructor_param(type_params, name, args, actual, subst)
            }
            HirTyKind::Named {
                name: left_name,
                args: left_args,
            } => self.unify_named_ty(type_params, actual, subst, left_name, left_args),
            HirTyKind::Arrow {
                params: left_params,
                ret: left_ret,
                is_effectful: left_effectful,
            } => self.unify_arrow_ty(
                type_params,
                actual,
                subst,
                left_params,
                left_ret,
                left_effectful,
            ),
            HirTyKind::Sum { left, right } => {
                self.unify_sum_ty(type_params, left, right, actual, subst)
            }
            HirTyKind::Tuple { items } => self.unify_tuple_ty(type_params, items, actual, subst),
            HirTyKind::Array { dims, item } => {
                self.unify_array_ty(type_params, dims, item, actual, subst)
            }
            HirTyKind::Mut { inner } => self.unify_mut_ty(type_params, inner, actual, subst),
            HirTyKind::AnyShape { capability } => {
                self.unify_any_shape_ty(type_params, capability, actual, subst)
            }
            HirTyKind::SomeShape { capability } => {
                self.unify_some_shape_ty(type_params, capability, actual, subst)
            }
            HirTyKind::Record { fields } => {
                self.unify_record_ty(type_params, actual, subst, fields)
            }
            _ => self.ty_matches(pattern, actual),
        }
    }

    fn unify_sum_ty(
        &mut self,
        type_params: &[Symbol],
        left: HirTyId,
        right: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::Sum {
            left: actual_left,
            right: actual_right,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty(type_params, left, actual_left, subst)
            && self.unify_ty(type_params, right, actual_right, subst)
    }

    fn unify_tuple_ty(
        &mut self,
        type_params: &[Symbol],
        items: SliceRange<HirTyId>,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::Tuple {
            items: actual_items,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty_lists(type_params, items, actual_items, subst)
    }

    fn unify_array_ty(
        &mut self,
        type_params: &[Symbol],
        dims: SliceRange<HirDim>,
        item: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::Array {
            dims: actual_dims,
            item: actual_item,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.dims(dims) == self.dims(actual_dims)
            && self.unify_ty(type_params, item, actual_item, subst)
    }

    fn unify_mut_ty(
        &mut self,
        type_params: &[Symbol],
        inner: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::Mut {
            inner: actual_inner,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty(type_params, inner, actual_inner, subst)
    }

    fn unify_any_shape_ty(
        &mut self,
        type_params: &[Symbol],
        shape: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::AnyShape {
            capability: actual_capability,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty(type_params, shape, actual_capability, subst)
    }

    fn unify_some_shape_ty(
        &mut self,
        type_params: &[Symbol],
        shape: HirTyId,
        actual: HirTyId,
        subst: &mut TypeSubstMap,
    ) -> bool {
        let HirTyKind::SomeShape {
            capability: actual_capability,
        } = self.ty(actual).kind
        else {
            return false;
        };
        self.unify_ty(type_params, shape, actual_capability, subst)
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
    fn shape_name_from_surface(
        &mut self,
        surface: &ModuleSurface,
        shape_args: &[SurfaceTyId],
        shape_key: &DefinitionKey,
    ) -> Symbol {
        let ctx = self;
        for shape in surface.exported_shapes() {
            if &shape.key == shape_key {
                return ctx.intern(&shape.key.name);
            }
        }
        let _ = shape_args;
        ctx.intern(&shape_key.name)
    }
}

impl CheckPass<'_, '_, '_> {
    fn given_provider_name(&self, given: &GivenFacts) -> Box<str> {
        let args = given
            .shape_args
            .iter()
            .copied()
            .map(|arg| self.render_ty(arg))
            .collect::<Vec<_>>()
            .join(",");
        format!(
            "__dict__::{}::{}[{args}]",
            given.shape_key.module.as_str(),
            given.shape_key.name
        )
        .into_boxed_str()
    }
}
