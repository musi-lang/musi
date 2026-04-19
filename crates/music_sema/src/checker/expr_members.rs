use std::collections::{BTreeSet, HashMap};

use music_hir::{HirAccessKind, HirExprId, HirExprKind, HirOrigin, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::{Ident, NameBindingId, Symbol};

use crate::BindingScheme;
use crate::api::{ExprFacts, ExprMemberFact, ExprMemberKind, ModuleSurface};

use super::decls::module_export_for_expr;
use super::exprs::peel_mut_ty;
use super::{CheckPass, DiagKind};

struct ImportedAttachedMethod {
    module: ModuleKey,
    scheme: BindingScheme,
}

impl CheckPass<'_, '_, '_> {
    pub(super) fn check_field_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        base: HirExprId,
        access: HirAccessKind,
        name: Ident,
    ) -> ExprFacts {
        let base_facts = super::exprs::check_expr(self, base);
        let effects = base_facts.effects.clone();

        if let HirExprKind::Name { name: effect_name } = self.expr(base).kind {
            let effect_name = self.resolve_symbol(effect_name.name);
            let op_name = self.resolve_symbol(name.name);
            if let Some(effect) = self.effect_def(effect_name)
                && let Some(op) = effect.op(op_name).cloned()
            {
                let params = self.alloc_ty_list(op.params().iter().copied());
                let ty = self.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret: op.result(),
                    is_effectful: true,
                });
                self.set_expr_member_fact(
                    expr_id,
                    ExprMemberFact::new(ExprMemberKind::EffectOperation, name.name, ty),
                );
                return ExprFacts::new(ty, effects);
            }
        }

        if let Some(ty) = self.check_class_member_field(expr_id, origin, base, name) {
            return ExprFacts::new(ty, effects);
        }

        let base_is_mut = self.is_mut_ty(base_facts.ty);
        let base_ty = peel_mut_ty(self, base_facts.ty);
        let ty = if let Some(field_ty) =
            self.record_like_field_ty(base_ty, self.resolve_symbol(name.name))
        {
            self.set_expr_member_fact(
                expr_id,
                ExprMemberFact::new(ExprMemberKind::RecordField, name.name, field_ty),
            );
            field_ty
        } else {
            self.check_non_record_field_expr(
                expr_id,
                origin,
                base,
                (base_ty, base_is_mut),
                access,
                name,
            )
        };
        ExprFacts::new(ty, effects)
    }

    fn check_class_member_field(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        base: HirExprId,
        name: Ident,
    ) -> Option<HirTyId> {
        let HirExprKind::Name { name: class_name } = self.expr(base).kind else {
            return None;
        };
        let class = self.class_facts_by_name(class_name.name)?.clone();
        let (member_index, member) = class
            .members
            .iter()
            .enumerate()
            .find(|(_, member)| member.name == name.name)
            .map(|(index, member)| (u16::try_from(index).unwrap_or(u16::MAX), member.clone()))?;
        let params = member
            .params
            .iter()
            .copied()
            .map(|ty| self.erase_class_type_params(ty, &class.type_params))
            .collect::<Vec<_>>();
        let params = self.alloc_ty_list(params);
        let ty = self.alloc_ty(HirTyKind::Arrow {
            params,
            ret: member.result,
            is_effectful: false,
        });
        self.set_expr_member_fact(
            expr_id,
            ExprMemberFact::new(ExprMemberKind::ClassMember, name.name, ty)
                .with_index(member_index),
        );
        let _ = origin;
        Some(ty)
    }

    fn erase_class_type_params(&self, ty: HirTyId, type_params: &[Symbol]) -> HirTyId {
        match self.ty(ty).kind {
            HirTyKind::Named { name, args }
                if type_params.contains(&name) && self.ty_ids(args).is_empty() =>
            {
                self.builtins().unknown
            }
            _ => ty,
        }
    }

    fn check_non_record_field_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        base_expr: HirExprId,
        base: (HirTyId, bool),
        access: HirAccessKind,
        name: Ident,
    ) -> HirTyId {
        let builtins = self.builtins();
        let (base_ty, base_is_mut) = base;
        if self.ty(base_ty).kind == HirTyKind::Module {
            if let Some((surface, export)) = module_export_for_expr(self, base_expr, name) {
                let module_target = export.module_target.clone();
                if let Some(target) = module_target.clone() {
                    self.set_expr_module_target(expr_id, target);
                }
                let scheme = self.scheme_from_export(&surface, &export);
                if scheme.type_params.is_empty() {
                    let instantiated = self.instantiate_monomorphic_scheme(&scheme);
                    if let Some(evidence) =
                        self.resolve_obligations_to_evidence(origin, &instantiated.obligations)
                        && !evidence.is_empty()
                    {
                        self.set_expr_evidence(expr_id, evidence);
                    }
                }
                self.set_expr_callable_effects(expr_id, scheme.effects.clone());
                let value_ty = self.scheme_value_ty(&scheme);
                let mut fact =
                    ExprMemberFact::new(ExprMemberKind::ModuleExport, name.name, value_ty);
                if let Some(target) = module_target {
                    fact = fact.with_module_target(target);
                }
                self.set_expr_member_fact(expr_id, fact);
                return value_ty;
            }
            return builtins.any;
        }

        let method_name = name.name;
        if self.ty(base_ty).kind == HirTyKind::Type
            && let Some(method_ty) =
                self.check_attached_method_namespace(expr_id, origin, base_expr, method_name)
        {
            return method_ty;
        }
        if let Some(field_ty) = self.check_std_ffi_pointer_field(expr_id, base_expr, name) {
            return field_ty;
        }
        if let Some(method_ty) =
            self.check_dot_callable_field(expr_id, origin, base_ty, base_is_mut, method_name)
        {
            return method_ty;
        }
        if matches!(self.ty(base_ty).kind, HirTyKind::Record { .. })
            || self.range_item_type(base_ty).is_some()
        {
            let field_name = self.resolve_symbol(name.name).to_owned();
            self.diag_message(
                origin.span,
                DiagKind::UnknownField,
                format!("unknown field `{field_name}`"),
                format!("unknown field `{field_name}`"),
            );
            return builtins.unknown;
        }
        if matches!(access, HirAccessKind::Direct) {
            self.diag(
                origin.span,
                DiagKind::InvalidFieldTarget,
                "field access target must be record, module, or dot-callable target",
            );
        } else {
            self.diag(
                origin.span,
                DiagKind::InvalidFieldTarget,
                "optional field access target must be optional record-like value",
            );
        }
        builtins.unknown
    }

    fn check_attached_method_namespace(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        base_expr: HirExprId,
        method_name: Symbol,
    ) -> Option<HirTyId> {
        let base_origin = self.expr(base_expr).origin;
        let receiver_ty = self.lower_type_expr(base_expr, base_origin);
        let candidates = self
            .visible_attached_methods_named(method_name)
            .into_iter()
            .filter(|binding| self.dot_callable_matches_binding(*binding, receiver_ty))
            .collect::<Vec<_>>();
        if candidates.len() > 1 {
            self.diag(origin.span, DiagKind::AmbiguousDotCallable, "");
            return Some(self.builtins().unknown);
        }
        if let Some(binding) = candidates.first().copied() {
            let scheme = self.binding_scheme(binding).cloned()?;
            self.set_expr_callable_effects(expr_id, scheme.effects.clone());
            let mut fact = ExprMemberFact::new(
                ExprMemberKind::AttachedMethodNamespace,
                method_name,
                scheme.ty,
            )
            .with_binding(binding);
            if let Some(target) = self.binding_module_target(binding).cloned() {
                self.set_expr_module_target(expr_id, target.clone());
                fact = fact.with_module_target(target);
            }
            self.set_expr_member_fact(expr_id, fact);
            return Some(scheme.ty);
        }

        let imported = self.imported_attached_method_candidates(method_name, receiver_ty);
        if imported.len() > 1 {
            self.diag(origin.span, DiagKind::AmbiguousDotCallable, "");
            return Some(self.builtins().unknown);
        }
        let imported = imported.into_iter().next()?;
        let ImportedAttachedMethod { module, scheme } = imported;
        self.set_expr_callable_effects(expr_id, scheme.effects.clone());
        self.set_expr_module_target(expr_id, module.clone());
        let fact = ExprMemberFact::new(
            ExprMemberKind::AttachedMethodNamespace,
            method_name,
            scheme.ty,
        )
        .with_module_target(module);
        self.set_expr_member_fact(expr_id, fact);
        Some(scheme.ty)
    }

    fn check_std_ffi_pointer_field(
        &mut self,
        expr_id: HirExprId,
        base_expr: HirExprId,
        name: Ident,
    ) -> Option<HirTyId> {
        let HirExprKind::Field {
            base: module_expr,
            name: pointer_name,
            ..
        } = self.expr(base_expr).kind
        else {
            return None;
        };
        if self.resolve_symbol(pointer_name.name) != "ptr" {
            return None;
        }
        let (surface, _) = module_export_for_expr(self, module_expr, pointer_name)?;
        let module_key = surface.module_key().as_str();
        if module_key != "@std/ffi" && !module_key.ends_with("ffi/index.ms") {
            return None;
        }
        let export = surface
            .exported_value(self.resolve_symbol(name.name))?
            .clone();
        let scheme = self.scheme_from_export(&surface, &export);
        self.set_expr_callable_effects(expr_id, scheme.effects.clone());
        let value_ty = self.scheme_value_ty(&scheme);
        self.set_expr_member_fact(
            expr_id,
            ExprMemberFact::new(ExprMemberKind::FfiPointerExport, name.name, value_ty),
        );
        Some(value_ty)
    }

    fn check_dot_callable_field(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        receiver_ty: HirTyId,
        receiver_is_mut: bool,
        method_name: Symbol,
    ) -> Option<HirTyId> {
        let builtins = self.builtins();
        if receiver_ty == builtins.unknown {
            return None;
        }
        let mut is_attached_method = true;
        let mut candidates = self
            .visible_attached_methods_named(method_name)
            .into_iter()
            .filter(|binding| self.dot_callable_matches_binding(*binding, receiver_ty))
            .collect::<Vec<_>>();
        if candidates.is_empty() {
            is_attached_method = false;
            candidates = self
                .visible_callable_bindings_named(method_name)
                .into_iter()
                .filter(|binding| self.dot_callable_matches_binding(*binding, receiver_ty))
                .collect::<Vec<_>>();
        }
        if candidates.is_empty() {
            return self.check_imported_attached_method_field(
                expr_id,
                origin,
                receiver_ty,
                method_name,
            );
        }
        if candidates.len() > 1 {
            self.diag(origin.span, DiagKind::AmbiguousDotCallable, "");
            return Some(builtins.unknown);
        }
        let binding = candidates[0];
        if self.dot_callable_requires_mut(binding) && !receiver_is_mut {
            self.diag(
                origin.span,
                DiagKind::DotCallableRequiresMutableReceiver,
                "",
            );
            return Some(builtins.unknown);
        }
        self.set_expr_dot_callable_binding(expr_id, binding);
        let scheme = self.binding_scheme(binding).cloned()?;
        if scheme.type_params.is_empty() {
            let instantiated = self.instantiate_monomorphic_scheme(&scheme);
            if let Some(evidence) =
                self.resolve_obligations_to_evidence(origin, &instantiated.obligations)
                && !evidence.is_empty()
            {
                self.set_expr_evidence(expr_id, evidence);
            }
        }
        self.set_expr_callable_effects(expr_id, scheme.effects.clone());
        let value_ty = self
            .strip_dot_callable_param(scheme.ty)
            .unwrap_or(scheme.ty);
        let member_kind = if is_attached_method {
            ExprMemberKind::AttachedMethod
        } else {
            ExprMemberKind::DotCallable
        };
        let mut fact =
            ExprMemberFact::new(member_kind, method_name, value_ty).with_binding(binding);
        if let Some(target) = self.binding_module_target(binding).cloned() {
            self.set_expr_module_target(expr_id, target.clone());
            fact = fact.with_module_target(target);
        }
        self.set_expr_member_fact(expr_id, fact);
        Some(value_ty)
    }

    fn check_imported_attached_method_field(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        receiver_ty: HirTyId,
        method_name: Symbol,
    ) -> Option<HirTyId> {
        let builtins = self.builtins();
        let imported = self.imported_attached_method_candidates(method_name, receiver_ty);
        if imported.is_empty() {
            return None;
        }
        if imported.len() > 1 {
            self.diag(origin.span, DiagKind::AmbiguousDotCallable, "");
            return Some(builtins.unknown);
        }
        let ImportedAttachedMethod { module, scheme } = imported.into_iter().next()?;
        if scheme.type_params.is_empty() {
            let instantiated = self.instantiate_monomorphic_scheme(&scheme);
            if let Some(evidence) =
                self.resolve_obligations_to_evidence(origin, &instantiated.obligations)
                && !evidence.is_empty()
            {
                self.set_expr_evidence(expr_id, evidence);
            }
        }
        self.set_expr_callable_effects(expr_id, scheme.effects.clone());
        let value_ty = self
            .strip_dot_callable_param(scheme.ty)
            .unwrap_or(scheme.ty);
        self.set_expr_module_target(expr_id, module.clone());
        self.set_expr_member_fact(
            expr_id,
            ExprMemberFact::new(ExprMemberKind::AttachedMethod, method_name, value_ty)
                .with_module_target(module),
        );
        Some(value_ty)
    }

    fn dot_callable_matches_binding(
        &mut self,
        binding: NameBindingId,
        receiver_ty: HirTyId,
    ) -> bool {
        let Some(scheme) = self.binding_scheme(binding).cloned() else {
            return false;
        };
        let Some(receiver_param) = self.dot_callable_receiver_param_ty(scheme.ty) else {
            return false;
        };
        let receiver_param = peel_mut_ty(self, receiver_param);
        let mut subst = HashMap::new();
        self.unify_ty_for_type_params(&scheme.type_params, receiver_param, receiver_ty, &mut subst)
    }

    fn matched_dot_callable_scheme(
        &mut self,
        scheme: &BindingScheme,
        receiver_ty: HirTyId,
    ) -> Option<BindingScheme> {
        let receiver_param = self.dot_callable_receiver_param_ty(scheme.ty)?;
        let receiver_param = peel_mut_ty(self, receiver_param);
        let mut subst = HashMap::new();
        if !self.unify_ty_for_type_params(
            &scheme.type_params,
            receiver_param,
            receiver_ty,
            &mut subst,
        ) {
            return None;
        }
        let instantiated = self.instantiate_binding_with_subst(scheme, &subst);
        Some(BindingScheme {
            type_params: Box::default(),
            type_param_kinds: Box::default(),
            param_names: scheme.param_names.clone(),
            comptime_params: scheme.comptime_params.clone(),
            constraints: Box::default(),
            ty: instantiated.ty,
            effects: instantiated.effects,
        })
    }

    fn imported_attached_method_candidates(
        &mut self,
        method_name: Symbol,
        receiver_ty: HirTyId,
    ) -> Vec<ImportedAttachedMethod> {
        let Some(env) = self.sema_env() else {
            return Vec::new();
        };
        let method_text: Box<str> = self.resolve_symbol(method_name).into();
        let mut visited = BTreeSet::new();
        let mut pending = self.static_imports();
        let mut candidates = Vec::new();
        while let Some(module) = pending.pop() {
            if !visited.insert(module.clone()) {
                continue;
            }
            let Some(surface) = env.module_surface(&module) else {
                continue;
            };
            pending.extend(surface.static_imports().iter().cloned());
            self.collect_imported_attached_method(
                method_text.as_ref(),
                receiver_ty,
                &module,
                &surface,
                &mut candidates,
            );
        }
        candidates
    }

    fn collect_imported_attached_method(
        &mut self,
        method_name: &str,
        receiver_ty: HirTyId,
        module: &ModuleKey,
        surface: &ModuleSurface,
        candidates: &mut Vec<ImportedAttachedMethod>,
    ) {
        let exports = surface
            .exported_values()
            .iter()
            .filter(|export| export.name.as_ref() == method_name && export.is_attached_method)
            .cloned()
            .collect::<Vec<_>>();
        for export in exports {
            let scheme = self.scheme_from_export(surface, &export);
            if let Some(scheme) = self.matched_dot_callable_scheme(&scheme, receiver_ty) {
                candidates.push(ImportedAttachedMethod {
                    module: module.clone(),
                    scheme,
                });
            }
        }
    }

    fn dot_callable_receiver_param_ty(&self, callable_ty: HirTyId) -> Option<HirTyId> {
        let HirTyKind::Arrow { params, .. } = self.ty(callable_ty).kind else {
            return None;
        };
        self.ty_ids(params).first().copied()
    }

    fn strip_dot_callable_param(&mut self, ty: HirTyId) -> Option<HirTyId> {
        let HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } = self.ty(ty).kind
        else {
            return None;
        };
        let params = self.ty_ids(params);
        let tail = params.get(1..).unwrap_or(&[]);
        let tail_params = self.alloc_ty_list(tail.iter().copied());
        Some(self.alloc_ty(HirTyKind::Arrow {
            params: tail_params,
            ret,
            is_effectful,
        }))
    }
}
