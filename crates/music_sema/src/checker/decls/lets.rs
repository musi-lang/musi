use music_arena::SliceRange;
use music_hir::{
    HirAttr, HirBinder, HirConstraint, HirExprId, HirExprKind, HirLetMods, HirMods, HirOrigin,
    HirParam, HirPatId, HirPatKind, HirPrefixOp, HirReceiverDecl, HirTyId, HirTyKind,
};
use music_names::{Ident, NameBindingId, Symbol};

use super::super::CheckPass;
use super::super::DiagKind;
use super::super::const_eval::try_comptime_value;
use super::super::decls::check_native_let;
use super::super::exprs::check_expr;
use super::super::pats::{bind_pat, bound_name_from_pat, pat_is_irrefutable};
use super::super::schemes::BindingScheme;
use super::imports::{
    bind_import_record_pattern, bind_structural_alias, import_record_target_for_expr,
};
use crate::api::{ConstraintFacts, ExprFacts};

type ConstraintFactsList = Box<[ConstraintFacts]>;

pub(in super::super) struct LetExprInput {
    pub(in super::super) expr_id: HirExprId,
    pub(in super::super) origin: HirOrigin,
    pub(in super::super) expr_mods: HirMods,
    pub(in super::super) mods: HirLetMods,
    pub(in super::super) pat: HirPatId,
    pub(in super::super) type_params: SliceRange<HirBinder>,
    pub(in super::super) receiver: Option<HirReceiverDecl>,
    pub(in super::super) has_param_clause: bool,
    pub(in super::super) params: SliceRange<HirParam>,
    pub(in super::super) constraints: SliceRange<HirConstraint>,
    pub(in super::super) sig: Option<HirExprId>,
    pub(in super::super) value: HirExprId,
}

struct RecCallableSeed<'a> {
    binding: Option<NameBindingId>,
    mods: HirLetMods,
    param_types: &'a [HirTyId],
    declared_ty: Option<HirTyId>,
    type_params: &'a [Symbol],
    type_param_kinds: &'a [HirTyId],
    constraints: &'a [ConstraintFacts],
}

struct CallableLetCheckInput {
    origin: HirOrigin,
    exported: bool,
    mods: HirLetMods,
    pat: HirPatId,
    params: SliceRange<HirParam>,
    declared_ty: Option<HirTyId>,
    value: HirExprId,
    binding: Option<NameBindingId>,
    receiver: Option<HirReceiverDecl>,
    type_params: Box<[Symbol]>,
    type_param_kinds: Box<[HirTyId]>,
    constraints: ConstraintFactsList,
}

struct NonCallableLetCheckInput {
    origin: HirOrigin,
    exported: bool,
    mods: HirLetMods,
    pat: HirPatId,
    value: HirExprId,
    params: SliceRange<HirParam>,
    binding: Option<NameBindingId>,
    declared_ty: Option<HirTyId>,
    is_module_stmt: bool,
    bound_name: Option<Ident>,
    type_params: Box<[Symbol]>,
    type_param_kinds: Box<[HirTyId]>,
    constraints: ConstraintFactsList,
}

struct LetBindingSchemeInput {
    binding: NameBindingId,
    ty: HirTyId,
    type_params: (Box<[Symbol]>, Box<[HirTyId]>),
    param_names: Box<[Symbol]>,
    comptime_params: Box<[bool]>,
    constraints: ConstraintFactsList,
}

struct LetFinalTyInput {
    expr_id: HirExprId,
    origin: HirOrigin,
    expr_mods: HirMods,
    mods: HirLetMods,
    pat: HirPatId,
    receiver: Option<HirReceiverDecl>,
    has_param_clause: bool,
    params: SliceRange<HirParam>,
    value: HirExprId,
    binding: Option<NameBindingId>,
    bound_name: Option<Ident>,
    type_params: Box<[Symbol]>,
    type_param_kinds: Box<[HirTyId]>,
    constraints: ConstraintFactsList,
    declared_ty: Option<HirTyId>,
    is_module_stmt: bool,
}

pub(in super::super) fn check_let_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    input: LetExprInput,
) -> ExprFacts {
    ctx.check_let_expr(input)
}

impl CheckPass<'_, '_, '_> {
    fn lower_let_type_params(
        &mut self,
        type_params: SliceRange<HirBinder>,
    ) -> (Box<[Symbol]>, Box<[HirTyId]>) {
        let kinds = self.lower_type_param_kinds(type_params);
        let names = kinds
            .iter()
            .map(|(name, _)| *name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let kind_tys = kinds
            .iter()
            .map(|(_, kind)| *kind)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        (names, kind_tys)
    }

    fn validate_non_callable_let_pattern(
        &mut self,
        origin: HirOrigin,
        pat: HirPatId,
        has_fallback: bool,
        _value: HirExprId,
        value_ty: HirTyId,
    ) {
        if !has_fallback && !pat_is_irrefutable(self, pat) {
            self.diag(
                origin.span,
                DiagKind::PlainLetRequiresIrrefutablePattern,
                "",
            );
        }
        if matches!(self.pat(pat).kind, HirPatKind::Record { .. })
            && !matches!(self.ty(value_ty).kind, HirTyKind::Record { .. })
        {
            self.diag(origin.span, DiagKind::RecordDestructuringRequiresRecord, "");
        }
    }

    fn insert_let_binding_scheme(&mut self, input: LetBindingSchemeInput) {
        let LetBindingSchemeInput {
            binding,
            ty,
            type_params,
            param_names,
            comptime_params,
            constraints,
        } = input;
        let scheme = BindingScheme {
            type_params: type_params.0,
            type_param_kinds: type_params.1,
            param_names,
            comptime_params,
            constraints,
            ty,
        };
        let value_ty = self.scheme_value_ty(&scheme);
        self.insert_binding_type(binding, value_ty);
        let evidence_keys = self
            .evidence_scope_for_constraints(&scheme.constraints)
            .into_keys()
            .collect::<Vec<_>>()
            .into_boxed_slice();
        self.insert_binding_scheme(binding, scheme);
        self.set_binding_constraint_keys(binding, evidence_keys);
    }

    fn check_callable_let_binding(
        &mut self,
        origin: HirOrigin,
        param_types: &[HirTyId],
        constraints: &[ConstraintFacts],
        declared_ty: Option<HirTyId>,
        value: HirExprId,
    ) -> HirTyId {
        let evidence_scope = self.evidence_scope_for_constraints(constraints);
        self.push_evidence_scope(evidence_scope);
        if let Some(expected) = declared_ty {
            self.push_expected_ty(expected);
        }
        let body_facts = check_expr(self, value);
        if declared_ty.is_some() {
            let _ = self.pop_expected_ty();
        }
        let _ = self.pop_evidence_scope();
        let result_ty = declared_ty.unwrap_or(body_facts.ty);
        self.type_mismatch(origin, result_ty, body_facts.ty);
        let params = self.alloc_ty_list(param_types.iter().copied());

        self.alloc_ty(HirTyKind::Arrow {
            params,
            ret: result_ty,
            is_effectful: false,
        })
    }

    fn seed_recursive_callable_scheme(&mut self, seed: &RecCallableSeed<'_>) {
        if !seed.mods.is_rec {
            return;
        }
        let Some(binding) = seed.binding else {
            return;
        };
        let builtins = self.builtins();
        let provisional_ret = seed.declared_ty.unwrap_or(builtins.unknown);
        let params = self.alloc_ty_list(seed.param_types.iter().copied());
        let provisional_ty = self.alloc_ty(HirTyKind::Arrow {
            params,
            ret: provisional_ret,
            is_effectful: false,
        });
        self.insert_let_binding_scheme(LetBindingSchemeInput {
            binding,
            ty: provisional_ty,
            type_params: (
                seed.type_params.to_vec().into_boxed_slice(),
                seed.type_param_kinds.to_vec().into_boxed_slice(),
            ),
            param_names: Box::default(),
            comptime_params: Box::default(),
            constraints: seed.constraints.to_vec().into_boxed_slice(),
        });
    }

    fn check_value_with_expected_ty(
        &mut self,
        declared_ty: Option<HirTyId>,
        value: HirExprId,
    ) -> ExprFacts {
        if let Some(expected) = declared_ty {
            self.push_expected_ty(expected);
        }
        let facts = check_expr(self, value);
        if declared_ty.is_some() {
            let _ = self.pop_expected_ty();
        }
        facts
    }

    fn check_non_callable_let_value(
        &mut self,
        is_module_stmt: bool,
        bound_name: Option<Ident>,
        type_params: &[Symbol],
        declared_ty: Option<HirTyId>,
        value: HirExprId,
    ) -> ExprFacts {
        let Some(name) = bound_name.filter(|_| is_module_stmt) else {
            return self.check_value_with_expected_ty(declared_ty, value);
        };

        match &self.expr(value).kind {
            HirExprKind::Data { variants, fields } => {
                self.check_bound_data(name, variants.clone(), fields.clone())
            }
            HirExprKind::Shape {
                constraints,
                members,
            } => self.check_bound_shape(
                value,
                name,
                type_params,
                constraints.clone(),
                members.clone(),
            ),
            _ => self.check_value_with_expected_ty(declared_ty, value),
        }
    }

    fn check_callable_let_expr(&mut self, input: CallableLetCheckInput) -> HirTyId {
        let CallableLetCheckInput {
            origin,
            exported,
            mods,
            pat,
            params,
            declared_ty,
            value,
            binding,
            receiver,
            type_params,
            type_param_kinds,
            constraints,
        } = input;
        if !matches!(
            self.pat(pat).kind,
            HirPatKind::Bind { .. } | HirPatKind::Wildcard
        ) {
            self.diag(
                origin.span,
                DiagKind::CallableLetRequiresSimpleBindingPattern,
                "",
            );
        }
        if exported && !type_params.is_empty() && !constraints.is_empty() {
            self.diag(
                origin.span,
                DiagKind::ExportedCallableRequiresConcreteConstraints,
                "",
            );
        }
        let param_list = self.params(params.clone());
        let param_names = param_list
            .iter()
            .map(|param| param.name.name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let comptime_params = param_list
            .iter()
            .map(|param| param.is_comptime)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let param_types = self.lower_params(params);
        self.seed_recursive_callable_scheme(&RecCallableSeed {
            binding,
            mods,
            param_types: &param_types,
            declared_ty,
            type_params: &type_params,
            type_param_kinds: &type_param_kinds,
            constraints: &constraints,
        });
        let ty =
            self.check_callable_let_binding(origin, &param_types, &constraints, declared_ty, value);
        binding.map_or(ty, |binding| {
            self.insert_let_binding_scheme(LetBindingSchemeInput {
                binding,
                ty,
                type_params: (type_params, type_param_kinds),
                param_names,
                comptime_params,
                constraints,
            });
            if let Some(receiver) = receiver {
                self.insert_attached_method(receiver.method.name, binding);
            }
            self.binding_type(binding).unwrap_or(ty)
        })
    }

    fn check_non_callable_let_expr(&mut self, input: NonCallableLetCheckInput) -> HirTyId {
        let NonCallableLetCheckInput {
            origin,
            exported: _exported,
            mods,
            pat,
            value,
            params,
            binding,
            declared_ty,
            is_module_stmt,
            bound_name,
            type_params,
            type_param_kinds,
            constraints,
        } = input;
        let builtins = self.builtins();
        if !constraints.is_empty() {
            self.diag(origin.span, DiagKind::ConstrainedNonCallableBinding, "");
        }
        let param_list = self.params(params);
        let param_names = param_list
            .iter()
            .map(|param| param.name.name)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let comptime_params = param_list
            .iter()
            .map(|param| param.is_comptime)
            .collect::<Vec<_>>()
            .into_boxed_slice();
        if mods.is_rec
            && let Some(binding) = binding
        {
            self.insert_binding_type(binding, declared_ty.unwrap_or(builtins.unknown));
        }
        let value_facts = self.check_non_callable_let_value(
            is_module_stmt,
            bound_name,
            &type_params,
            declared_ty,
            value,
        );
        self.validate_non_callable_let_pattern(
            origin,
            pat,
            mods.fallback.is_some(),
            value,
            value_facts.ty,
        );
        let ty = declared_ty.unwrap_or(value_facts.ty);
        self.type_mismatch(origin, ty, value_facts.ty);
        if let Some(binding) = binding {
            self.insert_let_binding_scheme(LetBindingSchemeInput {
                binding,
                ty,
                type_params: (type_params, type_param_kinds),
                param_names,
                comptime_params,
                constraints,
            });
            if is_explicit_comptime_expr(self, value)
                && let Some(value) = try_comptime_value(self, value)
            {
                self.insert_binding_comptime_value(binding, value);
            }
        }
        ty
    }

    fn check_let_expr(&mut self, input: LetExprInput) -> ExprFacts {
        let builtins = self.builtins();
        let is_module_stmt = self.in_module_stmt();
        let LetExprInput {
            expr_id,
            origin,
            expr_mods,
            mods,
            pat,
            type_params,
            receiver,
            has_param_clause,
            params,
            constraints,
            sig,
            value,
        } = input;
        if expr_mods.partial && expr_mods.native.is_some() {
            self.diag(origin.span, DiagKind::PartialForeignConflict, "");
        }
        let bound_name = bound_name_from_pat(self, pat);
        let binding = bound_name.and_then(|ident| self.binding_id_for_decl(ident));
        let (type_params, type_param_kinds) = self.lower_let_type_params(type_params);
        let type_param_scope = type_params
            .iter()
            .copied()
            .zip(type_param_kinds.iter().copied())
            .collect::<Vec<_>>();
        self.push_type_param_kinds(&type_param_scope);
        let constraints = self.lower_constraints(constraints);
        let declared_ty = sig.map(|expr| {
            let origin = self.expr(expr).origin;
            self.lower_type_expr(expr, origin)
        });
        if is_module_stmt
            && expr_mods.native.is_none()
            && !self.target_attrs_match(expr_mods.attrs.clone())
        {
            if let Some(binding) = binding {
                self.mark_gated_binding(binding);
            }
            self.pop_type_param_kinds();
            self.finish_let_expr(pat, value, self.builtins().unknown, binding, bound_name);
            return ExprFacts::new(builtins.unit);
        }

        let final_ty = self.check_let_final_ty(LetFinalTyInput {
            expr_id,
            origin,
            expr_mods,
            mods,
            pat,
            receiver,
            has_param_clause,
            params,
            value,
            binding,
            bound_name,
            type_params,
            type_param_kinds,
            constraints,
            declared_ty,
            is_module_stmt,
        });
        if let Some(fallback) = mods.fallback {
            let _ = check_expr(self, fallback);
        }

        self.pop_type_param_kinds();
        self.finish_let_expr(pat, value, final_ty, binding, bound_name);
        ExprFacts::new(builtins.unit)
    }

    fn check_let_final_ty(&mut self, input: LetFinalTyInput) -> HirTyId {
        if input.is_module_stmt && input.expr_mods.native.is_some() {
            return check_native_let(
                self,
                input.expr_id,
                input.type_params,
                input.type_param_kinds,
            )
            .unwrap_or_else(|| self.builtins().unknown);
        }
        if input.has_param_clause {
            return self.check_callable_let_expr(CallableLetCheckInput {
                origin: input.origin,
                exported: input.expr_mods.export.is_some(),
                mods: input.mods,
                pat: input.pat,
                params: input.params,
                declared_ty: input.declared_ty,
                value: input.value,
                binding: input.binding,
                receiver: input.receiver,
                type_params: input.type_params,
                type_param_kinds: input.type_param_kinds,
                constraints: input.constraints,
            });
        }
        self.check_non_callable_let_expr(NonCallableLetCheckInput {
            origin: input.origin,
            exported: input.expr_mods.export.is_some(),
            mods: input.mods,
            pat: input.pat,
            value: input.value,
            params: input.params,
            binding: input.binding,
            declared_ty: input.declared_ty,
            is_module_stmt: input.is_module_stmt,
            bound_name: input.bound_name,
            type_params: input.type_params,
            type_param_kinds: input.type_param_kinds,
            constraints: input.constraints,
        })
    }

    fn finish_let_expr(
        &mut self,
        pat: HirPatId,
        value: HirExprId,
        final_ty: HirTyId,
        binding: Option<NameBindingId>,
        bound_name: Option<Ident>,
    ) {
        if !bind_import_record_pattern(self, pat, value) {
            bind_pat(self, pat, final_ty);
        }
        if let Some(binding) = binding
            && let Some(target) = import_record_target_for_expr(self, value)
        {
            self.insert_binding_import_record_target(binding, target);
        }
        self.bind_tuple_import_targets(pat, value);
        if let Some(binding) = binding
            && let Some(name) = bound_name
        {
            self.mark_std_ffi_pointer_op_unsafe(binding, name);
        }
        if let Some(name) = bound_name {
            bind_structural_alias(self, name, value);
        }
    }

    fn bind_tuple_import_targets(&mut self, pat: HirPatId, value: HirExprId) {
        let HirPatKind::Tuple { items: pat_items } = self.pat(pat).kind else {
            return;
        };
        let value_items = match self.expr(value).kind {
            HirExprKind::Tuple { items } => Some(items),
            HirExprKind::Import { arg } => match self.expr(arg).kind {
                HirExprKind::Tuple { items } | HirExprKind::Sequence { exprs: items } => {
                    Some(items)
                }
                _ => None,
            },
            _ => None,
        };
        let Some(value_items) = value_items else {
            return;
        };
        let pat_items = self.pat_ids(pat_items);
        let value_items = self.expr_ids(value_items);
        if pat_items.len() != value_items.len() {
            return;
        }
        for (pat_item, value_item) in pat_items.into_iter().zip(value_items) {
            let HirPatKind::Bind { name } = self.pat(pat_item).kind else {
                continue;
            };
            let Some(binding) = self.binding_id_for_decl(name) else {
                continue;
            };
            if let Some(target) = import_record_target_for_expr(self, value_item)
                .or_else(|| self.static_import_target(self.expr(value_item).origin.span))
            {
                self.insert_binding_import_record_target(binding, target);
            }
        }
    }

    fn mark_std_ffi_pointer_op_unsafe(&mut self, binding: NameBindingId, name: Ident) {
        if is_std_ffi_unsafe_public_pointer_op(
            self.module_key().as_str(),
            self.resolve_symbol(name.name),
        ) {
            self.mark_unsafe_binding(binding);
        }
    }

    fn target_attrs_match(&self, attrs: SliceRange<HirAttr>) -> bool {
        let attrs = self.attrs(attrs);
        let target = self.target();
        for attr in &attrs {
            let path = self.attr_path(attr);
            if path.as_slice() != ["target"] {
                continue;
            }
            if !self.when_attr_matches(target, attr) {
                return false;
            }
        }
        true
    }
}

fn is_explicit_comptime_expr(ctx: &CheckPass<'_, '_, '_>, expr: HirExprId) -> bool {
    matches!(
        ctx.expr(expr).kind,
        HirExprKind::Prefix {
            op: HirPrefixOp::Known,
            ..
        }
    )
}

fn is_std_ffi_unsafe_public_pointer_op(module_key: &str, name: &str) -> bool {
    (module_key == "@std/ffi" || module_key.ends_with("ffi.ms"))
        && matches!(name, "offset" | "read" | "write")
}
