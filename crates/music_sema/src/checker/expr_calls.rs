use std::collections::{HashMap, HashSet};

use music_arena::SliceRange;
use music_hir::{HirArg, HirDim, HirExprId, HirExprKind, HirOrigin, HirTyId, HirTyKind};
use music_names::{Ident, NameBindingId, Symbol};

use crate::api::{ConstraintKind, ExprFacts, ExprMemberKind};
use crate::effects::EffectRow;

use super::const_eval::try_comptime_value;
use super::decls::{call_effects_for_expr, module_export_for_expr, module_target_for_expr};
use super::exprs::{check_expr, peel_mut_ty};
use super::schemes::{BindingScheme, ConstraintObligation};
use super::{CheckPass, DiagKind};

pub(super) fn check_call_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    origin: HirOrigin,
    callee: HirExprId,
    args: SliceRange<HirArg>,
) -> ExprFacts {
    ctx.check_call_expr_impl(origin, callee, args)
}

pub(super) fn check_apply_expr(
    ctx: &mut CheckPass<'_, '_, '_>,
    expr_id: HirExprId,
    origin: HirOrigin,
    callee: HirExprId,
    args: SliceRange<HirExprId>,
) -> ExprFacts {
    ctx.check_apply_expr_impl(expr_id, origin, callee, args)
}

struct CallArgChecker<'ctx, 'interner, 'env, 'borrow, 'filled, 'effects> {
    ctx: &'borrow mut CheckPass<'ctx, 'interner, 'env>,
    param_index: usize,
    filled: &'filled mut [bool],
    effects: &'effects mut EffectRow,
    has_runtime_spread: bool,
}

impl CallArgChecker<'_, '_, '_, '_, '_, '_> {
    fn check_args(
        &mut self,
        args: &[HirArg],
        params: &[HirTyId],
        param_names: &[Symbol],
        comptime_params: &[bool],
    ) {
        let mut named_seen = HashSet::<Symbol>::new();
        let mut in_named_suffix = false;
        for arg in args {
            let Some(name) = arg.name else {
                self.check_positional_arg_order(arg, in_named_suffix);
                self.check_arg(arg, params, comptime_params);
                continue;
            };
            if self.check_named_arg_blockers(arg, name, params, comptime_params) {
                continue;
            }
            in_named_suffix = true;
            self.check_named_arg(
                arg,
                name,
                params,
                param_names,
                comptime_params,
                &mut named_seen,
            );
        }
    }

    fn check_positional_arg_order(&mut self, arg: &HirArg, in_named_suffix: bool) {
        if in_named_suffix {
            let diag = if arg.spread {
                DiagKind::CallSpreadAfterNamedArgument
            } else {
                DiagKind::CallPositionalAfterNamedArgument
            };
            let span = self.ctx.expr(arg.expr).origin.span;
            self.ctx.diag(span, diag, "");
        }
    }

    fn check_named_arg_blockers(
        &mut self,
        arg: &HirArg,
        name: Ident,
        params: &[HirTyId],
        comptime_params: &[bool],
    ) -> bool {
        if arg.spread {
            self.ctx
                .diag(name.span, DiagKind::CallNamedSpreadArgument, "");
            self.check_arg(arg, params, comptime_params);
            return true;
        }
        if self.has_runtime_spread {
            self.ctx.diag(
                name.span,
                DiagKind::CallNamedArgumentsAfterRuntimeSpread,
                "",
            );
            self.ctx
                .check_named_call_arg(name, arg.expr, None, false, self.effects);
            return true;
        }
        false
    }

    fn check_named_arg(
        &mut self,
        arg: &HirArg,
        name: Ident,
        params: &[HirTyId],
        param_names: &[Symbol],
        comptime_params: &[bool],
        named_seen: &mut HashSet<Symbol>,
    ) {
        if !named_seen.insert(name.name) {
            self.ctx
                .diag(name.span, DiagKind::CallNamedArgumentDuplicate, "");
        }
        let Some(expected_index) = self.named_arg_index(name, param_names) else {
            self.ctx
                .check_named_call_arg(name, arg.expr, None, false, self.effects);
            return;
        };
        if self.filled.get(expected_index).copied().unwrap_or(false) {
            self.ctx
                .diag(name.span, DiagKind::CallNamedArgumentAlreadyProvided, "");
        }
        let expected = params.get(expected_index).copied();
        let is_comptime = comptime_params
            .get(expected_index)
            .copied()
            .unwrap_or(false);
        self.ctx
            .check_named_call_arg(name, arg.expr, expected, is_comptime, self.effects);
        if let Some(slot) = self.filled.get_mut(expected_index) {
            *slot = true;
        }
    }

    fn named_arg_index(&mut self, name: Ident, param_names: &[Symbol]) -> Option<usize> {
        let index = param_names.iter().position(|param| *param == name.name);
        if index.is_none() {
            let argument_name = self.ctx.resolve_symbol(name.name).to_owned();
            self.ctx.diag_message(
                name.span,
                DiagKind::CallNamedArgumentUnknown,
                format!("unknown named call argument `{argument_name}`"),
                format!("unknown named call argument `{argument_name}`"),
            );
        }
        index
    }

    fn check_arg(&mut self, arg: &HirArg, params: &[HirTyId], comptime_params: &[bool]) {
        let builtins = self.ctx.builtins();
        if !arg.spread {
            let is_comptime = comptime_params
                .get(self.param_index)
                .copied()
                .unwrap_or(false);
            let expected = params
                .get(self.param_index)
                .copied()
                .unwrap_or(builtins.unknown);
            self.ctx.push_expected_ty(expected);
            let facts = check_expr(self.ctx, arg.expr);
            let _ = self.ctx.pop_expected_ty();
            self.effects.union_with(&facts.effects);
            let arg_origin = self.ctx.expr(arg.expr).origin;
            self.ctx.type_mismatch(arg_origin, expected, facts.ty);
            if is_comptime {
                if let Some(value) = try_comptime_value(self.ctx, arg.expr) {
                    self.ctx.set_expr_comptime_value(arg.expr, value);
                }
            }
            if let Some(slot) = self.filled.get_mut(self.param_index) {
                *slot = true;
            }
            self.param_index = self.param_index.saturating_add(1);
            return;
        }

        let facts = check_expr(self.ctx, arg.expr);
        self.effects.union_with(&facts.effects);
        let spread_origin = self.ctx.expr(arg.expr).origin;
        let spread_ty = peel_mut_ty(self.ctx, facts.ty);
        self.check_spread_arg(spread_origin, arg.expr, spread_ty, params);
    }

    fn check_spread_arg(
        &mut self,
        origin: HirOrigin,
        spread_expr: HirExprId,
        spread_ty: HirTyId,
        params: &[HirTyId],
    ) {
        let builtins = self.ctx.builtins();
        match self.ctx.ty(spread_ty).kind {
            HirTyKind::Tuple { items } => {
                let item_tys = self.ctx.ty_ids(items);
                for found in item_tys {
                    let expected = params
                        .get(self.param_index)
                        .copied()
                        .unwrap_or(builtins.unknown);
                    self.ctx.type_mismatch(origin, expected, found);
                    if let Some(slot) = self.filled.get_mut(self.param_index) {
                        *slot = true;
                    }
                    self.param_index = self.param_index.saturating_add(1);
                }
            }
            HirTyKind::Array { dims, item } => {
                self.check_array_spread(origin, dims, item, params);
            }
            HirTyKind::Seq { item: seq_item } => {
                self.has_runtime_spread = true;
                let expected = params
                    .get(self.param_index)
                    .copied()
                    .unwrap_or(builtins.unknown);
                self.ctx.type_mismatch(origin, expected, seq_item);
            }
            HirTyKind::Range { bound: range_item }
            | HirTyKind::ClosedRange { bound: range_item }
            | HirTyKind::PartialRangeFrom { bound: range_item }
            | HirTyKind::PartialRangeUpTo { bound: range_item }
            | HirTyKind::PartialRangeThru { bound: range_item } => {
                self.has_runtime_spread = true;
                let expected = params
                    .get(self.param_index)
                    .copied()
                    .unwrap_or(builtins.unknown);
                self.ctx.type_mismatch(origin, expected, range_item);
                let rangeable_symbol = self.ctx.known().rangeable;
                let rangeable = self.ctx.named_type_for_symbol(rangeable_symbol);
                let obligation = super::schemes::ConstraintObligation {
                    kind: ConstraintKind::Implements,
                    subject: range_item,
                    value: rangeable,
                    class_key: self
                        .ctx
                        .class_facts_by_name(rangeable_symbol)
                        .map(|facts| facts.key.clone()),
                };
                if let Some(evidence) = self
                    .ctx
                    .resolve_obligations_to_evidence(origin, &[obligation])
                    && !evidence.is_empty()
                {
                    self.ctx.set_expr_evidence(spread_expr, evidence);
                }
            }
            _ => self.ctx.diag(
                origin.span,
                DiagKind::InvalidSpreadSource,
                "call spread source must expand to arguments",
            ),
        }
    }

    fn check_array_spread(
        &mut self,
        origin: HirOrigin,
        dims: SliceRange<HirDim>,
        item: HirTyId,
        params: &[HirTyId],
    ) {
        let builtins = self.ctx.builtins();
        let dims_vec = self.ctx.dims(dims);
        if dims_vec.is_empty() {
            if self.ctx.ty(item).kind == HirTyKind::Any {
                self.has_runtime_spread = true;
            } else {
                self.ctx
                    .diag(origin.span, DiagKind::CallRuntimeSpreadRequiresArrayAny, "");
            }
            return;
        }
        if dims_vec.len() != 1 {
            self.ctx
                .diag(origin.span, DiagKind::CallSpreadRequiresTupleOrArray, "");
            return;
        }
        match dims_vec[0] {
            HirDim::Int(len) => {
                for _ in 0..len {
                    let expected = params
                        .get(self.param_index)
                        .copied()
                        .unwrap_or(builtins.unknown);
                    self.ctx.type_mismatch(origin, expected, item);
                    if let Some(slot) = self.filled.get_mut(self.param_index) {
                        *slot = true;
                    }
                    self.param_index = self.param_index.saturating_add(1);
                }
            }
            HirDim::Unknown | HirDim::Name(_) if self.ctx.ty(item).kind == HirTyKind::Any => {
                self.has_runtime_spread = true;
            }
            _ => self
                .ctx
                .diag(origin.span, DiagKind::CallRuntimeSpreadRequiresArrayAny, ""),
        }
    }
}

impl CheckPass<'_, '_, '_> {
    fn check_call_expr_impl(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        args: SliceRange<HirArg>,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let callee_facts = check_expr(self, callee);
        let (params, mut ret) =
            if let HirTyKind::Arrow { params, ret, .. } = self.ty(callee_facts.ty).kind {
                (self.ty_ids(params), ret)
            } else {
                self.invalid_call_target(callee, callee_facts.ty);
                return ExprFacts::new(builtins.unknown, callee_facts.effects);
            };

        self.validate_unsafe_call(origin, callee);

        let args_vec = self.args(args);
        let mut effects = callee_facts.effects;
        let mut filled = vec![false; params.len()];
        let param_names = self.call_param_names(callee, params.len());
        let comptime_params = self.call_comptime_params(callee, params.len());
        let (param_index, has_runtime_spread) = {
            let mut state = CallArgChecker {
                ctx: self,
                param_index: 0,
                filled: &mut filled,
                effects: &mut effects,
                has_runtime_spread: false,
            };
            state.check_args(&args_vec, &params, &param_names, &comptime_params);
            (state.param_index, state.has_runtime_spread)
        };

        if !has_runtime_spread {
            if param_index > params.len() || filled.iter().any(|filled| !filled) {
                self.call_arity_mismatch(origin, callee, params.len(), args_vec.len());
            }
        } else if param_index > params.len() {
            self.call_arity_mismatch(origin, callee, params.len(), args_vec.len());
        }

        if let Some(class_ret) = self.resolve_class_member_call_evidence(origin, callee, &args_vec)
        {
            ret = class_ret;
        }
        self.merge_call_effects(origin, callee, &mut effects);
        ExprFacts::new(ret, effects)
    }

    fn resolve_class_member_call_evidence(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        args: &[HirArg],
    ) -> Option<HirTyId> {
        let fact = self.expr_member_fact(callee)?;
        if fact.kind != ExprMemberKind::ClassMember {
            return None;
        }
        let HirExprKind::Field { base, .. } = self.expr(callee).kind else {
            return None;
        };
        let HirExprKind::Name { name } = self.expr(base).kind else {
            return None;
        };
        let class_name = name.name;
        let class = self.class_facts_by_name(class_name)?.clone();
        let member = fact
            .index
            .and_then(|index| class.members.get(usize::from(index)))
            .cloned()?;
        let mut subst = HashMap::new();
        for (member_param, arg) in member.params.iter().copied().zip(args.iter()) {
            let arg_ty = check_expr(self, arg.expr).ty;
            if !self.unify_ty_for_type_params(&class.type_params, member_param, arg_ty, &mut subst)
            {
                let expected = self.substitute_ty(member_param, &subst);
                let arg_origin = self.expr(arg.expr).origin;
                self.type_mismatch(arg_origin, expected, arg_ty);
            }
        }
        let subject = class
            .type_params
            .first()
            .and_then(|param| subst.get(param).copied())
            .unwrap_or_else(|| {
                let Some(name) = class.type_params.first().copied() else {
                    return self.builtins().unknown;
                };
                self.alloc_ty(HirTyKind::Named {
                    name,
                    args: SliceRange::EMPTY,
                })
            });
        if subject == self.builtins().unknown {
            return Some(self.substitute_ty(member.result, &subst));
        }
        let class_ty = self.alloc_ty(HirTyKind::Named {
            name: class_name,
            args: SliceRange::EMPTY,
        });
        let obligation = ConstraintObligation {
            kind: ConstraintKind::Implements,
            subject,
            value: class_ty,
            class_key: Some(class.key),
        };
        if let Some(evidence) = self.resolve_obligations_to_evidence(origin, &[obligation])
            && !evidence.is_empty()
        {
            self.set_expr_evidence(callee, evidence);
        }
        Some(self.substitute_ty(member.result, &subst))
    }

    fn validate_unsafe_call(&mut self, origin: HirOrigin, callee: HirExprId) {
        if self.in_unsafe_block() {
            return;
        }
        let callee = self.peel_call_type_application(callee);
        if self.is_std_ffi_unsafe_pointer_field(callee) {
            self.diag(origin.span, DiagKind::UnsafeCallRequiresUnsafeBlock, "");
            return;
        }
        let binding = match self.expr(callee).kind {
            HirExprKind::Name { name } => self.binding_id_for_use(name),
            HirExprKind::Field { .. } => self.expr_dot_callable_binding(callee),
            _ => None,
        };
        if binding.is_some_and(|binding| self.is_unsafe_binding(binding)) {
            self.diag(origin.span, DiagKind::UnsafeCallRequiresUnsafeBlock, "");
        }
    }

    fn invalid_call_target(&mut self, callee: HirExprId, found: HirTyId) {
        let subject = self.expr_subject(callee);
        let found = self.render_ty(found);
        let origin = self.expr(callee).origin;
        self.diag_message(
            origin.span,
            DiagKind::InvalidCallTarget,
            format!("callee `{subject}` lacks callable type `{found}`"),
            format!("callee `{subject}` has type `{found}` here"),
        );
    }

    fn call_arity_mismatch(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        expected: usize,
        found: usize,
    ) {
        let subject = self.expr_subject(callee);
        self.diag_message(
            origin.span,
            DiagKind::CallArityMismatch,
            format!("call `{subject}` expected `{expected}` arguments, found `{found}`"),
            format!("call `{subject}` has `{found}` arguments here"),
        );
    }

    fn expr_subject(&self, expr: HirExprId) -> String {
        match &self.expr(expr).kind {
            HirExprKind::Name { name } | HirExprKind::Field { name, .. } => {
                self.resolve_symbol(name.name).to_owned()
            }
            HirExprKind::Lit { .. } => "literal".to_owned(),
            HirExprKind::Call { .. } => "call result".to_owned(),
            HirExprKind::Index { .. } => "index result".to_owned(),
            _ => "expression".to_owned(),
        }
    }

    fn check_apply_expr_impl(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        callee: HirExprId,
        args: SliceRange<HirExprId>,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let callee_facts = check_expr(self, callee);
        let effectful_eval = callee_facts.effects;
        let args = self
            .expr_ids(args)
            .into_iter()
            .map(|arg| {
                let arg_origin = self.expr(arg).origin;
                self.lower_type_expr(arg, arg_origin)
            })
            .collect::<Vec<_>>();
        let instantiated = if let Some(scheme) = self.callable_scheme_for_expr(callee) {
            if scheme.type_params.is_empty()
                && matches!(self.ty(scheme.ty).kind, HirTyKind::Pi { .. })
            {
                self.instantiate_pi_ty(origin, scheme.ty, &args)
            } else {
                self.instantiate_binding_scheme(origin, &scheme, &args)
            }
        } else {
            self.instantiate_pi_ty(origin, callee_facts.ty, &args)
        };
        let Some(instantiated) = instantiated else {
            self.diag(origin.span, DiagKind::InvalidTypeApplication, "");
            return ExprFacts::new(builtins.unknown, effectful_eval);
        };
        if let Some(target) = module_target_for_expr(self, callee) {
            self.set_expr_module_target(expr_id, target);
        }
        if let Some(evidence) =
            self.resolve_obligations_to_evidence(origin, &instantiated.obligations)
            && !evidence.is_empty()
        {
            self.set_expr_evidence(expr_id, evidence);
        }
        self.set_expr_callable_effects(expr_id, instantiated.effects.clone());
        ExprFacts::new(instantiated.ty, effectful_eval)
    }

    fn check_named_call_arg(
        &mut self,
        name: Ident,
        expr: HirExprId,
        expected: Option<HirTyId>,
        is_comptime: bool,
        effects: &mut EffectRow,
    ) {
        let builtins = self.builtins();
        let expected = expected.unwrap_or(builtins.unknown);
        self.push_expected_ty(expected);
        let facts = check_expr(self, expr);
        let _ = self.pop_expected_ty();
        effects.union_with(&facts.effects);
        let origin = self.expr(expr).origin;
        let name = self.resolve_symbol(name.name).to_owned();
        self.type_mismatch_for(
            &format!("call argument `{name}`"),
            origin,
            expected,
            facts.ty,
        );
        if is_comptime {
            if let Some(value) = try_comptime_value(self, expr) {
                self.set_expr_comptime_value(expr, value);
            }
        }
    }

    fn merge_call_effects(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        effects: &mut EffectRow,
    ) {
        if let Some(extra) = call_effects_for_expr(self, callee) {
            effects.union_with(&extra);
            return;
        }
        let Some(scheme) = self.callable_scheme_for_expr(callee) else {
            return;
        };
        if !scheme.type_params.is_empty() {
            return;
        }
        let instantiated = self.instantiate_monomorphic_scheme(&scheme);
        let _ = self.resolve_obligations_to_evidence(origin, &instantiated.obligations);
        effects.union_with(&instantiated.effects);
    }

    fn callable_scheme_for_expr(&mut self, expr: HirExprId) -> Option<BindingScheme> {
        let expr = self.peel_call_type_application(expr);
        if let Some(binding) = self.expr_dot_callable_binding(expr)
            && let Some(scheme) = self.binding_scheme(binding).cloned()
        {
            return self.strip_dot_callable_scheme(scheme);
        }
        match self.expr(expr).kind {
            HirExprKind::Name { name } => self
                .binding_id_for_use(name)
                .and_then(|binding| self.binding_scheme(binding).cloned()),
            HirExprKind::Field { base, name, .. } => {
                self.std_ffi_ptr_field_scheme(base, name).or_else(|| {
                    module_export_for_expr(self, base, name)
                        .map(|(surface, export)| self.scheme_from_export(&surface, &export))
                })
            }
            _ => None,
        }
    }

    fn peel_call_type_application(&self, expr: HirExprId) -> HirExprId {
        match self.expr(expr).kind {
            HirExprKind::Apply { callee, .. } => callee,
            _ => expr,
        }
    }

    fn std_ffi_ptr_field_scheme(&mut self, base: HirExprId, name: Ident) -> Option<BindingScheme> {
        let HirExprKind::Field {
            base: module_expr,
            name: ptr_name,
            ..
        } = self.expr(base).kind
        else {
            return None;
        };
        if self.resolve_symbol(ptr_name.name) != "ptr" {
            return None;
        }
        let (surface, _) = module_export_for_expr(self, module_expr, ptr_name)?;
        if !is_std_ffi_module(surface.module_key().as_str()) {
            return None;
        }
        let export = surface
            .exported_value(self.resolve_symbol(name.name))?
            .clone();
        Some(self.scheme_from_export(&surface, &export))
    }

    fn is_std_ffi_unsafe_pointer_field(&mut self, expr: HirExprId) -> bool {
        let HirExprKind::Field { base, name, .. } = self.expr(expr).kind else {
            return false;
        };
        if !matches!(self.resolve_symbol(name.name), "offset" | "read" | "write") {
            return false;
        }
        self.std_ffi_ptr_field_scheme(base, name).is_some()
    }

    fn strip_dot_callable_scheme(&mut self, mut scheme: BindingScheme) -> Option<BindingScheme> {
        let HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } = self.ty(scheme.ty).kind
        else {
            return None;
        };
        let params = self.ty_ids(params);
        if params.is_empty() {
            return None;
        }
        let tail_params = self.alloc_ty_list(params.into_iter().skip(1));
        scheme.param_names = scheme.param_names.into_iter().skip(1).collect();
        scheme.comptime_params = scheme.comptime_params.into_iter().skip(1).collect();
        scheme.ty = self.alloc_ty(HirTyKind::Arrow {
            params: tail_params,
            ret,
            is_effectful,
        });
        Some(scheme)
    }

    fn call_param_names(&mut self, callee: HirExprId, param_count: usize) -> Box<[Symbol]> {
        self.callable_scheme_for_expr(callee)
            .map(|scheme| scheme.param_names)
            .or_else(|| self.effect_op_param_names(callee))
            .filter(|names| names.len() == param_count)
            .unwrap_or_default()
    }

    fn call_comptime_params(&mut self, callee: HirExprId, param_count: usize) -> Box<[bool]> {
        self.callable_scheme_for_expr(callee)
            .map(|scheme| scheme.comptime_params)
            .filter(|params| params.len() == param_count)
            .unwrap_or_default()
    }

    fn effect_op_param_names(&self, callee: HirExprId) -> Option<Box<[Symbol]>> {
        let HirExprKind::Field { base, name, .. } = self.expr(callee).kind else {
            return None;
        };
        let HirExprKind::Name { name: effect_name } = self.expr(base).kind else {
            return None;
        };
        self.effect_def(self.resolve_symbol(effect_name.name))
            .and_then(|effect| effect.op(self.resolve_symbol(name.name)))
            .map(|op| op.param_names().to_vec().into_boxed_slice())
    }

    pub(super) fn dot_callable_requires_mut(&self, binding: NameBindingId) -> bool {
        let Some(scheme) = self.binding_scheme(binding) else {
            return false;
        };
        let HirTyKind::Arrow { params, .. } = self.ty(scheme.ty).kind else {
            return false;
        };
        self.ty_ids(params)
            .first()
            .copied()
            .is_some_and(|ty| matches!(self.ty(ty).kind, HirTyKind::Mut { .. }))
    }
}

fn is_std_ffi_module(module_key: &str) -> bool {
    module_key == "@std/ffi" || module_key.ends_with("ffi/index.ms")
}
