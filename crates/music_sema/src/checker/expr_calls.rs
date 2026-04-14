use music_arena::SliceRange;
use music_hir::{HirArg, HirDim, HirExprId, HirExprKind, HirOrigin, HirTyId, HirTyKind};
use music_names::NameBindingId;

use crate::api::{ConstraintKind, ExprFacts};
use crate::effects::EffectRow;

use super::decls::{call_effects_for_expr, module_export_for_expr, module_target_for_expr};
use super::exprs::{check_expr, peel_mut_ty};
use super::schemes::BindingScheme;
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

impl CheckPass<'_, '_, '_> {
    fn check_call_expr_impl(
        &mut self,
        origin: HirOrigin,
        callee: HirExprId,
        args: SliceRange<HirArg>,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let callee_facts = check_expr(self, callee);
        let (params, ret) =
            if let HirTyKind::Arrow { params, ret, .. } = self.ty(callee_facts.ty).kind {
                (self.ty_ids(params), ret)
            } else {
                self.diag(origin.span, DiagKind::InvalidCallTarget, "");
                return ExprFacts::new(builtins.unknown, callee_facts.effects);
            };

        let args_vec = self.args(args);
        let mut effects = callee_facts.effects;
        let mut param_index = 0usize;
        let mut has_runtime_spread = false;

        for arg in &args_vec {
            self.check_call_arg(
                arg,
                &params,
                &mut param_index,
                &mut effects,
                &mut has_runtime_spread,
            );
        }

        if !has_runtime_spread {
            if param_index != params.len() {
                self.diag(origin.span, DiagKind::CallArityMismatch, "");
            }
        } else if param_index > params.len() {
            self.diag(origin.span, DiagKind::CallArityMismatch, "");
        }

        self.merge_call_effects(origin, callee, &mut effects);
        ExprFacts::new(ret, effects)
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
        let Some(scheme) = self.callable_scheme_for_expr(callee) else {
            self.diag(origin.span, DiagKind::InvalidTypeApplication, "");
            return ExprFacts::new(builtins.unknown, effectful_eval);
        };
        let Some(instantiated) = self.instantiate_binding_scheme(origin, &scheme, &args) else {
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

    fn check_call_arg(
        &mut self,
        arg: &HirArg,
        params: &[HirTyId],
        param_index: &mut usize,
        effects: &mut EffectRow,
        has_runtime_spread: &mut bool,
    ) {
        let builtins = self.builtins();
        if !arg.spread {
            let expected = params
                .get(*param_index)
                .copied()
                .unwrap_or(builtins.unknown);
            self.push_expected_ty(expected);
            let facts = check_expr(self, arg.expr);
            let _ = self.pop_expected_ty();
            effects.union_with(&facts.effects);
            let arg_origin = self.expr(arg.expr).origin;
            self.type_mismatch(arg_origin, expected, facts.ty);
            *param_index = param_index.saturating_add(1);
            return;
        }

        let facts = check_expr(self, arg.expr);
        effects.union_with(&facts.effects);
        let spread_origin = self.expr(arg.expr).origin;
        let spread_ty = peel_mut_ty(self, facts.ty);
        self.check_call_spread_arg(
            spread_origin,
            arg.expr,
            spread_ty,
            params,
            param_index,
            has_runtime_spread,
        );
    }

    fn check_call_spread_arg(
        &mut self,
        origin: HirOrigin,
        spread_expr: HirExprId,
        spread_ty: HirTyId,
        params: &[HirTyId],
        param_index: &mut usize,
        has_runtime_spread: &mut bool,
    ) {
        let builtins = self.builtins();
        match self.ty(spread_ty).kind {
            HirTyKind::Tuple { items } => {
                let item_tys = self.ty_ids(items);
                for found in item_tys {
                    let expected = params
                        .get(*param_index)
                        .copied()
                        .unwrap_or(builtins.unknown);
                    self.type_mismatch(origin, expected, found);
                    *param_index = param_index.saturating_add(1);
                }
            }
            HirTyKind::Array { dims, item } => {
                self.check_call_array_spread(
                    origin,
                    dims,
                    item,
                    params,
                    param_index,
                    has_runtime_spread,
                );
            }
            HirTyKind::Seq { item: seq_item } => {
                *has_runtime_spread = true;
                let expected = params
                    .get(*param_index)
                    .copied()
                    .unwrap_or(builtins.unknown);
                self.type_mismatch(origin, expected, seq_item);
            }
            HirTyKind::Range { bound: range_item }
            | HirTyKind::ClosedRange { bound: range_item }
            | HirTyKind::PartialRangeFrom { bound: range_item }
            | HirTyKind::PartialRangeUpTo { bound: range_item }
            | HirTyKind::PartialRangeThru { bound: range_item } => {
                *has_runtime_spread = true;
                let expected = params
                    .get(*param_index)
                    .copied()
                    .unwrap_or(builtins.unknown);
                self.type_mismatch(origin, expected, range_item);
                let rangeable_symbol = self.known().rangeable;
                let rangeable = self.named_type_for_symbol(rangeable_symbol);
                let obligation = super::schemes::ConstraintObligation {
                    kind: ConstraintKind::Implements,
                    subject: range_item,
                    value: rangeable,
                    class_key: self
                        .class_facts_by_name(rangeable_symbol)
                        .map(|facts| facts.key.clone()),
                };
                if let Some(evidence) = self.resolve_obligations_to_evidence(origin, &[obligation])
                    && !evidence.is_empty()
                {
                    self.set_expr_evidence(spread_expr, evidence);
                }
            }
            _ => self.diag(
                origin.span,
                DiagKind::InvalidSpreadSource,
                "call spread source must expand to arguments",
            ),
        }
    }

    fn check_call_array_spread(
        &mut self,
        origin: HirOrigin,
        dims: SliceRange<HirDim>,
        item: HirTyId,
        params: &[HirTyId],
        param_index: &mut usize,
        has_runtime_spread: &mut bool,
    ) {
        let builtins = self.builtins();
        let dims_vec = self.dims(dims);
        if dims_vec.is_empty() {
            if self.ty(item).kind == HirTyKind::Any {
                *has_runtime_spread = true;
            } else {
                self.diag(origin.span, DiagKind::CallRuntimeSpreadRequiresArrayAny, "");
            }
            return;
        }
        if dims_vec.len() != 1 {
            self.diag(origin.span, DiagKind::CallSpreadRequiresTupleOrArray, "");
            return;
        }
        match dims_vec[0] {
            HirDim::Int(len) => {
                for _ in 0..len {
                    let expected = params
                        .get(*param_index)
                        .copied()
                        .unwrap_or(builtins.unknown);
                    self.type_mismatch(origin, expected, item);
                    *param_index = param_index.saturating_add(1);
                }
            }
            HirDim::Unknown | HirDim::Name(_) if self.ty(item).kind == HirTyKind::Any => {
                *has_runtime_spread = true;
            }
            _ => self.diag(origin.span, DiagKind::CallRuntimeSpreadRequiresArrayAny, ""),
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
        if let Some(binding) = self.expr_attached_binding(expr)
            && let Some(scheme) = self.binding_scheme(binding).cloned()
        {
            return self.strip_attached_receiver_scheme(scheme);
        }
        match self.expr(expr).kind {
            HirExprKind::Name { name } => self
                .binding_id_for_use(name)
                .and_then(|binding| self.binding_scheme(binding).cloned()),
            HirExprKind::Field { base, name, .. } => module_export_for_expr(self, base, name)
                .map(|(surface, export)| self.scheme_from_export(&surface, &export)),
            _ => None,
        }
    }

    fn strip_attached_receiver_scheme(
        &mut self,
        mut scheme: BindingScheme,
    ) -> Option<BindingScheme> {
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
        scheme.ty = self.alloc_ty(HirTyKind::Arrow {
            params: tail_params,
            ret,
            is_effectful,
        });
        Some(scheme)
    }

    pub(super) fn attached_method_requires_mut(&self, binding: NameBindingId) -> bool {
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
