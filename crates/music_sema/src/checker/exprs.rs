use std::collections::BTreeMap;

use music_arena::SliceRange;
use music_hir::{
    HirAccessKind, HirBinaryOp, HirBinder, HirCaseArm, HirConstraint, HirExprId, HirExprKind,
    HirLitId, HirLitKind, HirMemberDef, HirOrigin, HirParam, HirPartialRangeKind, HirPrefixOp,
    HirQuoteKind, HirRecordItem, HirSpliceKind, HirTemplatePart, HirTyField, HirTyId, HirTyKind,
};
use music_names::{Ident, Symbol};

use crate::api::{ConstraintKind, ExprFacts};

use super::decls::{LetExprInput, check_let_expr, module_export_for_expr};
use super::expr_calls::{check_apply_expr, check_call_expr};
use super::patterns::bind_pat;
use super::{CheckPass, DiagKind};
use crate::effects::EffectRow;

pub fn check_module_root(ctx: &mut CheckPass<'_, '_, '_>, id: HirExprId) -> ExprFacts {
    ctx.check_module_root(id)
}

pub fn check_expr(ctx: &mut CheckPass<'_, '_, '_>, id: HirExprId) -> ExprFacts {
    ctx.check_expr(id)
}

pub(super) fn peel_mut_ty(ctx: &CheckPass<'_, '_, '_>, ty: HirTyId) -> HirTyId {
    ctx.peel_mut_ty(ty)
}

impl CheckPass<'_, '_, '_> {
    fn check_module_root(&mut self, id: HirExprId) -> ExprFacts {
        let ctx = self;
        ctx.check_module_stmt(id)
    }

    fn check_expr(&mut self, id: HirExprId) -> ExprFacts {
        let ctx = self;
        let expr = ctx.expr(id);
        let origin = expr.origin;
        let attrs = expr.mods.attrs;
        if !attrs.is_empty() {
            ctx.validate_expr_attrs(origin, attrs, id);
        }
        let facts = ctx.check_expr_kind(id);
        ctx.set_expr_facts(id, facts.clone());
        facts
    }

    fn check_expr_kind(&mut self, id: HirExprId) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let expr = ctx.expr(id);
        let kind = expr.kind.clone();
        match kind {
            HirExprKind::Error => ExprFacts::new(builtins.error, EffectRow::empty()),
            HirExprKind::Name { name } => ctx.check_name_expr(id, name),
            HirExprKind::Lit { lit } => ctx.check_lit_expr(lit),
            HirExprKind::Template { parts } => ctx.check_template_expr(parts),
            HirExprKind::Sequence { exprs } => ctx.check_sequence_expr(exprs),
            HirExprKind::Tuple { items } => ctx.check_tuple_expr(items),
            HirExprKind::Array { items } => ctx.check_array_expr(items),
            HirExprKind::ArrayTy { dims, item } => ctx.check_array_ty_expr(&dims, item),
            HirExprKind::HandlerTy {
                effect,
                input,
                output,
            } => ctx.check_handler_ty_expr(effect, input, output),
            HirExprKind::Record { items } => ctx.check_record_expr(items),
            HirExprKind::Variant { tag, args } => ctx.check_variant_expr(tag, args),
            HirExprKind::Pi {
                binder,
                binder_ty,
                ret,
                is_effectful,
            } => ctx.check_pi_expr(binder, binder_ty, ret, is_effectful),
            HirExprKind::Lambda {
                params,
                ret_ty,
                body,
            } => ctx.check_lambda_expr(params, ret_ty, body),
            HirExprKind::Call { callee, args } => check_call_expr(ctx, expr.origin, callee, args),
            HirExprKind::Apply { callee, args } => {
                check_apply_expr(ctx, id, expr.origin, callee, args)
            }
            HirExprKind::Index { base, args } => ctx.check_index_expr(expr.origin, base, args),
            HirExprKind::Field { base, access, name } => {
                ctx.check_field_expr(id, expr.origin, base, access, name)
            }
            HirExprKind::RecordUpdate { base, items } => {
                ctx.check_record_update_expr(expr.origin, base, items)
            }
            HirExprKind::TypeTest { base, ty, as_name } => {
                ctx.check_type_test_expr(id, base, ty, as_name)
            }
            HirExprKind::TypeCast { base, ty } => ctx.check_type_cast_expr(base, ty),
            HirExprKind::Prefix { op, expr: inner } => {
                ctx.check_prefix_expr(expr.origin, &op, inner)
            }
            HirExprKind::PartialRange { kind, expr: inner } => {
                ctx.check_partial_range_expr(id, expr.origin, kind, inner)
            }
            HirExprKind::Binary { op, left, right } => {
                ctx.check_binary_expr(id, expr.origin, &op, left, right)
            }
            HirExprKind::Let {
                mods,
                pat,
                type_params,
                has_param_clause,
                params,
                constraints,
                effects,
                sig,
                value,
            } => ctx.check_let_kind(LetExprInput {
                expr_id: id,
                origin: expr.origin,
                expr_mods: expr.mods,
                mods,
                pat,
                type_params,
                has_param_clause,
                params,
                constraints,
                effects,
                sig,
                value,
            }),
            other => ctx.check_decl_expr(id, expr.origin, other),
        }
    }

    fn check_decl_expr(
        &mut self,
        id: HirExprId,
        origin: HirOrigin,
        kind: HirExprKind,
    ) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        match kind {
            HirExprKind::Error => ExprFacts::new(builtins.error, EffectRow::empty()),
            HirExprKind::Name { name } => ctx.check_name_expr(id, name),
            HirExprKind::Lit { lit } => ctx.check_lit_expr(lit),
            HirExprKind::Template { parts } => ctx.check_template_expr(parts),
            HirExprKind::Sequence { exprs } => ctx.check_sequence_expr(exprs),
            HirExprKind::Tuple { items } => ctx.check_tuple_expr(items),
            HirExprKind::Array { items } => ctx.check_array_expr(items),
            HirExprKind::ArrayTy { dims, item } => ctx.check_array_ty_expr(&dims, item),
            HirExprKind::HandlerTy {
                effect,
                input,
                output,
            } => ctx.check_handler_ty_expr(effect, input, output),
            HirExprKind::Record { items } => ctx.check_record_expr(items),
            HirExprKind::Variant { tag, args } => ctx.check_variant_expr(tag, args),
            HirExprKind::Pi {
                binder,
                binder_ty,
                ret,
                is_effectful,
            } => ctx.check_pi_expr(binder, binder_ty, ret, is_effectful),
            HirExprKind::Lambda {
                params,
                ret_ty,
                body,
            } => ctx.check_lambda_expr(params, ret_ty, body),
            HirExprKind::Call { callee, args } => check_call_expr(ctx, origin, callee, args),
            HirExprKind::Apply { callee, args } => check_apply_expr(ctx, id, origin, callee, args),
            HirExprKind::Index { base, args } => ctx.check_index_expr(origin, base, args),
            HirExprKind::Field { base, access, name } => {
                ctx.check_field_expr(id, origin, base, access, name)
            }
            HirExprKind::RecordUpdate { base, items } => {
                ctx.check_record_update_expr(origin, base, items)
            }
            HirExprKind::TypeTest { base, ty, as_name } => {
                ctx.check_type_test_expr(id, base, ty, as_name)
            }
            HirExprKind::TypeCast { base, ty } => ctx.check_type_cast_expr(base, ty),
            HirExprKind::Prefix { op, expr: inner } => ctx.check_prefix_expr(origin, &op, inner),
            HirExprKind::PartialRange { kind, expr: inner } => {
                ctx.check_partial_range_expr(id, origin, kind, inner)
            }
            HirExprKind::Binary { op, left, right } => {
                ctx.check_binary_expr(id, origin, &op, left, right)
            }
            HirExprKind::Let {
                mods,
                pat,
                type_params,
                has_param_clause,
                params,
                constraints,
                effects,
                sig,
                value,
            } => ctx.check_let_kind(LetExprInput {
                expr_id: id,
                origin,
                expr_mods: ctx.expr(id).mods,
                mods,
                pat,
                type_params,
                has_param_clause,
                params,
                constraints,
                effects,
                sig,
                value,
            }),
            HirExprKind::Import { arg } => ctx.check_import_expr(id, arg),
            HirExprKind::Case { scrutinee, arms } => ctx.check_case_expr(scrutinee, arms),
            HirExprKind::Data { .. }
            | HirExprKind::Effect { .. }
            | HirExprKind::Class { .. }
            | HirExprKind::Instance { .. } => {
                ctx.diag(origin.span, DiagKind::DeclarationUsedAsValue, "");
                ExprFacts::new(builtins.unknown, EffectRow::empty())
            }
            HirExprKind::Perform { expr: inner } => ctx.check_perform_expr(origin, inner),
            HirExprKind::HandlerLit { effect, clauses } => {
                ctx.check_handler_literal_expr(origin, effect, clauses, None)
            }
            HirExprKind::Handle {
                expr: inner,
                handler,
            } => ctx.check_handle_expr(origin, inner, handler),
            HirExprKind::Resume { expr: inner } => ctx.check_resume_expr(origin, inner),
            HirExprKind::Quote { kind } => ctx.check_quote_expr(kind),
            HirExprKind::Splice { kind } => ctx.check_splice_expr(kind),
        }
    }

    fn check_module_stmt(&mut self, id: HirExprId) -> ExprFacts {
        let ctx = self;
        ctx.enter_module_stmt();
        let expr = ctx.expr(id);
        let origin = expr.origin;
        let facts = match expr.kind {
            HirExprKind::Sequence { exprs } => {
                let mut ty = ctx.builtins().unit;
                let mut effects = EffectRow::empty();
                for expr_id in ctx.expr_ids(exprs) {
                    let facts = ctx.check_module_stmt(expr_id);
                    ty = facts.ty;
                    effects.union_with(&facts.effects);
                }
                ExprFacts::new(ty, effects)
            }
            HirExprKind::Instance {
                type_params,
                constraints,
                class,
                members,
            } => {
                let _ =
                    ctx.check_instance_kind(id, origin, type_params, constraints, class, &members);
                ExprFacts::new(ctx.builtins().unit, EffectRow::empty())
            }
            _ => check_expr(ctx, id),
        };
        ctx.set_expr_facts(id, facts.clone());
        ctx.exit_module_stmt();
        facts
    }

    fn check_let_kind(&mut self, input: LetExprInput) -> ExprFacts {
        let ctx = self;
        check_let_expr(ctx, input)
    }

    fn check_instance_kind(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        type_params: SliceRange<HirBinder>,
        constraints: SliceRange<HirConstraint>,
        class: HirExprId,
        members: &SliceRange<HirMemberDef>,
    ) -> ExprFacts {
        self.check_instance_expr(expr_id, origin, type_params, constraints, class, members)
    }

    fn check_name_expr(&mut self, expr_id: HirExprId, name: Ident) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        if let Some(binding) = ctx.binding_id_for_use(name)
            && ctx.is_gated_binding(binding)
        {
            ctx.diag(name.span, DiagKind::TargetGateRejected, "");
            return ExprFacts::new(builtins.unknown, EffectRow::empty());
        }
        if let Some(binding) = ctx.binding_id_for_use(name)
            && let Some(target) = ctx.binding_module_target(binding).cloned()
        {
            ctx.set_expr_module_target(expr_id, target);
        }
        if let Some(binding) = ctx.binding_id_for_use(name)
            && let Some(scheme) = ctx.binding_scheme(binding).cloned()
            && scheme.type_params.is_empty()
        {
            let instantiated = ctx.instantiate_monomorphic_scheme(&scheme);
            if let Some(evidence) = ctx.resolve_obligations_to_evidence(
                ctx.expr(expr_id).origin,
                &instantiated.obligations,
            ) && !evidence.is_empty()
            {
                ctx.set_expr_evidence(expr_id, evidence);
            }
            return ExprFacts::new(instantiated.ty, EffectRow::empty());
        }
        let ty = ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_type(binding))
            .unwrap_or_else(|| ctx.symbol_value_type(name.name));
        ExprFacts::new(ty, EffectRow::empty())
    }

    fn check_lit_expr(&self, lit: HirLitId) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let ty = match ctx.lit_kind(lit) {
            HirLitKind::Int { .. } | HirLitKind::Rune { .. } => builtins.int_,
            HirLitKind::Float { .. } => builtins.float_,
            HirLitKind::String { .. } => builtins.string_,
        };
        ExprFacts::new(ty, EffectRow::empty())
    }

    fn check_template_expr(&mut self, parts: SliceRange<HirTemplatePart>) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let mut effects = EffectRow::empty();
        for part in ctx.template_parts(parts) {
            if let HirTemplatePart::Expr { expr } = part {
                let facts = check_expr(ctx, expr);
                let origin = ctx.expr(expr).origin;
                ctx.type_mismatch(origin, builtins.string_, facts.ty);
                effects.union_with(&facts.effects);
            }
        }
        ExprFacts::new(builtins.string_, effects)
    }

    fn check_sequence_expr(&mut self, exprs: SliceRange<HirExprId>) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let mut effects = EffectRow::empty();
        let mut ty = builtins.unit;
        let exprs = ctx.expr_ids(exprs);
        let len = exprs.len();
        let expected = ctx.expected_ty();
        for (idx, expr) in exprs.into_iter().enumerate() {
            let suppress_expected = expected.is_some() && idx + 1 != len;
            let saved_expected = suppress_expected.then(|| ctx.pop_expected_ty()).flatten();
            let facts = check_expr(ctx, expr);
            if let Some(saved) = saved_expected {
                ctx.push_expected_ty(saved);
            }
            effects.union_with(&facts.effects);
            ty = facts.ty;
        }
        ExprFacts::new(ty, effects)
    }

    fn check_tuple_expr(&mut self, items: SliceRange<HirExprId>) -> ExprFacts {
        let ctx = self;
        let mut effects = EffectRow::empty();
        let item_types = ctx
            .expr_ids(items)
            .into_iter()
            .map(|expr| {
                let facts = check_expr(ctx, expr);
                effects.union_with(&facts.effects);
                facts.ty
            })
            .collect::<Vec<_>>();
        let items = ctx.alloc_ty_list(item_types);
        let ty = ctx.alloc_ty(HirTyKind::Tuple { items });
        ExprFacts::new(ty, effects)
    }

    fn check_pi_expr(
        &mut self,
        binder: Ident,
        binder_ty: HirExprId,
        ret: HirExprId,
        is_effectful: bool,
    ) -> ExprFacts {
        let ctx = self;
        let binder_origin = ctx.expr(binder_ty).origin;
        let param_ty = ctx.lower_type_expr(binder_ty, binder_origin);
        if let Some(binding) = ctx.binding_id_for_decl(binder) {
            ctx.insert_binding_type(binding, param_ty);
        }
        let ret_origin = ctx.expr(ret).origin;
        let ret_ty = ctx.lower_type_expr(ret, ret_origin);
        let params = ctx.alloc_ty_list([param_ty]);
        let ty = ctx.alloc_ty(HirTyKind::Arrow {
            params,
            ret: ret_ty,
            is_effectful,
        });
        ExprFacts::new(ty, EffectRow::empty())
    }

    fn check_lambda_expr(
        &mut self,
        params: SliceRange<HirParam>,
        ret_ty: Option<HirExprId>,
        body: HirExprId,
    ) -> ExprFacts {
        let ctx = self;
        let param_types = ctx.lower_params(params);
        let declared_ret = ret_ty.map(|ret| {
            let origin = ctx.expr(ret).origin;
            ctx.lower_type_expr(ret, origin)
        });
        if let Some(expected) = declared_ret {
            ctx.push_expected_ty(expected);
        }
        let body_facts = check_expr(ctx, body);
        if declared_ret.is_some() {
            let _ = ctx.pop_expected_ty();
        }
        let result_ty = declared_ret.unwrap_or(body_facts.ty);
        if let Some(ret) = ret_ty {
            let origin = ctx.expr(ret).origin;
            ctx.type_mismatch(origin, result_ty, body_facts.ty);
        }
        let params = ctx.alloc_ty_list(param_types.iter().copied());
        let ty = ctx.alloc_ty(HirTyKind::Arrow {
            params,
            ret: result_ty,
            is_effectful: !body_facts.effects.is_pure(),
        });
        ExprFacts::new(ty, EffectRow::empty())
    }

    fn check_index_expr(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        args: SliceRange<HirExprId>,
    ) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let base_facts = check_expr(ctx, base);
        let mut effects = base_facts.effects.clone();
        let arg_count = ctx.check_index_args(origin, args, &mut effects);
        let ty =
            if let HirTyKind::Array { dims, item } = ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
                let dims = ctx.dims(dims);
                if !dims.is_empty() && dims.len() != arg_count {
                    ctx.diag(origin.span, DiagKind::InvalidIndexArity, "");
                }
                item
            } else if let HirTyKind::Seq { item } = ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
                if arg_count != 1 {
                    ctx.diag(origin.span, DiagKind::InvalidIndexArity, "");
                }
                item
            } else {
                ctx.diag(origin.span, DiagKind::InvalidIndexTarget, "");
                builtins.unknown
            };
        ExprFacts::new(ty, effects)
    }

    fn check_index_args(
        &mut self,
        origin: HirOrigin,
        args: SliceRange<HirExprId>,
        effects: &mut EffectRow,
    ) -> usize {
        let ctx = self;
        let builtins = ctx.builtins();
        let index_exprs = ctx.expr_ids(args);
        if index_exprs.is_empty() {
            ctx.diag(origin.span, DiagKind::IndexRequiresArgument, "");
        }
        for index_expr in &index_exprs {
            let facts = check_expr(ctx, *index_expr);
            effects.union_with(&facts.effects);
            let index_origin = ctx.expr(*index_expr).origin;
            ctx.type_mismatch(index_origin, builtins.int_, facts.ty);
        }
        index_exprs.len()
    }

    fn check_field_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        base: HirExprId,
        access: HirAccessKind,
        name: Ident,
    ) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let base_facts = check_expr(ctx, base);
        let effects = base_facts.effects.clone();

        if let HirExprKind::Name { name: effect_name } = ctx.expr(base).kind {
            let effect_name = ctx.resolve_symbol(effect_name.name);
            let op_name = ctx.resolve_symbol(name.name);
            if let Some(effect) = ctx.effect_def(effect_name) {
                if let Some(op) = effect.op(op_name).cloned() {
                    let params = ctx.alloc_ty_list(op.params().iter().copied());
                    let ty = ctx.alloc_ty(HirTyKind::Arrow {
                        params,
                        ret: op.result(),
                        is_effectful: true,
                    });
                    return ExprFacts::new(ty, effects);
                }
            }
        }

        let base_ty = peel_mut_ty(ctx, base_facts.ty);
        let ty = ctx
            .record_like_field_ty(base_ty, ctx.resolve_symbol(name.name))
            .unwrap_or_else(|| match ctx.ty(base_ty).kind {
                HirTyKind::Record { .. } | HirTyKind::Range { .. } => {
                    let field_name = ctx.resolve_symbol(name.name).to_owned();
                    ctx.diag(
                        origin.span,
                        DiagKind::UnknownField,
                        &format!("unknown field `{field_name}`"),
                    );
                    builtins.unknown
                }
                HirTyKind::Module => {
                    if let Some((surface, export)) = module_export_for_expr(ctx, base, name) {
                        if let Some(target) = export.module_target.clone() {
                            ctx.set_expr_module_target(expr_id, target);
                        }
                        let scheme = ctx.scheme_from_export(&surface, &export);
                        if scheme.type_params.is_empty() {
                            let instantiated = ctx.instantiate_monomorphic_scheme(&scheme);
                            if let Some(evidence) = ctx
                                .resolve_obligations_to_evidence(origin, &instantiated.obligations)
                                && !evidence.is_empty()
                            {
                                ctx.set_expr_evidence(expr_id, evidence);
                            }
                        }
                        ctx.set_expr_callable_effects(expr_id, scheme.effects.clone());
                        scheme.ty
                    } else {
                        builtins.any
                    }
                }
                _ => {
                    if matches!(access, HirAccessKind::Direct) {
                        ctx.diag(origin.span, DiagKind::InvalidFieldAccess, "");
                    } else {
                        ctx.diag(origin.span, DiagKind::InvalidOptionalFieldAccess, "");
                    }
                    builtins.unknown
                }
            });
        ExprFacts::new(ty, effects)
    }

    fn check_record_update_expr(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        items: SliceRange<HirRecordItem>,
    ) -> ExprFacts {
        let ctx = self;
        let base_facts = check_expr(ctx, base);
        let mut effects = base_facts.effects.clone();
        let base_ty = peel_mut_ty(ctx, base_facts.ty);
        let mut fields = ctx.record_like_fields(base_ty).unwrap_or_else(|| {
            ctx.diag(origin.span, DiagKind::InvalidRecordUpdateTarget, "");
            BTreeMap::new()
        });
        for record_item in ctx.record_items(items) {
            if record_item.spread {
                let facts = check_expr(ctx, record_item.value);
                effects.union_with(&facts.effects);
                let spread_origin = ctx.expr(record_item.value).origin;
                let spread_ty = peel_mut_ty(ctx, facts.ty);
                let Some(spread_fields) = ctx.record_like_fields(spread_ty) else {
                    ctx.diag(spread_origin.span, DiagKind::InvalidRecordSpreadSource, "");
                    continue;
                };
                for (field_name, field_ty) in spread_fields {
                    let _prev = fields.insert(field_name, field_ty);
                }
                continue;
            }

            let expected = record_item
                .name
                .and_then(|name| fields.get(ctx.resolve_symbol(name.name)).copied())
                .unwrap_or_else(|| ctx.builtins().unknown);
            ctx.push_expected_ty(expected);
            let facts = check_expr(ctx, record_item.value);
            let _ = ctx.pop_expected_ty();
            effects.union_with(&facts.effects);
            if let Some(name) = record_item.name {
                let _prev = fields.insert(ctx.resolve_symbol(name.name).into(), facts.ty);
            }
        }
        let ty = (match ctx.ty(base_ty).kind {
            HirTyKind::Range { bound } => {
                if let Some(found) = fields.get("lowerBound").copied() {
                    ctx.type_mismatch(origin, bound, found);
                }
                if let Some(found) = fields.get("upperBound").copied() {
                    ctx.type_mismatch(origin, bound, found);
                }
                Some(ctx.alloc_ty(HirTyKind::Range { bound }))
            }
            HirTyKind::ClosedRange { bound } => {
                if let Some(found) = fields.get("lowerBound").copied() {
                    ctx.type_mismatch(origin, bound, found);
                }
                if let Some(found) = fields.get("upperBound").copied() {
                    ctx.type_mismatch(origin, bound, found);
                }
                Some(ctx.alloc_ty(HirTyKind::ClosedRange { bound }))
            }
            HirTyKind::PartialRangeFrom { bound } => {
                if let Some(found) = fields.get("lowerBound").copied() {
                    ctx.type_mismatch(origin, bound, found);
                }
                Some(ctx.alloc_ty(HirTyKind::PartialRangeFrom { bound }))
            }
            HirTyKind::PartialRangeUpTo { bound } => {
                if let Some(found) = fields.get("upperBound").copied() {
                    ctx.type_mismatch(origin, bound, found);
                }
                Some(ctx.alloc_ty(HirTyKind::PartialRangeUpTo { bound }))
            }
            HirTyKind::PartialRangeThru { bound } => {
                if let Some(found) = fields.get("upperBound").copied() {
                    ctx.type_mismatch(origin, bound, found);
                }
                Some(ctx.alloc_ty(HirTyKind::PartialRangeThru { bound }))
            }
            _ => None,
        })
        .unwrap_or_else(|| {
            let fields = fields
                .into_iter()
                .map(|(name, ty)| HirTyField::new(ctx.intern(name.as_ref()), ty))
                .collect::<Vec<_>>();
            let fields = ctx.alloc_ty_fields(fields);
            ctx.alloc_ty(HirTyKind::Record { fields })
        });
        ExprFacts::new(ty, effects)
    }

    fn check_type_test_expr(
        &mut self,
        expr_id: HirExprId,
        base: HirExprId,
        ty_expr: HirExprId,
        as_name: Option<Ident>,
    ) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let base_facts = check_expr(ctx, base);
        let origin = ctx.expr(ty_expr).origin;
        let target = ctx.lower_type_expr(ty_expr, origin);
        if ctx.contains_mut_ty(target) {
            ctx.diag(origin.span, DiagKind::MutForbiddenInTypeTestTarget, "");
        }
        ctx.set_type_test_target(expr_id, target);
        if let Some(binding) = as_name.and_then(|ident| ctx.binding_id_for_decl(ident)) {
            ctx.insert_binding_type(binding, base_facts.ty);
        }
        ExprFacts::new(builtins.bool_, base_facts.effects)
    }

    fn check_type_cast_expr(&mut self, base: HirExprId, ty_expr: HirExprId) -> ExprFacts {
        let ctx = self;
        let base_facts = check_expr(ctx, base);
        let origin = ctx.expr(ty_expr).origin;
        let ty = ctx.lower_type_expr(ty_expr, origin);
        if ctx.contains_mut_ty(ty) {
            ctx.diag(origin.span, DiagKind::MutForbiddenInTypeCastTarget, "");
        }
        ExprFacts::new(ty, base_facts.effects)
    }

    fn check_prefix_expr(
        &mut self,
        origin: HirOrigin,
        op: &HirPrefixOp,
        inner: HirExprId,
    ) -> ExprFacts {
        let ctx = self;
        let inner_facts = check_expr(ctx, inner);
        let ty = match op {
            HirPrefixOp::Neg => ctx.numeric_unary_type(origin, inner_facts.ty),
            HirPrefixOp::Not => {
                let bool_ty = ctx.builtins().bool_;
                ctx.type_mismatch(origin, bool_ty, inner_facts.ty);
                bool_ty
            }
            HirPrefixOp::Mut => ctx.alloc_ty(HirTyKind::Mut {
                inner: inner_facts.ty,
            }),
        };
        ExprFacts::new(ty, inner_facts.effects)
    }

    fn check_binary_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        op: &HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    ) -> ExprFacts {
        let ctx = self;
        if matches!(op, HirBinaryOp::Assign) {
            return ctx.check_assign_expr(origin, left, right);
        }
        let builtins = ctx.builtins();
        let left_facts = check_expr(ctx, left);
        let right_facts = check_expr(ctx, right);
        let mut effects = left_facts.effects.clone();
        effects.union_with(&right_facts.effects);
        if matches!(op, HirBinaryOp::ClosedRange | HirBinaryOp::OpenRange) {
            return ctx.check_range_binary_expr(origin, op, left_facts.ty, right_facts.ty, effects);
        }
        if matches!(op, HirBinaryOp::In) {
            return ctx.check_in_binary_expr(
                expr_id,
                origin,
                left_facts.ty,
                right_facts.ty,
                effects,
            );
        }
        if matches!(
            op,
            HirBinaryOp::Pipe
                | HirBinaryOp::Or
                | HirBinaryOp::Xor
                | HirBinaryOp::And
                | HirBinaryOp::Shl
                | HirBinaryOp::Shr
                | HirBinaryOp::UserOp(_)
        ) {
            ctx.diag(
                origin.span,
                DiagKind::BinaryOperatorHasNoExecutableLowering,
                "",
            );
            return ExprFacts::new(builtins.unknown, effects);
        }
        let ty = match op {
            HirBinaryOp::Assign => builtins.unit,
            HirBinaryOp::Arrow | HirBinaryOp::EffectArrow => {
                let left_origin = ctx.expr(left).origin;
                let left_ty = ctx.lower_type_expr(left, left_origin);
                let params = ctx.alloc_ty_list([left_ty]);
                let right_origin = ctx.expr(right).origin;
                let ret = ctx.lower_type_expr(right, right_origin);
                ctx.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret,
                    is_effectful: matches!(op, HirBinaryOp::EffectArrow),
                })
            }
            HirBinaryOp::Add
                if matches!(ctx.ty(left_facts.ty).kind, HirTyKind::Type)
                    || matches!(ctx.ty(right_facts.ty).kind, HirTyKind::Type) =>
            {
                let left_origin = ctx.expr(left).origin;
                let right_origin = ctx.expr(right).origin;
                let left_ty = ctx.lower_type_expr(left, left_origin);
                let right_ty = ctx.lower_type_expr(right, right_origin);
                ctx.alloc_ty(HirTyKind::Sum {
                    left: left_ty,
                    right: right_ty,
                })
            }
            HirBinaryOp::Add
                if matches!(ctx.ty(left_facts.ty).kind, HirTyKind::String)
                    || matches!(ctx.ty(right_facts.ty).kind, HirTyKind::String) =>
            {
                ctx.type_mismatch(origin, builtins.string_, left_facts.ty);
                ctx.type_mismatch(origin, builtins.string_, right_facts.ty);
                builtins.string_
            }
            HirBinaryOp::Add
            | HirBinaryOp::Sub
            | HirBinaryOp::Mul
            | HirBinaryOp::Div
            | HirBinaryOp::Rem => ctx.numeric_binary_type(origin, left_facts.ty, right_facts.ty),
            HirBinaryOp::Eq
            | HirBinaryOp::Ne
            | HirBinaryOp::Lt
            | HirBinaryOp::Gt
            | HirBinaryOp::Le
            | HirBinaryOp::Ge => builtins.bool_,
            HirBinaryOp::Pipe
            | HirBinaryOp::Or
            | HirBinaryOp::Xor
            | HirBinaryOp::And
            | HirBinaryOp::ClosedRange
            | HirBinaryOp::OpenRange
            | HirBinaryOp::In
            | HirBinaryOp::Shl
            | HirBinaryOp::Shr
            | HirBinaryOp::UserOp(_) => builtins.unknown,
        };
        ExprFacts::new(ty, effects)
    }

    fn check_assign_expr(
        &mut self,
        origin: HirOrigin,
        left: HirExprId,
        right: HirExprId,
    ) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();

        let (expected_rhs, mut effects) = match ctx.expr(left).kind {
            HirExprKind::Name { name } => {
                let binding = ctx.binding_id_for_use(name);
                let ty = binding
                    .and_then(|binding| ctx.binding_type(binding))
                    .unwrap_or_else(|| ctx.symbol_value_type(name.name));
                if ctx.is_mut_ty(ty) {
                    (peel_mut_ty(ctx, ty), EffectRow::empty())
                } else {
                    ctx.diag(origin.span, DiagKind::WriteRequiresMutValue, "");
                    (builtins.unknown, EffectRow::empty())
                }
            }
            HirExprKind::Index { base, args } => {
                let base_facts = check_expr(ctx, base);
                let mut effects = base_facts.effects;
                let arg_count = ctx.check_index_args(origin, args, &mut effects);

                let expected = match ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
                    HirTyKind::Array { dims, item } if ctx.is_mut_ty(base_facts.ty) => {
                        let dims = ctx.dims(dims);
                        if !dims.is_empty() && dims.len() != arg_count {
                            ctx.diag(origin.span, DiagKind::InvalidIndexArity, "");
                        }
                        item
                    }
                    HirTyKind::Array { .. } => {
                        ctx.diag(origin.span, DiagKind::WriteRequiresMutArray, "");
                        builtins.unknown
                    }
                    HirTyKind::Seq { item } => {
                        if arg_count != 1 {
                            ctx.diag(origin.span, DiagKind::InvalidIndexArity, "");
                        }
                        if ctx.is_mut_ty(base_facts.ty) {
                            item
                        } else {
                            ctx.diag(origin.span, DiagKind::WriteRequiresMutArray, "");
                            builtins.unknown
                        }
                    }
                    _ => {
                        ctx.diag(origin.span, DiagKind::InvalidIndexTarget, "");
                        builtins.unknown
                    }
                };
                (expected, effects)
            }
            HirExprKind::Field { base, name, .. } => {
                let base_facts = check_expr(ctx, base);
                let effects = base_facts.effects;

                let expected = match ctx.ty(peel_mut_ty(ctx, base_facts.ty)).kind {
                    HirTyKind::Record { fields } if ctx.is_mut_ty(base_facts.ty) => ctx
                        .ty_fields(fields)
                        .into_iter()
                        .find(|field| field.name == name.name)
                        .map_or_else(
                            || {
                                let field_name = ctx.resolve_symbol(name.name).to_owned();
                                ctx.diag(
                                    origin.span,
                                    DiagKind::UnknownField,
                                    &format!("unknown field `{field_name}`"),
                                );
                                builtins.unknown
                            },
                            |field| field.ty,
                        ),
                    HirTyKind::Record { .. } => {
                        ctx.diag(origin.span, DiagKind::WriteRequiresMutRecord, "");
                        builtins.unknown
                    }
                    _ => {
                        ctx.diag(origin.span, DiagKind::InvalidFieldUpdateTarget, "");
                        builtins.unknown
                    }
                };
                (expected, effects)
            }
            _ => {
                ctx.diag(origin.span, DiagKind::UnsupportedAssignmentTarget, "");
                (builtins.unknown, EffectRow::empty())
            }
        };

        ctx.push_expected_ty(expected_rhs);
        let rhs_facts = check_expr(ctx, right);
        let _ = ctx.pop_expected_ty();
        effects.union_with(&rhs_facts.effects);
        ctx.type_mismatch(origin, expected_rhs, rhs_facts.ty);

        ExprFacts::new(builtins.unit, effects)
    }

    fn check_case_expr(&mut self, scrutinee: HirExprId, arms: SliceRange<HirCaseArm>) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let scrutinee_facts = check_expr(ctx, scrutinee);
        let mut effects = scrutinee_facts.effects.clone();
        let mut result_ty = builtins.unknown;
        for arm in ctx.case_arms(arms) {
            bind_pat(ctx, arm.pat, scrutinee_facts.ty);
            if let Some(guard) = arm.guard {
                let guard_facts = check_expr(ctx, guard);
                let origin = ctx.expr(guard).origin;
                ctx.type_mismatch(origin, builtins.bool_, guard_facts.ty);
                effects.union_with(&guard_facts.effects);
            }
            let arm_facts = check_expr(ctx, arm.expr);
            effects.union_with(&arm_facts.effects);
            if result_ty == builtins.unknown {
                result_ty = arm_facts.ty;
            } else {
                let origin = ctx.expr(arm.expr).origin;
                ctx.type_mismatch(origin, result_ty, arm_facts.ty);
            }
        }
        ExprFacts::new(result_ty, effects)
    }

    fn check_quote_expr(&self, _kind: HirQuoteKind) -> ExprFacts {
        let ctx = self;
        ExprFacts::new(ctx.builtins().syntax, EffectRow::empty())
    }

    fn check_splice_expr(&self, _kind: HirSpliceKind) -> ExprFacts {
        let ctx = self;
        ExprFacts::new(ctx.builtins().syntax, EffectRow::empty())
    }

    fn peel_mut_ty(&self, mut ty: HirTyId) -> HirTyId {
        let ctx = self;
        while let HirTyKind::Mut { inner } = ctx.ty(ty).kind {
            ty = inner;
        }
        ty
    }

    fn contains_mut_ty(&self, ty: HirTyId) -> bool {
        let ctx = self;
        match &ctx.ty(ty).kind {
            HirTyKind::Mut { .. } => true,
            HirTyKind::Named { args, .. } => ctx
                .ty_ids(*args)
                .into_iter()
                .any(|ty| ctx.contains_mut_ty(ty)),
            HirTyKind::Pi {
                binder_ty, body, ..
            } => ctx.contains_mut_ty(*binder_ty) || ctx.contains_mut_ty(*body),
            HirTyKind::Arrow { params, ret, .. } => {
                ctx.ty_ids(*params)
                    .into_iter()
                    .any(|ty| ctx.contains_mut_ty(ty))
                    || ctx.contains_mut_ty(*ret)
            }
            HirTyKind::Sum { left, right } => {
                ctx.contains_mut_ty(*left) || ctx.contains_mut_ty(*right)
            }
            HirTyKind::Tuple { items } => ctx
                .ty_ids(*items)
                .into_iter()
                .any(|ty| ctx.contains_mut_ty(ty)),
            HirTyKind::Seq { item }
            | HirTyKind::Range { bound: item }
            | HirTyKind::ClosedRange { bound: item }
            | HirTyKind::PartialRangeFrom { bound: item }
            | HirTyKind::PartialRangeUpTo { bound: item }
            | HirTyKind::PartialRangeThru { bound: item }
            | HirTyKind::Array { item, .. } => ctx.contains_mut_ty(*item),
            HirTyKind::Handler {
                effect,
                input,
                output,
            } => {
                ctx.contains_mut_ty(*effect)
                    || ctx.contains_mut_ty(*input)
                    || ctx.contains_mut_ty(*output)
            }
            HirTyKind::Record { fields } => ctx
                .ty_fields(fields.clone())
                .into_iter()
                .any(|field| ctx.contains_mut_ty(field.ty)),
            HirTyKind::Error
            | HirTyKind::Unknown
            | HirTyKind::Type
            | HirTyKind::Syntax
            | HirTyKind::Any
            | HirTyKind::Empty
            | HirTyKind::Unit
            | HirTyKind::Bool
            | HirTyKind::Nat
            | HirTyKind::Int
            | HirTyKind::Float
            | HirTyKind::String
            | HirTyKind::CString
            | HirTyKind::CPtr
            | HirTyKind::Module
            | HirTyKind::NatLit(_) => false,
        }
    }

    fn is_mut_ty(&self, ty: HirTyId) -> bool {
        let ctx = self;
        matches!(ctx.ty(ty).kind, HirTyKind::Mut { .. })
    }

    fn check_range_binary_expr(
        &mut self,
        origin: HirOrigin,
        op: &HirBinaryOp,
        left: HirTyId,
        right: HirTyId,
        effects: EffectRow,
    ) -> ExprFacts {
        let item_ty = self.range_item_ty(origin, left, right);
        let ty = match op {
            HirBinaryOp::OpenRange => self.alloc_ty(HirTyKind::Range { bound: item_ty }),
            HirBinaryOp::ClosedRange => self.alloc_ty(HirTyKind::ClosedRange { bound: item_ty }),
            _ => self.builtins().unknown,
        };
        ExprFacts::new(ty, effects)
    }

    fn check_partial_range_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        kind: HirPartialRangeKind,
        expr: HirExprId,
    ) -> ExprFacts {
        let facts = check_expr(self, expr);
        let bound = self.normalize_range_bound_ty(facts.ty);
        let rangeable = self.range_obligation(bound, self.known().rangeable);
        let range_bounds = self.range_obligation(bound, self.known().range_bounds);
        let obligations = [rangeable, range_bounds];
        if let Some(evidence) = self.resolve_obligations_to_evidence(origin, &obligations)
            && !evidence.is_empty()
        {
            self.set_expr_evidence(expr_id, evidence);
        }
        let ty = match kind {
            HirPartialRangeKind::From => self.alloc_ty(HirTyKind::PartialRangeFrom { bound }),
            HirPartialRangeKind::UpTo => self.alloc_ty(HirTyKind::PartialRangeUpTo { bound }),
            HirPartialRangeKind::Thru => self.alloc_ty(HirTyKind::PartialRangeThru { bound }),
        };
        ExprFacts::new(ty, facts.effects)
    }

    fn check_in_binary_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        left: HirTyId,
        right: HirTyId,
        effects: EffectRow,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let Some(item_ty) = self.range_item_type(right) else {
            let expected = self.alloc_ty(HirTyKind::Range { bound: left });
            self.type_mismatch(origin, expected, right);
            return ExprFacts::new(builtins.bool_, effects);
        };
        self.type_mismatch(origin, item_ty, left);
        let obligation = self.range_obligation(item_ty, self.known().rangeable);
        if let Some(evidence) = self.resolve_obligations_to_evidence(origin, &[obligation])
            && !evidence.is_empty()
        {
            self.set_expr_evidence(expr_id, evidence);
        }
        ExprFacts::new(builtins.bool_, effects)
    }

    fn range_item_ty(&mut self, origin: HirOrigin, left: HirTyId, right: HirTyId) -> HirTyId {
        let builtins = self.builtins();
        let left = self.normalize_range_bound_ty(left);
        let right = self.normalize_range_bound_ty(right);
        if left == right {
            return left;
        }
        if self.is_integer_like_range_ty(left) && self.is_integer_like_range_ty(right) {
            self.type_mismatch(origin, left, right);
            return left;
        }
        self.type_mismatch(origin, builtins.int_, left);
        self.type_mismatch(origin, builtins.int_, right);
        builtins.int_
    }

    fn normalize_range_bound_ty(&self, ty: HirTyId) -> HirTyId {
        match self.ty(ty).kind {
            HirTyKind::NatLit(_) => self.builtins().nat,
            _ => ty,
        }
    }

    fn is_integer_like_range_ty(&self, ty: HirTyId) -> bool {
        let builtins = self.builtins();
        ty == builtins.int_ || ty == builtins.nat
    }

    fn range_item_type(&self, ty: HirTyId) -> Option<HirTyId> {
        match self.ty(peel_mut_ty(self, ty)).kind {
            HirTyKind::Range { bound }
            | HirTyKind::ClosedRange { bound }
            | HirTyKind::PartialRangeFrom { bound }
            | HirTyKind::PartialRangeUpTo { bound }
            | HirTyKind::PartialRangeThru { bound } => Some(bound),
            _ => None,
        }
    }

    fn record_like_fields(&self, ty: HirTyId) -> Option<BTreeMap<Box<str>, HirTyId>> {
        match self.ty(ty).kind {
            HirTyKind::Record { fields } => Some(
                self.ty_fields(fields)
                    .into_iter()
                    .map(|field| (self.resolve_symbol(field.name).into(), field.ty))
                    .collect(),
            ),
            HirTyKind::Range { bound } | HirTyKind::ClosedRange { bound } => {
                Some(BTreeMap::from([
                    ("lowerBound".into(), bound),
                    ("upperBound".into(), bound),
                ]))
            }
            HirTyKind::PartialRangeFrom { bound } => {
                Some(BTreeMap::from([("lowerBound".into(), bound)]))
            }
            HirTyKind::PartialRangeUpTo { bound } | HirTyKind::PartialRangeThru { bound } => {
                Some(BTreeMap::from([("upperBound".into(), bound)]))
            }
            _ => None,
        }
    }

    fn range_obligation(
        &mut self,
        subject: HirTyId,
        class_name: Symbol,
    ) -> super::schemes::ConstraintObligation {
        let class_ty = self.named_type_for_symbol(class_name);
        super::schemes::ConstraintObligation {
            kind: ConstraintKind::Implements,
            subject,
            value: class_ty,
            class_key: self
                .class_facts_by_name(class_name)
                .map(|facts| facts.key.clone()),
        }
    }

    fn record_like_field_ty(&self, ty: HirTyId, field_name: &str) -> Option<HirTyId> {
        self.record_like_fields(ty)
            .and_then(|fields| fields.get(field_name).copied())
    }

    fn numeric_unary_type(&mut self, origin: HirOrigin, ty: HirTyId) -> HirTyId {
        let ctx = self;
        let builtins = ctx.builtins();
        if ty == builtins.int_ || ty == builtins.float_ {
            ty
        } else {
            ctx.diag(origin.span, DiagKind::NumericOperandRequired, "");
            builtins.unknown
        }
    }

    fn numeric_binary_type(&mut self, origin: HirOrigin, left: HirTyId, right: HirTyId) -> HirTyId {
        let ctx = self;
        let builtins = ctx.builtins();
        if left == builtins.float_ || right == builtins.float_ {
            ctx.type_mismatch(origin, builtins.float_, left);
            ctx.type_mismatch(origin, builtins.float_, right);
            builtins.float_
        } else {
            ctx.type_mismatch(origin, builtins.int_, left);
            ctx.type_mismatch(origin, builtins.int_, right);
            builtins.int_
        }
    }
}
