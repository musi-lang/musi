mod arrays;
mod binary;
mod calls;
mod members;
mod records;
mod variants;

use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{
    HirBinder, HirConstraint, HirExprId, HirExprKind, HirLitId, HirLitKind, HirMatchArm,
    HirMemberDef, HirOrigin, HirParam, HirPrefixOp, HirQuoteKind, HirSpliceKind, HirTemplatePart,
    HirTyId, HirTyKind,
};
use music_names::Ident;

use crate::api::{ComptimeValue, ExprFacts};

use self::calls::{check_apply_expr, check_call_expr};
use super::decls::{LetExprInput, check_let_expr};
use super::pats::bind_pat;
use super::state::Builtins;
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
        ctx.validate_export_mods(origin, id);
        let attrs = expr.mods.attrs;
        if !attrs.is_empty() {
            ctx.validate_expr_attrs(origin, attrs, id);
        }
        if expr.mods.partial && !matches!(expr.kind, HirExprKind::Let { .. }) {
            ctx.diag(origin.span, DiagKind::InvalidPartialModifier, "");
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
            HirExprKind::Let {
                mods,
                pat,
                type_params,
                receiver,
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
                receiver,
                has_param_clause,
                params,
                constraints,
                effects,
                sig,
                value,
            }),
            other => ctx.check_non_let_expr(id, expr.origin, other),
        }
    }

    fn check_non_let_expr(
        &mut self,
        id: HirExprId,
        origin: HirOrigin,
        kind: HirExprKind,
    ) -> ExprFacts {
        match kind {
            HirExprKind::Template { parts } => self.check_template_expr(parts),
            HirExprKind::Sequence { exprs } => self.check_sequence_expr(exprs),
            HirExprKind::Tuple { .. }
            | HirExprKind::Array { .. }
            | HirExprKind::ArrayTy { .. }
            | HirExprKind::AnswerTy { .. }
            | HirExprKind::Record { .. }
            | HirExprKind::Variant { .. }
            | HirExprKind::Pi { .. }
            | HirExprKind::Lambda { .. } => self.check_composite_expr(kind),
            HirExprKind::Call { .. }
            | HirExprKind::Apply { .. }
            | HirExprKind::Index { .. }
            | HirExprKind::Field { .. }
            | HirExprKind::RecordUpdate { .. }
            | HirExprKind::TypeTest { .. }
            | HirExprKind::TypeCast { .. }
            | HirExprKind::Prefix { .. }
            | HirExprKind::PartialRange { .. }
            | HirExprKind::Binary { .. } => self.check_operation_expr(id, origin, kind),
            HirExprKind::Let { .. } => {
                invalid_expr_path(self, "nested let escaped primary dispatcher")
            }
            HirExprKind::Import { arg } => self.check_import_expr(id, arg),
            HirExprKind::Unsafe { body } => self.check_unsafe_expr(body),
            HirExprKind::Pin { value, name, body } => self.check_pin_expr(value, name, body),
            HirExprKind::Match { scrutinee, arms } => self.check_match_expr(scrutinee, arms),
            HirExprKind::Data { .. }
            | HirExprKind::Effect { .. }
            | HirExprKind::Shape { .. }
            | HirExprKind::Given { .. } => self.check_decl_value_expr(origin),
            HirExprKind::Request { .. }
            | HirExprKind::AnswerLit { .. }
            | HirExprKind::Handle { .. }
            | HirExprKind::Resume { .. } => self.check_control_expr(origin, kind),
            HirExprKind::Quote { kind } => self.check_quote_expr(kind),
            HirExprKind::Splice { kind } => self.check_splice_expr(kind),
            HirExprKind::Error | HirExprKind::Name { .. } | HirExprKind::Lit { .. } => {
                invalid_expr_path(self, "simple expr escaped primary dispatcher")
            }
        }
    }

    fn check_unsafe_expr(&mut self, body: HirExprId) -> ExprFacts {
        self.enter_unsafe_block();
        let facts = check_expr(self, body);
        self.exit_unsafe_block();
        facts
    }

    fn check_pin_expr(&mut self, value: HirExprId, name: Ident, body: HirExprId) -> ExprFacts {
        if !self.in_unsafe_block() {
            self.diag(name.span, DiagKind::PinRequiresUnsafeBlock, "");
        }
        let value_facts = check_expr(self, value);
        let target_ty = value_facts.ty;
        let mut effects = value_facts.effects;

        if !self.is_pinnable_ty(target_ty) {
            let target = self.render_ty(target_ty);
            self.diag_with(
                name.span,
                DiagKind::UnsupportedPinTarget,
                DiagContext::new().with("target", target),
            );
        }

        let pin_name = self.intern("Pin");
        let args = self.alloc_ty_list([target_ty]);
        let pin_ty = self.alloc_ty(HirTyKind::Named {
            name: pin_name,
            args,
        });
        if let Some(binding) = self.binding_id_for_decl(name) {
            self.insert_binding_type(binding, pin_ty);
        }

        let body_facts = check_expr(self, body);
        effects.union_with(&body_facts.effects);

        if self.is_pin_ty(body_facts.ty) {
            let name_text = self.resolve_symbol(name.name).to_owned();
            self.diag_with(
                name.span,
                DiagKind::PinnedValueEscapes,
                DiagContext::new().with("name", name_text),
            );
        }

        ExprFacts::new(body_facts.ty, effects)
    }

    fn is_pinnable_ty(&self, ty: HirTyId) -> bool {
        matches!(
            self.ty(ty).kind,
            HirTyKind::String
                | HirTyKind::Seq { .. }
                | HirTyKind::Array { .. }
                | HirTyKind::Range { .. }
                | HirTyKind::Named { .. }
                | HirTyKind::Record { .. }
        )
    }

    fn is_pin_ty(&self, ty: HirTyId) -> bool {
        let HirTyKind::Named { name, .. } = self.ty(ty).kind else {
            return false;
        };
        self.resolve_symbol(name) == "Pin"
    }

    fn check_composite_expr(&mut self, kind: HirExprKind) -> ExprFacts {
        match kind {
            HirExprKind::Tuple { items } => self.check_tuple_expr(items),
            HirExprKind::Array { items } => self.check_array_expr(items),
            HirExprKind::ArrayTy { dims, item } => self.check_array_ty_expr(&dims, item),
            HirExprKind::AnswerTy {
                effect,
                input,
                output,
            } => self.check_handler_ty_expr(effect, input, output),
            HirExprKind::Record { items } => self.check_record_expr(items),
            HirExprKind::Variant { tag, args } => self.check_variant_expr(tag, args),
            HirExprKind::Pi {
                binder,
                binder_ty,
                ret,
                is_effectful,
            } => self.check_pi_expr(binder, binder_ty, ret, is_effectful),
            HirExprKind::Lambda {
                params,
                ret_ty,
                body,
            } => self.check_lambda_expr(params, ret_ty, body),
            _ => invalid_expr_path(self, "composite expr dispatcher mismatch"),
        }
    }

    fn check_operation_expr(
        &mut self,
        id: HirExprId,
        origin: HirOrigin,
        kind: HirExprKind,
    ) -> ExprFacts {
        match kind {
            HirExprKind::Call { callee, args } => check_call_expr(self, origin, callee, args),
            HirExprKind::Apply { callee, args } => check_apply_expr(self, id, origin, callee, args),
            HirExprKind::Index { base, args } => self.check_index_expr(origin, base, args),
            HirExprKind::Field { base, access, name } => {
                self.check_field_expr(id, origin, base, access, name)
            }
            HirExprKind::RecordUpdate { base, items } => {
                self.check_record_update_expr(origin, base, items)
            }
            HirExprKind::TypeTest { base, ty, as_name } => {
                self.check_type_test_expr(id, base, ty, as_name)
            }
            HirExprKind::TypeCast { base, ty } => self.check_type_cast_expr(id, base, ty),
            HirExprKind::Prefix { op, expr } => self.check_prefix_expr(id, origin, &op, expr),
            HirExprKind::PartialRange { kind, expr } => {
                self.check_partial_range_expr(id, origin, kind, expr)
            }
            HirExprKind::Binary { op, left, right } => {
                self.check_binary_expr(id, origin, &op, left, right)
            }
            _ => invalid_expr_path(self, "operation expr dispatcher mismatch"),
        }
    }

    fn check_decl_value_expr(&mut self, origin: HirOrigin) -> ExprFacts {
        let builtins = self.builtins();
        self.diag(origin.span, DiagKind::DeclarationUsedAsValue, "");
        ExprFacts::new(builtins.unknown, EffectRow::empty())
    }

    fn check_control_expr(&mut self, origin: HirOrigin, kind: HirExprKind) -> ExprFacts {
        match kind {
            HirExprKind::Request { expr } => self.check_perform_expr(origin, expr),
            HirExprKind::AnswerLit { effect, clauses } => {
                self.check_handler_literal_expr(origin, effect, clauses, None)
            }
            HirExprKind::Handle { expr, handler } => self.check_handle_expr(origin, expr, handler),
            HirExprKind::Resume { expr } => self.check_resume_expr(origin, expr),
            _ => invalid_expr_path(self, "control expr dispatcher mismatch"),
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
            HirExprKind::Given {
                type_params,
                constraints,
                capability,
                members,
            } => {
                let _ = ctx.check_instance_kind(
                    id,
                    origin,
                    type_params,
                    constraints,
                    capability,
                    &members,
                );
                ExprFacts::new(ctx.builtins().unit, EffectRow::empty())
            }
            _ => check_expr(ctx, id),
        };
        ctx.set_expr_facts(id, facts.clone());
        ctx.exit_module_stmt();
        facts
    }
}

fn invalid_expr_path(ctx: &CheckPass<'_, '_, '_>, detail: &str) -> ExprFacts {
    let _ = detail;
    ExprFacts::new(ctx.builtins().error, EffectRow::empty())
}

impl CheckPass<'_, '_, '_> {
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
        capability: HirExprId,
        members: &SliceRange<HirMemberDef>,
    ) -> ExprFacts {
        self.check_given_expr(
            expr_id,
            origin,
            type_params,
            constraints,
            capability,
            members,
        )
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
            && let Some(target) = ctx.binding_import_record_target(binding).cloned()
        {
            ctx.set_expr_import_record_target(expr_id, target);
        }
        if let Some(binding) = ctx.binding_id_for_use(name)
            && let Some(scheme) = ctx.binding_scheme(binding).cloned()
            && scheme.type_params.is_empty()
        {
            let instantiated = ctx.instantiate_monomorphic_scheme(&scheme);
            if let Some(answers) = ctx
                .resolve_obligations_to_answers(ctx.expr(expr_id).origin, &instantiated.obligations)
                && !answers.is_empty()
            {
                ctx.set_expr_constraint_answers(expr_id, answers);
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
            HirLitKind::Int { .. } => builtins.int_,
            HirLitKind::Rune { .. } => builtins.rune,
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
}

impl CheckPass<'_, '_, '_> {
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

    fn check_type_cast_expr(
        &mut self,
        expr_id: HirExprId,
        base: HirExprId,
        ty_expr: HirExprId,
    ) -> ExprFacts {
        let ctx = self;
        let base_facts = check_expr(ctx, base);
        let origin = ctx.expr(ty_expr).origin;
        let ty = ctx.lower_type_expr(ty_expr, origin);
        if ctx.contains_mut_ty(ty) {
            ctx.diag(origin.span, DiagKind::MutForbiddenInTypeCastTarget, "");
        }
        ctx.set_type_test_target(expr_id, ty);
        ExprFacts::new(ty, base_facts.effects)
    }

    fn check_prefix_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        op: &HirPrefixOp,
        inner: HirExprId,
    ) -> ExprFacts {
        let ctx = self;
        if matches!(op, HirPrefixOp::Known) {
            if let Some(value) = super::const_eval::try_comptime_value(ctx, inner) {
                let ty = if let Some(expanded) = ctx.comptime_expr_expansion(inner, &value) {
                    check_expr(ctx, expanded).ty
                } else {
                    comptime_value_ty(ctx.builtins(), &value)
                };
                ctx.set_expr_comptime_value(inner, value.clone());
                ctx.set_expr_comptime_value(expr_id, value);
                return ExprFacts::new(ty, EffectRow::empty());
            }
            let inner_facts = check_expr(ctx, inner);
            let _ = origin;
            return ExprFacts::new(inner_facts.ty, EffectRow::empty());
        }
        let inner_facts = check_expr(ctx, inner);
        let ty = match op {
            HirPrefixOp::Neg => ctx.numeric_unary_type(origin, inner_facts.ty),
            HirPrefixOp::Not => {
                let bool_ty = ctx.builtins().bool_;
                if ctx.ty_matches(bool_ty, inner_facts.ty) || ctx.is_bits_ty(inner_facts.ty) {
                    inner_facts.ty
                } else {
                    let found = ctx.render_ty(inner_facts.ty);
                    ctx.diag_with(
                        origin.span,
                        DiagKind::UnaryLogicalOperatorDomainMismatch,
                        DiagContext::new()
                            .with("operator", "not")
                            .with("found", found),
                    );
                    ctx.builtins().unknown
                }
            }
            HirPrefixOp::Mut => ctx.alloc_ty(HirTyKind::Mut {
                inner: inner_facts.ty,
            }),
            HirPrefixOp::Known => inner_facts.ty,
            HirPrefixOp::Any | HirPrefixOp::Some => {
                let target = ctx.expr_subject(expr_id);
                ctx.diag_with(
                    origin.span,
                    DiagKind::InvalidTypeExpression,
                    DiagContext::new().with("target", target),
                );
                ctx.builtins().error
            }
        };
        ExprFacts::new(ty, inner_facts.effects)
    }

    fn check_match_expr(
        &mut self,
        scrutinee: HirExprId,
        arms: SliceRange<HirMatchArm>,
    ) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let scrutinee_facts = check_expr(ctx, scrutinee);
        let mut effects = scrutinee_facts.effects.clone();
        let mut result_ty = builtins.unknown;
        for arm in ctx.match_arms(arms) {
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
}

const fn comptime_value_ty(builtins: Builtins, value: &ComptimeValue) -> HirTyId {
    match value {
        ComptimeValue::Int(_) => builtins.int_,
        ComptimeValue::Nat(_) => builtins.nat,
        ComptimeValue::Float(_) => builtins.float_,
        ComptimeValue::String(_) => builtins.string_,
        ComptimeValue::Rune(_) => builtins.rune,
        ComptimeValue::CPtr(_) => builtins.cptr,
        ComptimeValue::Syntax(_) => builtins.syntax,
        ComptimeValue::Unit => builtins.unit,
        ComptimeValue::Seq(_)
        | ComptimeValue::Data(_)
        | ComptimeValue::Closure(_)
        | ComptimeValue::Continuation(_)
        | ComptimeValue::Type(_)
        | ComptimeValue::ImportRecord(_)
        | ComptimeValue::Foreign(_)
        | ComptimeValue::Effect(_)
        | ComptimeValue::Shape(_) => builtins.any,
    }
}

impl CheckPass<'_, '_, '_> {
    fn check_quote_expr(&self, _kind: HirQuoteKind) -> ExprFacts {
        let ctx = self;
        ExprFacts::new(ctx.builtins().syntax, EffectRow::empty())
    }

    fn check_splice_expr(&self, _kind: HirSpliceKind) -> ExprFacts {
        let ctx = self;
        ExprFacts::new(ctx.builtins().syntax, EffectRow::empty())
    }

    fn comptime_expr_expansion(&self, expr: HirExprId, value: &ComptimeValue) -> Option<HirExprId> {
        let ComptimeValue::Syntax(term) = value else {
            return None;
        };
        if !matches!(term.shape(), music_term::SyntaxShape::Expr) {
            return None;
        }
        if self
            .expected_ty()
            .is_some_and(|expected| self.ty_matches(expected, self.builtins().syntax))
        {
            return None;
        }
        match self.expr(expr).kind {
            HirExprKind::Quote {
                kind: HirQuoteKind::Expr { expr, .. },
            } => Some(expr),
            _ => None,
        }
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
            HirTyKind::AnyShape { capability } | HirTyKind::SomeShape { capability } => {
                ctx.contains_mut_ty(*capability)
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
            | HirTyKind::Int8
            | HirTyKind::Int16
            | HirTyKind::Int32
            | HirTyKind::Int64
            | HirTyKind::Nat8
            | HirTyKind::Nat16
            | HirTyKind::Nat32
            | HirTyKind::Nat64
            | HirTyKind::Float
            | HirTyKind::Float32
            | HirTyKind::Float64
            | HirTyKind::String
            | HirTyKind::Rune
            | HirTyKind::CString
            | HirTyKind::CPtr
            | HirTyKind::Bits { .. }
            | HirTyKind::NatLit(_) => false,
        }
    }

    fn is_bits_ty(&self, ty: HirTyId) -> bool {
        match self.ty(ty).kind {
            HirTyKind::Bits { .. } => true,
            HirTyKind::Named { name, args } => {
                name == self.known().bits && self.ty_ids(args).len() == 1
            }
            _ => false,
        }
    }

    pub(super) fn is_mut_ty(&self, ty: HirTyId) -> bool {
        let ctx = self;
        matches!(ctx.ty(ty).kind, HirTyKind::Mut { .. })
    }

    fn numeric_unary_type(&mut self, origin: HirOrigin, ty: HirTyId) -> HirTyId {
        if self.is_numeric_ty(ty) {
            ty
        } else {
            self.diag(origin.span, DiagKind::NumericOperandRequired, "");
            self.builtins().unknown
        }
    }

    pub(super) fn numeric_binary_type(
        &mut self,
        origin: HirOrigin,
        left: HirTyId,
        right: HirTyId,
    ) -> HirTyId {
        if self.is_float_ty(left) || self.is_float_ty(right) {
            let ty = if left == right {
                left
            } else {
                self.builtins().float_
            };
            self.type_mismatch(origin, ty, left);
            self.type_mismatch(origin, ty, right);
            ty
        } else {
            let ty = if left == right {
                left
            } else {
                self.builtins().int_
            };
            self.type_mismatch(origin, ty, left);
            self.type_mismatch(origin, ty, right);
            ty
        }
    }

    fn is_numeric_ty(&self, ty: HirTyId) -> bool {
        self.is_integer_ty(ty) || self.is_float_ty(ty)
    }

    fn is_integer_ty(&self, ty: HirTyId) -> bool {
        matches!(
            self.ty(ty).kind,
            HirTyKind::Int
                | HirTyKind::Nat
                | HirTyKind::Int8
                | HirTyKind::Int16
                | HirTyKind::Int32
                | HirTyKind::Int64
                | HirTyKind::Nat8
                | HirTyKind::Nat16
                | HirTyKind::Nat32
                | HirTyKind::Nat64
                | HirTyKind::NatLit(_)
        )
    }

    fn is_float_ty(&self, ty: HirTyId) -> bool {
        matches!(
            self.ty(ty).kind,
            HirTyKind::Float | HirTyKind::Float32 | HirTyKind::Float64
        )
    }
}
