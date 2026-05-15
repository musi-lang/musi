mod arrays;
mod binary;
mod calls;
mod literals;
mod members;
mod records;
mod variants;

use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{
    HirArg, HirArrayItem, HirExprId, HirExprKind, HirMatchArm, HirOrigin, HirParam, HirPrefixOp,
    HirRecordItem, HirTemplatePart, HirTyId, HirTyKind,
};
use music_names::{Ident, NameBindingId};

use crate::api::{ComptimeValue, ExprFacts};

use self::calls::{check_apply_expr, check_call_expr};
use super::decls::{LetExprInput, check_let_expr};
use super::pats::bind_pat;
use super::state::Builtins;
use super::{CheckPass, DiagKind};

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
            HirExprKind::Error => ExprFacts::new(builtins.error),
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
            HirExprKind::Yield { value } => self.check_yield_expr(origin, value),
            HirExprKind::Defer { cleanup, guard } => self.check_defer_expr(cleanup, guard),
            HirExprKind::Unsafe { body } => self.check_unsafe_expr(body),
            HirExprKind::Pin { value, name, body } => self.check_pin_expr(value, name, body),
            HirExprKind::Match { scrutinee, arms } => self.check_match_expr(scrutinee, arms),
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => self.check_if_expr(condition, then_expr, else_expr),
            HirExprKind::Data { .. } | HirExprKind::Shape { .. } => {
                self.check_decl_value_expr(origin)
            }
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

    fn check_yield_expr(&mut self, origin: HirOrigin, value: HirExprId) -> ExprFacts {
        if self.in_pin_scope() {
            self.diag(origin.span, DiagKind::YieldInsidePinScope, "");
        }
        check_expr(self, value)
    }

    fn check_defer_expr(&mut self, cleanup: HirExprId, guard: Option<HirExprId>) -> ExprFacts {
        let builtins = self.builtins();
        let cleanup_facts = check_expr(self, cleanup);
        let cleanup_origin = self.expr(cleanup).origin;
        self.type_mismatch_for(
            "defer cleanup",
            cleanup_origin,
            builtins.unit,
            cleanup_facts.ty,
        );
        if let Some(guard) = guard {
            let guard_facts = check_expr(self, guard);
            let guard_origin = self.expr(guard).origin;
            self.type_mismatch_for("defer guard", guard_origin, builtins.bool_, guard_facts.ty);
        }
        ExprFacts::new(builtins.unit)
    }

    fn check_pin_expr(&mut self, value: HirExprId, name: Ident, body: HirExprId) -> ExprFacts {
        if !self.in_unsafe_block() {
            self.diag(name.span, DiagKind::PinRequiresUnsafeBlock, "");
        }
        let value_facts = check_expr(self, value);
        let target_ty = value_facts.ty;

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
        let pin_binding = self.binding_id_for_decl(name);
        if let Some(binding) = pin_binding {
            self.insert_binding_type(binding, pin_ty);
        }

        self.enter_pin_scope();
        let body_facts = check_expr(self, body);
        self.exit_pin_scope();

        if let Some(pin_binding) = pin_binding
            && let Some(captured_name) = self.find_pin_capture_in_closure(body, pin_binding)
        {
            let name_text = self.resolve_symbol(name.name).to_owned();
            self.diag_with(
                captured_name.span,
                DiagKind::PinnedValueCapturedByClosure,
                DiagContext::new().with("name", name_text),
            );
        }

        if self.is_pin_ty(body_facts.ty) {
            let name_text = self.resolve_symbol(name.name).to_owned();
            self.diag_with(
                name.span,
                DiagKind::PinnedValueEscapes,
                DiagContext::new().with("name", name_text),
            );
        }

        ExprFacts::new(body_facts.ty)
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

    fn find_pin_capture_in_closure(
        &self,
        expr_id: HirExprId,
        pin_binding: NameBindingId,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr(expr_id, pin_binding, false)
    }

    fn find_pin_capture_in_expr(
        &self,
        expr_id: HirExprId,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr_kind(self.expr(expr_id).kind, pin_binding, in_lambda)
    }

    fn find_pin_capture_in_expr_kind(
        &self,
        kind: HirExprKind,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        match kind {
            HirExprKind::Error
            | HirExprKind::Lit { .. }
            | HirExprKind::ArrayTy { .. }
            | HirExprKind::Pi { .. }
            | HirExprKind::Data { .. }
            | HirExprKind::Shape { .. } => None,
            HirExprKind::Name { name } => {
                self.find_pin_capture_for_name(name, pin_binding, in_lambda)
            }
            HirExprKind::Template { parts } => {
                self.find_pin_capture_in_template(parts, pin_binding, in_lambda)
            }
            HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
                self.find_pin_capture_in_exprs(exprs, pin_binding, in_lambda)
            }
            HirExprKind::Array { items } => {
                self.find_pin_capture_in_array(items, pin_binding, in_lambda)
            }
            HirExprKind::Record { items } => {
                self.find_pin_capture_in_record(items, pin_binding, in_lambda)
            }
            HirExprKind::Variant { args, .. } => {
                self.find_pin_capture_in_variant(args, pin_binding, in_lambda)
            }
            HirExprKind::Lambda { body, .. } => {
                self.find_pin_capture_in_expr(body, pin_binding, true)
            }
            HirExprKind::Call { callee, args } => {
                self.find_pin_capture_for_call(callee, args, pin_binding, in_lambda)
            }
            HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
                self.find_pin_capture_for_apply(callee, args, pin_binding, in_lambda)
            }
            HirExprKind::Field { base, .. }
            | HirExprKind::TypeTest { base, .. }
            | HirExprKind::TypeCast { base, .. } => {
                self.find_pin_capture_in_expr(base, pin_binding, in_lambda)
            }
            HirExprKind::RecordUpdate { base, items } => {
                self.find_pin_capture_for_record_update(base, items, pin_binding, in_lambda)
            }
            HirExprKind::Prefix { expr, .. } | HirExprKind::PartialRange { expr, .. } => {
                self.find_pin_capture_in_expr(expr, pin_binding, in_lambda)
            }
            HirExprKind::Binary { left, right, .. } => {
                self.find_pin_capture_for_binary(left, right, pin_binding, in_lambda)
            }
            HirExprKind::Let {
                receiver,
                params,
                value,
                ..
            } => {
                let callable_scope = receiver.is_some() || !self.params(params).is_empty();
                self.find_pin_capture_in_expr(value, pin_binding, in_lambda || callable_scope)
            }
            HirExprKind::Import { arg }
            | HirExprKind::Yield { value: arg }
            | HirExprKind::Unsafe { body: arg } => {
                self.find_pin_capture_in_expr(arg, pin_binding, in_lambda)
            }
            HirExprKind::Defer { cleanup, guard } => {
                self.find_pin_capture_for_defer(cleanup, guard, pin_binding, in_lambda)
            }
            HirExprKind::Match { scrutinee, arms } => {
                self.find_pin_capture_for_match(scrutinee, arms, pin_binding, in_lambda)
            }
            HirExprKind::If {
                condition,
                then_expr,
                else_expr,
            } => self.find_pin_capture_for_if(
                condition,
                then_expr,
                else_expr,
                pin_binding,
                in_lambda,
            ),
            HirExprKind::Pin { value, body, .. } => self
                .find_pin_capture_in_expr(value, pin_binding, in_lambda)
                .or_else(|| self.find_pin_capture_in_expr(body, pin_binding, in_lambda)),
        }
    }

    fn find_pin_capture_in_template(
        &self,
        parts: SliceRange<HirTemplatePart>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr_iter(
            self.template_parts(parts)
                .into_iter()
                .filter_map(|part| match part {
                    HirTemplatePart::Expr { expr } => Some(expr),
                    HirTemplatePart::Text { .. } => None,
                }),
            pin_binding,
            in_lambda,
        )
    }

    fn find_pin_capture_in_array(
        &self,
        items: SliceRange<HirArrayItem>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr_iter(
            self.array_items(items).into_iter().map(|item| item.expr),
            pin_binding,
            in_lambda,
        )
    }

    fn find_pin_capture_in_record(
        &self,
        items: SliceRange<HirRecordItem>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr_iter(
            self.record_items(items).into_iter().map(|item| item.value),
            pin_binding,
            in_lambda,
        )
    }

    fn find_pin_capture_in_variant(
        &self,
        args: SliceRange<HirArg>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr_iter(
            self.args(args).into_iter().map(|arg| arg.expr),
            pin_binding,
            in_lambda,
        )
    }

    fn find_pin_capture_for_record_update(
        &self,
        base: HirExprId,
        items: SliceRange<HirRecordItem>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_record(items, pin_binding, in_lambda)
            .or_else(|| self.find_pin_capture_in_expr(base, pin_binding, in_lambda))
    }

    fn find_pin_capture_for_binary(
        &self,
        left: HirExprId,
        right: HirExprId,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr(left, pin_binding, in_lambda)
            .or_else(|| self.find_pin_capture_in_expr(right, pin_binding, in_lambda))
    }

    fn find_pin_capture_for_match(
        &self,
        scrutinee: HirExprId,
        arms: SliceRange<HirMatchArm>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr(scrutinee, pin_binding, in_lambda)
            .or_else(|| {
                self.find_pin_capture_in_match_arms(self.match_arms(arms), pin_binding, in_lambda)
            })
    }

    fn find_pin_capture_for_name(
        &self,
        name: Ident,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        if in_lambda
            && self
                .binding_id_for_use(name)
                .is_some_and(|binding| binding == pin_binding)
        {
            Some(name)
        } else {
            None
        }
    }

    fn find_pin_capture_for_call(
        &self,
        callee: HirExprId,
        args: SliceRange<HirArg>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr(callee, pin_binding, in_lambda)
            .or_else(|| {
                self.find_pin_capture_in_expr_iter(
                    self.args(args).into_iter().map(|arg| arg.expr),
                    pin_binding,
                    in_lambda,
                )
            })
    }

    fn find_pin_capture_for_apply(
        &self,
        callee: HirExprId,
        args: SliceRange<HirExprId>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr(callee, pin_binding, in_lambda)
            .or_else(|| self.find_pin_capture_in_exprs(args, pin_binding, in_lambda))
    }

    fn find_pin_capture_for_defer(
        &self,
        cleanup: HirExprId,
        guard: Option<HirExprId>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr(cleanup, pin_binding, in_lambda)
            .or_else(|| {
                guard.and_then(|expr| self.find_pin_capture_in_expr(expr, pin_binding, in_lambda))
            })
    }

    fn find_pin_capture_for_if(
        &self,
        condition: HirExprId,
        then_expr: HirExprId,
        else_expr: HirExprId,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        self.find_pin_capture_in_expr(condition, pin_binding, in_lambda)
            .or_else(|| self.find_pin_capture_in_expr(then_expr, pin_binding, in_lambda))
            .or_else(|| self.find_pin_capture_in_expr(else_expr, pin_binding, in_lambda))
    }

    fn find_pin_capture_in_expr_iter<I>(
        &self,
        exprs: I,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident>
    where
        I: IntoIterator<Item = HirExprId>,
    {
        for expr_id in exprs {
            if let Some(captured_name) =
                self.find_pin_capture_in_expr(expr_id, pin_binding, in_lambda)
            {
                return Some(captured_name);
            }
        }
        None
    }

    fn find_pin_capture_in_match_arms<I>(
        &self,
        arms: I,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident>
    where
        I: IntoIterator<Item = HirMatchArm>,
    {
        for arm in arms {
            if let Some(captured_name) = arm
                .guard
                .and_then(|guard| self.find_pin_capture_in_expr(guard, pin_binding, in_lambda))
            {
                return Some(captured_name);
            }
            if let Some(captured_name) =
                self.find_pin_capture_in_expr(arm.expr, pin_binding, in_lambda)
            {
                return Some(captured_name);
            }
        }
        None
    }

    fn find_pin_capture_in_exprs(
        &self,
        exprs: SliceRange<HirExprId>,
        pin_binding: NameBindingId,
        in_lambda: bool,
    ) -> Option<Ident> {
        for expr_id in self.expr_ids(exprs) {
            if let Some(captured_name) =
                self.find_pin_capture_in_expr(expr_id, pin_binding, in_lambda)
            {
                return Some(captured_name);
            }
        }
        None
    }

    fn check_composite_expr(&mut self, kind: HirExprKind) -> ExprFacts {
        match kind {
            HirExprKind::Tuple { items } => self.check_tuple_expr(items),
            HirExprKind::Array { items } => self.check_array_expr(items),
            HirExprKind::ArrayTy { dims, item } => self.check_array_ty_expr(&dims, item),
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
        ExprFacts::new(builtins.unknown)
    }

    fn check_module_stmt(&mut self, id: HirExprId) -> ExprFacts {
        let ctx = self;
        ctx.enter_module_stmt();
        let expr = ctx.expr(id);
        let facts = match expr.kind {
            HirExprKind::Sequence { exprs } => {
                let mut ty = ctx.builtins().unit;
                for expr_id in ctx.expr_ids(exprs) {
                    let facts = ctx.check_module_stmt(expr_id);
                    ty = facts.ty;
                }
                ExprFacts::new(ty)
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
    ExprFacts::new(ctx.builtins().error)
}

impl CheckPass<'_, '_, '_> {
    fn check_let_kind(&mut self, input: LetExprInput) -> ExprFacts {
        let ctx = self;
        check_let_expr(ctx, input)
    }

    fn check_name_expr(&mut self, expr_id: HirExprId, name: Ident) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        if let Some(binding) = ctx.binding_id_for_use(name)
            && ctx.is_gated_binding(binding)
        {
            ctx.diag(name.span, DiagKind::TargetGateRejected, "");
            return ExprFacts::new(builtins.unknown);
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
            if let Some(evidence) = ctx.resolve_obligations_to_evidence(
                ctx.expr(expr_id).origin,
                &instantiated.obligations,
            ) && !evidence.is_empty()
            {
                ctx.set_expr_constraint_evidence(expr_id, evidence);
            }
            return ExprFacts::new(instantiated.ty);
        }
        let ty = ctx
            .binding_id_for_use(name)
            .and_then(|binding| ctx.binding_type(binding))
            .unwrap_or_else(|| ctx.symbol_value_type(name.name));
        ExprFacts::new(ty)
    }

    fn check_template_expr(&mut self, parts: SliceRange<HirTemplatePart>) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        for part in ctx.template_parts(parts) {
            if let HirTemplatePart::Expr { expr } = part {
                let facts = check_expr(ctx, expr);
                let origin = ctx.expr(expr).origin;
                ctx.type_mismatch(origin, builtins.string_, facts.ty);
            }
        }
        ExprFacts::new(builtins.string_)
    }

    fn check_sequence_expr(&mut self, exprs: SliceRange<HirExprId>) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
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
            ty = facts.ty;
        }
        ExprFacts::new(ty)
    }

    fn check_tuple_expr(&mut self, items: SliceRange<HirExprId>) -> ExprFacts {
        let ctx = self;
        let item_types = ctx
            .expr_ids(items)
            .into_iter()
            .map(|expr| {
                let facts = check_expr(ctx, expr);
                facts.ty
            })
            .collect::<Vec<_>>();
        let items = ctx.alloc_ty_list(item_types);
        let ty = ctx.alloc_ty(HirTyKind::Tuple { items });
        ExprFacts::new(ty)
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
        let params = if ctx.pi_binder_is_empty_tuple_expr(binder_ty) {
            ctx.alloc_ty_list([])
        } else {
            ctx.alloc_ty_list([param_ty])
        };
        let ty = ctx.alloc_ty(HirTyKind::Arrow {
            params,
            ret: ret_ty,
            is_effectful,
        });
        ExprFacts::new(ty)
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
            is_effectful: false,
        });
        ExprFacts::new(ty)
    }

    fn pi_binder_is_empty_tuple_expr(&self, expr: HirExprId) -> bool {
        matches!(
            self.expr(expr).kind,
            HirExprKind::Tuple { items } | HirExprKind::Sequence { exprs: items }
                if self.expr_ids(items).is_empty()
        )
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
        ExprFacts::new(builtins.bool_)
    }

    fn check_type_cast_expr(
        &mut self,
        expr_id: HirExprId,
        base: HirExprId,
        ty_expr: HirExprId,
    ) -> ExprFacts {
        let ctx = self;
        let _ = check_expr(ctx, base);
        let origin = ctx.expr(ty_expr).origin;
        let ty = ctx.lower_type_expr(ty_expr, origin);
        if ctx.contains_mut_ty(ty) {
            ctx.diag(origin.span, DiagKind::MutForbiddenInTypeCastTarget, "");
        }
        ctx.set_type_test_target(expr_id, ty);
        ExprFacts::new(ty)
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
                return ExprFacts::new(ty);
            }
            let inner_facts = check_expr(ctx, inner);
            ctx.diag(origin.span, DiagKind::RuntimeValueInComptimeContext, "");
            return ExprFacts::new(inner_facts.ty);
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
        };
        ExprFacts::new(ty)
    }

    fn check_match_expr(
        &mut self,
        scrutinee: HirExprId,
        arms: SliceRange<HirMatchArm>,
    ) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let scrutinee_facts = check_expr(ctx, scrutinee);
        let mut result_ty = builtins.unknown;
        for arm in ctx.match_arms(arms) {
            bind_pat(ctx, arm.pat, scrutinee_facts.ty);
            if let Some(guard) = arm.guard {
                let guard_facts = check_expr(ctx, guard);
                let origin = ctx.expr(guard).origin;
                ctx.type_mismatch(origin, builtins.bool_, guard_facts.ty);
            }
            let arm_facts = check_expr(ctx, arm.expr);
            if result_ty == builtins.unknown {
                result_ty = arm_facts.ty;
            } else {
                let origin = ctx.expr(arm.expr).origin;
                ctx.type_mismatch(origin, result_ty, arm_facts.ty);
            }
        }
        ExprFacts::new(result_ty)
    }

    fn check_if_expr(
        &mut self,
        condition: HirExprId,
        then_expr: HirExprId,
        else_expr: HirExprId,
    ) -> ExprFacts {
        let ctx = self;
        let builtins = ctx.builtins();
        let condition_facts = check_expr(ctx, condition);
        let condition_origin = ctx.expr(condition).origin;
        ctx.type_mismatch(condition_origin, builtins.bool_, condition_facts.ty);
        let then_facts = check_expr(ctx, then_expr);
        let else_facts = check_expr(ctx, else_expr);
        let else_origin = ctx.expr(else_expr).origin;
        ctx.type_mismatch(else_origin, then_facts.ty, else_facts.ty);
        ExprFacts::new(then_facts.ty)
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
        | ComptimeValue::Type(_)
        | ComptimeValue::ImportRecord(_)
        | ComptimeValue::Foreign(_)
        | ComptimeValue::Shape(_) => builtins.any,
    }
}

impl CheckPass<'_, '_, '_> {
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
        let _ = expr;
        None
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
