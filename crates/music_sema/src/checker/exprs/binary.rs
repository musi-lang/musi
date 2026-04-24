use music_arena::SliceRange;
use music_base::diag::DiagContext;
use music_hir::{
    HirBinaryOp, HirExprId, HirExprKind, HirOrigin, HirPartialRangeKind, HirTyId, HirTyKind,
};
use music_names::{Ident, Symbol};

use crate::api::{ConstraintKind, ExprFacts};
use crate::effects::EffectRow;

use super::super::{CheckPass, DiagKind};
use super::peel_mut_ty;

impl CheckPass<'_, '_, '_> {
    pub(super) fn check_binary_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        op: &HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
    ) -> ExprFacts {
        if matches!(op, HirBinaryOp::Assign) {
            return self.check_assign_expr(origin, left, right);
        }
        let builtins = self.builtins();
        let left_facts = super::check_expr(self, left);
        let right_facts = super::check_expr(self, right);
        let mut effects = left_facts.effects.clone();
        effects.union_with(&right_facts.effects);
        if matches!(op, HirBinaryOp::Range { .. }) {
            return self.check_range_binary_expr(origin, left_facts.ty, right_facts.ty, effects);
        }
        if matches!(op, HirBinaryOp::In) {
            return self.check_in_binary_expr(
                expr_id,
                origin,
                left_facts.ty,
                right_facts.ty,
                effects,
            );
        }
        if matches!(
            op,
            HirBinaryOp::Shl | HirBinaryOp::Shr | HirBinaryOp::UserOp(_)
        ) {
            self.diag(
                origin.span,
                DiagKind::BinaryOperatorHasNoExecutableLowering,
                "",
            );
            return ExprFacts::new(builtins.unknown, effects);
        }
        ExprFacts::new(
            self.binary_result_ty(origin, op, left, right, left_facts.ty, right_facts.ty),
            effects,
        )
    }

    pub(super) fn check_partial_range_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        _kind: HirPartialRangeKind,
        expr: HirExprId,
    ) -> ExprFacts {
        let facts = super::check_expr(self, expr);
        let bound = self.normalize_range_bound_ty(facts.ty);
        let obligations = [
            self.range_obligation(bound, self.known().rangeable),
            self.range_obligation(bound, self.known().range_bounds),
        ];
        if let Some(answers) = self.resolve_obligations_to_answers(origin, &obligations)
            && !answers.is_empty()
        {
            self.set_expr_constraint_answers(expr_id, answers);
        }
        let ty = self.alloc_ty(HirTyKind::Range { bound });
        ExprFacts::new(ty, facts.effects)
    }

    fn check_assign_expr(
        &mut self,
        origin: HirOrigin,
        left: HirExprId,
        right: HirExprId,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let (expected_rhs, mut effects) = self.assignment_contract(origin, left);
        self.push_expected_ty(expected_rhs);
        let rhs_facts = super::check_expr(self, right);
        let _ = self.pop_expected_ty();
        effects.union_with(&rhs_facts.effects);
        self.type_mismatch(origin, expected_rhs, rhs_facts.ty);
        ExprFacts::new(builtins.unit, effects)
    }

    fn assignment_contract(&mut self, origin: HirOrigin, left: HirExprId) -> (HirTyId, EffectRow) {
        let builtins = self.builtins();
        match self.expr(left).kind {
            HirExprKind::Name { name } => {
                let binding = self.binding_id_for_use(name);
                let ty = binding
                    .and_then(|binding| self.binding_type(binding))
                    .unwrap_or_else(|| self.symbol_value_type(name.name));
                if self.is_mut_ty(ty) {
                    (peel_mut_ty(self, ty), EffectRow::empty())
                } else {
                    self.diag(
                        origin.span,
                        DiagKind::WriteTargetRequiresMut,
                        "assignment target must be mutable",
                    );
                    (builtins.unknown, EffectRow::empty())
                }
            }
            HirExprKind::Index { base, args } => self.assignment_index_contract(origin, base, args),
            HirExprKind::Field { base, name, .. } => {
                self.assignment_field_contract(origin, base, name)
            }
            _ => {
                let target = self.expr_subject(left);
                self.diag_with(
                    origin.span,
                    DiagKind::UnsupportedAssignmentTarget,
                    DiagContext::new().with("target", target),
                );
                (builtins.unknown, EffectRow::empty())
            }
        }
    }

    fn assignment_index_contract(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        args: SliceRange<HirExprId>,
    ) -> (HirTyId, EffectRow) {
        let builtins = self.builtins();
        let base_facts = super::check_expr(self, base);
        let mut effects = base_facts.effects;
        let arg_count = self.check_index_args(origin, args, &mut effects);
        let expected = match self.ty(peel_mut_ty(self, base_facts.ty)).kind {
            HirTyKind::Array { dims, item } if self.is_mut_ty(base_facts.ty) => {
                let dims = self.dims(dims);
                if !dims.is_empty() && dims.len() != arg_count {
                    self.diag_with(
                        origin.span,
                        DiagKind::InvalidIndexArgCount,
                        DiagContext::new()
                            .with("expected", dims.len())
                            .with("found", arg_count),
                    );
                }
                item
            }
            HirTyKind::Array { .. } => {
                self.diag(
                    origin.span,
                    DiagKind::WriteTargetRequiresMut,
                    "indexed write target must be mutable array",
                );
                builtins.unknown
            }
            HirTyKind::Seq { item } => {
                if arg_count != 1 {
                    self.diag_with(
                        origin.span,
                        DiagKind::InvalidIndexArgCount,
                        DiagContext::new()
                            .with("expected", 1)
                            .with("found", arg_count),
                    );
                }
                if self.is_mut_ty(base_facts.ty) {
                    item
                } else {
                    self.diag(
                        origin.span,
                        DiagKind::WriteTargetRequiresMut,
                        "indexed write target must be mutable array",
                    );
                    builtins.unknown
                }
            }
            _ => {
                let target = self.render_ty(base_facts.ty);
                self.diag_with(
                    origin.span,
                    DiagKind::InvalidIndexTarget,
                    DiagContext::new().with("target", target),
                );
                builtins.unknown
            }
        };
        (expected, effects)
    }

    fn assignment_field_contract(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        name: Ident,
    ) -> (HirTyId, EffectRow) {
        let builtins = self.builtins();
        let base_facts = super::check_expr(self, base);
        let effects = base_facts.effects;
        let expected = match self.ty(peel_mut_ty(self, base_facts.ty)).kind {
            HirTyKind::Record { fields } if self.is_mut_ty(base_facts.ty) => self
                .ty_fields(fields)
                .into_iter()
                .find(|field| field.name == name.name)
                .map_or_else(
                    || {
                        let field_name = self.resolve_symbol(name.name).to_owned();
                        self.diag_with(
                            origin.span,
                            DiagKind::UnknownField,
                            DiagContext::new().with("field", field_name),
                        );
                        builtins.unknown
                    },
                    |field| field.ty,
                ),
            HirTyKind::Record { .. } => {
                self.diag(
                    origin.span,
                    DiagKind::WriteTargetRequiresMut,
                    "field write target must be mutable record",
                );
                builtins.unknown
            }
            _ => {
                let target = self.render_ty(base_facts.ty);
                self.diag_with(
                    origin.span,
                    DiagKind::InvalidFieldTarget,
                    DiagContext::new().with("target", target),
                );
                builtins.unknown
            }
        };
        (expected, effects)
    }

    fn check_range_binary_expr(
        &mut self,
        origin: HirOrigin,
        left: HirTyId,
        right: HirTyId,
        effects: EffectRow,
    ) -> ExprFacts {
        let item_ty = self.range_item_ty(origin, left, right);
        let ty = self.alloc_ty(HirTyKind::Range { bound: item_ty });
        ExprFacts::new(ty, effects)
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
        if let Some(answers) = self.resolve_obligations_to_answers(origin, &[obligation])
            && !answers.is_empty()
        {
            self.set_expr_constraint_answers(expr_id, answers);
        }
        ExprFacts::new(builtins.bool_, effects)
    }

    fn binary_result_ty(
        &mut self,
        origin: HirOrigin,
        op: &HirBinaryOp,
        left: HirExprId,
        right: HirExprId,
        left_ty: HirTyId,
        right_ty: HirTyId,
    ) -> HirTyId {
        let builtins = self.builtins();
        match op {
            HirBinaryOp::Arrow | HirBinaryOp::EffectArrow => {
                let left_origin = self.expr(left).origin;
                let left_ty = self.lower_type_expr(left, left_origin);
                let params = self.alloc_ty_list([left_ty]);
                let right_origin = self.expr(right).origin;
                let ret = self.lower_type_expr(right, right_origin);
                self.alloc_ty(HirTyKind::Arrow {
                    params,
                    ret,
                    is_effectful: matches!(op, HirBinaryOp::EffectArrow),
                })
            }
            HirBinaryOp::Add
                if matches!(self.ty(left_ty).kind, HirTyKind::Type)
                    || matches!(self.ty(right_ty).kind, HirTyKind::Type) =>
            {
                let left_origin = self.expr(left).origin;
                let right_origin = self.expr(right).origin;
                let left_ty = self.lower_type_expr(left, left_origin);
                let right_ty = self.lower_type_expr(right, right_origin);
                self.alloc_ty(HirTyKind::Sum {
                    left: left_ty,
                    right: right_ty,
                })
            }
            HirBinaryOp::Add
                if matches!(self.ty(left_ty).kind, HirTyKind::String)
                    || matches!(self.ty(right_ty).kind, HirTyKind::String) =>
            {
                self.type_mismatch(origin, builtins.string_, left_ty);
                self.type_mismatch(origin, builtins.string_, right_ty);
                builtins.string_
            }
            HirBinaryOp::Add
            | HirBinaryOp::Sub
            | HirBinaryOp::Mul
            | HirBinaryOp::Div
            | HirBinaryOp::Rem => self.numeric_binary_type(origin, left_ty, right_ty),
            HirBinaryOp::Or | HirBinaryOp::Xor | HirBinaryOp::And => {
                self.logical_binary_type(origin, op, left_ty, right_ty)
            }
            HirBinaryOp::Eq
            | HirBinaryOp::TypeEq
            | HirBinaryOp::Ne
            | HirBinaryOp::Lt
            | HirBinaryOp::Gt
            | HirBinaryOp::Le
            | HirBinaryOp::Ge => builtins.bool_,
            HirBinaryOp::Assign
            | HirBinaryOp::Range { .. }
            | HirBinaryOp::In
            | HirBinaryOp::Shl
            | HirBinaryOp::Shr
            | HirBinaryOp::UserOp(_) => builtins.unknown,
        }
    }

    fn logical_binary_type(
        &mut self,
        origin: HirOrigin,
        op: &HirBinaryOp,
        left_ty: HirTyId,
        right_ty: HirTyId,
    ) -> HirTyId {
        let builtins = self.builtins();
        if self.ty_matches(builtins.bool_, left_ty) && self.ty_matches(builtins.bool_, right_ty) {
            return builtins.bool_;
        }
        if self.matching_bits_tys(left_ty, right_ty) {
            return left_ty;
        }
        let left = self.render_ty(left_ty);
        let right = self.render_ty(right_ty);
        self.diag_with(
            origin.span,
            DiagKind::LogicalOperatorDomainMismatch,
            DiagContext::new()
                .with("operator", logical_operator_name(op))
                .with("left", left)
                .with("right", right),
        );
        builtins.unknown
    }

    fn matching_bits_tys(&self, left_ty: HirTyId, right_ty: HirTyId) -> bool {
        match (self.ty(left_ty).kind, self.ty(right_ty).kind) {
            (HirTyKind::Bits { width: left }, HirTyKind::Bits { width: right }) => left == right,
            (
                HirTyKind::Named {
                    name: left_name,
                    args: left_args,
                },
                HirTyKind::Named {
                    name: right_name,
                    args: right_args,
                },
            ) if left_name == self.known().bits && right_name == self.known().bits => {
                self.ty_ids(left_args).len() == 1
                    && self.ty_ids(right_args).len() == 1
                    && self.ty_matches(left_ty, right_ty)
            }
            _ => false,
        }
    }

    fn normalize_range_bound_ty(&self, ty: HirTyId) -> HirTyId {
        match self.ty(ty).kind {
            HirTyKind::NatLit(_) => self.builtins().nat,
            _ => ty,
        }
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

    fn is_integer_like_range_ty(&self, ty: HirTyId) -> bool {
        let builtins = self.builtins();
        ty == builtins.int_ || ty == builtins.nat
    }

    pub(super) fn range_item_type(&self, ty: HirTyId) -> Option<HirTyId> {
        match self.ty(peel_mut_ty(self, ty)).kind {
            HirTyKind::Range { bound } => Some(bound),
            _ => None,
        }
    }

    pub(super) fn range_obligation(
        &mut self,
        subject: HirTyId,
        shape_name: Symbol,
    ) -> super::super::schemes::ConstraintObligation {
        let shape_ty = self.named_type_for_symbol(shape_name);
        super::super::schemes::ConstraintObligation {
            kind: ConstraintKind::Implements,
            subject,
            value: shape_ty,
            shape_key: self
                .shape_facts_by_name(shape_name)
                .map(|facts| facts.key.clone()),
        }
    }
}

const fn logical_operator_name(op: &HirBinaryOp) -> &'static str {
    match op {
        HirBinaryOp::Or => "or",
        HirBinaryOp::Xor => "xor",
        HirBinaryOp::And => "and",
        _ => "<operator>",
    }
}
