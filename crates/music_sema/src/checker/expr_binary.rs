use music_arena::SliceRange;
use music_hir::{
    HirBinaryOp, HirExprId, HirExprKind, HirOrigin, HirPartialRangeKind, HirTyId, HirTyKind,
};
use music_names::{Ident, Symbol};

use crate::api::{ConstraintKind, ExprFacts};
use crate::effects::EffectRow;

use super::exprs::peel_mut_ty;
use super::{CheckPass, DiagKind};

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
        let left_facts = super::exprs::check_expr(self, left);
        let right_facts = super::exprs::check_expr(self, right);
        let mut effects = left_facts.effects.clone();
        effects.union_with(&right_facts.effects);
        if matches!(op, HirBinaryOp::ClosedRange | HirBinaryOp::OpenRange) {
            return self.check_range_binary_expr(
                origin,
                op,
                left_facts.ty,
                right_facts.ty,
                effects,
            );
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
            HirBinaryOp::Or
                | HirBinaryOp::Xor
                | HirBinaryOp::And
                | HirBinaryOp::Shl
                | HirBinaryOp::Shr
                | HirBinaryOp::UserOp(_)
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
        kind: HirPartialRangeKind,
        expr: HirExprId,
    ) -> ExprFacts {
        let facts = super::exprs::check_expr(self, expr);
        let bound = self.normalize_range_bound_ty(facts.ty);
        let obligations = [
            self.range_obligation(bound, self.known().rangeable),
            self.range_obligation(bound, self.known().range_bounds),
        ];
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

    fn check_assign_expr(
        &mut self,
        origin: HirOrigin,
        left: HirExprId,
        right: HirExprId,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let (expected_rhs, mut effects) = self.assignment_contract(origin, left);
        self.push_expected_ty(expected_rhs);
        let rhs_facts = super::exprs::check_expr(self, right);
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
                self.diag(origin.span, DiagKind::UnsupportedAssignmentTarget, "");
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
        let base_facts = super::exprs::check_expr(self, base);
        let mut effects = base_facts.effects;
        let arg_count = self.check_index_args(origin, args, &mut effects);
        let expected = match self.ty(peel_mut_ty(self, base_facts.ty)).kind {
            HirTyKind::Array { dims, item } if self.is_mut_ty(base_facts.ty) => {
                let dims = self.dims(dims);
                if !dims.is_empty() && dims.len() != arg_count {
                    self.diag(
                        origin.span,
                        DiagKind::InvalidIndexArgCount,
                        "index arg count does not match array dimensions",
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
                    self.diag(
                        origin.span,
                        DiagKind::InvalidIndexArgCount,
                        "index expression requires exactly one argument here",
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
                self.diag(origin.span, DiagKind::InvalidIndexTarget, "");
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
        let base_facts = super::exprs::check_expr(self, base);
        let effects = base_facts.effects;
        let expected = match self.ty(peel_mut_ty(self, base_facts.ty)).kind {
            HirTyKind::Record { fields } if self.is_mut_ty(base_facts.ty) => self
                .ty_fields(fields)
                .into_iter()
                .find(|field| field.name == name.name)
                .map_or_else(
                    || {
                        let field_name = self.resolve_symbol(name.name).to_owned();
                        self.diag_message(
                            origin.span,
                            DiagKind::UnknownField,
                            format!("unknown field `{field_name}`"),
                            format!("unknown field `{field_name}`"),
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
                self.diag(
                    origin.span,
                    DiagKind::InvalidFieldTarget,
                    "field update target must support field assignment",
                );
                builtins.unknown
            }
        };
        (expected, effects)
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
            HirBinaryOp::Eq
            | HirBinaryOp::TypeEq
            | HirBinaryOp::Ne
            | HirBinaryOp::Lt
            | HirBinaryOp::Gt
            | HirBinaryOp::Le
            | HirBinaryOp::Ge => builtins.bool_,
            HirBinaryOp::Assign
            | HirBinaryOp::Or
            | HirBinaryOp::Xor
            | HirBinaryOp::And
            | HirBinaryOp::ClosedRange
            | HirBinaryOp::OpenRange
            | HirBinaryOp::In
            | HirBinaryOp::Shl
            | HirBinaryOp::Shr
            | HirBinaryOp::UserOp(_) => builtins.unknown,
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
            HirTyKind::Range { bound }
            | HirTyKind::ClosedRange { bound }
            | HirTyKind::PartialRangeFrom { bound }
            | HirTyKind::PartialRangeUpTo { bound }
            | HirTyKind::PartialRangeThru { bound } => Some(bound),
            _ => None,
        }
    }

    pub(super) fn range_obligation(
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
}
