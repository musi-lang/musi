use music_arena::SliceRange;
use music_base::Span;
use music_hir::{HirArrayItem, HirDim, HirExprId, HirOrigin, HirTyId, HirTyKind};

use crate::api::{ConstraintKind, ExprFacts};
use crate::effects::EffectRow;

use super::exprs::peel_mut_ty;
use super::{CheckPass, DiagKind};

impl CheckPass<'_, '_, '_> {
    pub(super) fn check_index_expr(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        args: SliceRange<HirExprId>,
    ) -> ExprFacts {
        let builtins = self.builtins();
        let base_facts = super::exprs::check_expr(self, base);
        let mut effects = base_facts.effects.clone();
        let arg_count = self.check_index_args(origin, args, &mut effects);
        let ty = if let HirTyKind::Array { dims, item } =
            self.ty(peel_mut_ty(self, base_facts.ty)).kind
        {
            let dims = self.dims(dims);
            if !dims.is_empty() && dims.len() != arg_count {
                self.diag(
                    origin.span,
                    DiagKind::InvalidIndexArgCount,
                    "index arg count does not match array dimensions",
                );
            }
            item
        } else if let HirTyKind::Seq { item } = self.ty(peel_mut_ty(self, base_facts.ty)).kind {
            if arg_count != 1 {
                self.diag(
                    origin.span,
                    DiagKind::InvalidIndexArgCount,
                    "index expression requires exactly one argument here",
                );
            }
            item
        } else {
            self.diag(origin.span, DiagKind::InvalidIndexTarget, "");
            builtins.unknown
        };
        ExprFacts::new(ty, effects)
    }

    pub(super) fn check_array_expr(&mut self, items: SliceRange<HirArrayItem>) -> ExprFacts {
        let builtins = self.builtins();
        let mut effects = EffectRow::empty();
        let (expected_dims, expected_item, expected_seq) = self.expected_array_contract();
        let mut item_ty = expected_item.unwrap_or(builtins.unknown);

        let mut has_runtime_spread = false;
        let mut known_len: u32 = 0;
        let items_vec = self.array_items(items);
        for array_item in &items_vec {
            if !array_item.spread {
                self.check_array_direct_item(
                    array_item,
                    &mut item_ty,
                    &mut effects,
                    &mut known_len,
                );
                continue;
            }
            self.check_array_spread_item(
                array_item,
                &mut item_ty,
                &mut effects,
                &mut has_runtime_spread,
                &mut known_len,
            );
        }

        self.check_array_literal_expected_len(
            expected_dims.as_ref(),
            &items_vec,
            has_runtime_spread,
            known_len,
        );

        let ty = if expected_seq || expected_dims.is_none() {
            self.alloc_ty(HirTyKind::Seq { item: item_ty })
        } else {
            let dims = expected_dims.unwrap_or_else(|| self.alloc_dims([HirDim::Unknown]));
            self.alloc_ty(HirTyKind::Array {
                dims,
                item: item_ty,
            })
        };
        ExprFacts::new(ty, effects)
    }

    fn check_array_direct_item(
        &mut self,
        array_item: &HirArrayItem,
        item_ty: &mut HirTyId,
        effects: &mut EffectRow,
        known_len: &mut u32,
    ) {
        let builtins = self.builtins();
        self.push_expected_ty(*item_ty);
        let facts = super::exprs::check_expr(self, array_item.expr);
        let _ = self.pop_expected_ty();
        effects.union_with(&facts.effects);
        if *item_ty == builtins.unknown {
            *item_ty = facts.ty;
        } else {
            let origin = self.expr(array_item.expr).origin;
            self.type_mismatch(origin, *item_ty, facts.ty);
        }
        *known_len = known_len.saturating_add(1);
    }

    fn check_array_spread_item(
        &mut self,
        array_item: &HirArrayItem,
        item_ty: &mut HirTyId,
        effects: &mut EffectRow,
        has_runtime_spread: &mut bool,
        known_len: &mut u32,
    ) {
        let spread_facts = super::exprs::check_expr(self, array_item.expr);
        effects.union_with(&spread_facts.effects);
        let spread_origin = self.expr(array_item.expr).origin;
        let spread_ty = peel_mut_ty(self, spread_facts.ty);
        match self.ty(spread_ty).kind {
            HirTyKind::Tuple { items } => {
                let item_tys = self.ty_ids(items);
                for found in item_tys {
                    self.merge_array_item_ty(spread_origin, item_ty, found);
                    *known_len = known_len.saturating_add(1);
                }
            }
            HirTyKind::Array { dims, item } => {
                self.check_array_spread_array(
                    spread_origin,
                    dims,
                    item,
                    item_ty,
                    has_runtime_spread,
                    known_len,
                );
            }
            HirTyKind::Seq { item }
            | HirTyKind::Range { bound: item }
            | HirTyKind::ClosedRange { bound: item }
            | HirTyKind::PartialRangeFrom { bound: item }
            | HirTyKind::PartialRangeUpTo { bound: item }
            | HirTyKind::PartialRangeThru { bound: item } => {
                *has_runtime_spread = true;
                self.merge_array_item_ty(spread_origin, item_ty, item);
                if matches!(
                    self.ty(spread_ty).kind,
                    HirTyKind::Range { .. }
                        | HirTyKind::ClosedRange { .. }
                        | HirTyKind::PartialRangeFrom { .. }
                        | HirTyKind::PartialRangeUpTo { .. }
                        | HirTyKind::PartialRangeThru { .. }
                ) {
                    self.resolve_rangeable_evidence(array_item.expr, spread_origin, item);
                }
            }
            _ => self.diag(
                spread_origin.span,
                DiagKind::InvalidSpreadSource,
                "array spread source must be array, tuple, or range-like value",
            ),
        }
    }

    fn check_array_spread_array(
        &mut self,
        spread_origin: HirOrigin,
        dims: SliceRange<HirDim>,
        item: HirTyId,
        item_ty: &mut HirTyId,
        has_runtime_spread: &mut bool,
        known_len: &mut u32,
    ) {
        let dims_vec = self.dims(dims);
        if dims_vec.is_empty() {
            *has_runtime_spread = true;
            self.merge_array_item_ty(spread_origin, item_ty, item);
            return;
        }
        if dims_vec.len() != 1 {
            self.diag(
                spread_origin.span,
                DiagKind::ArraySpreadRequiresOneDimensionalArray,
                "",
            );
            return;
        }
        match dims_vec[0] {
            HirDim::Int(len) => {
                self.merge_array_item_ty(spread_origin, item_ty, item);
                *known_len = known_len.saturating_add(len);
            }
            HirDim::Unknown | HirDim::Name(_) => {
                *has_runtime_spread = true;
                self.merge_array_item_ty(spread_origin, item_ty, item);
            }
        }
    }

    pub(super) fn resolve_rangeable_evidence(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        item_ty: HirTyId,
    ) {
        let rangeable_symbol = self.known().rangeable;
        let rangeable = self.named_type_for_symbol(rangeable_symbol);
        let obligation = super::schemes::ConstraintObligation {
            kind: ConstraintKind::Implements,
            subject: item_ty,
            value: rangeable,
            class_key: self
                .class_facts_by_name(rangeable_symbol)
                .map(|facts| facts.key.clone()),
        };
        if let Some(evidence) = self.resolve_obligations_to_evidence(origin, &[obligation])
            && !evidence.is_empty()
        {
            self.set_expr_evidence(expr_id, evidence);
        }
    }

    pub(super) fn check_array_ty_expr(
        &mut self,
        dims: &SliceRange<HirDim>,
        item: HirExprId,
    ) -> ExprFacts {
        let origin = self.expr(item).origin;
        let item_ty = self.lower_type_expr(item, origin);
        let ty = if self.dims(dims.clone()).is_empty() {
            self.alloc_ty(HirTyKind::Seq { item: item_ty })
        } else {
            self.alloc_ty(HirTyKind::Array {
                dims: dims.clone(),
                item: item_ty,
            })
        };
        ExprFacts::new(ty, EffectRow::empty())
    }

    pub(super) fn check_handler_ty_expr(
        &mut self,
        effect: HirExprId,
        input: HirExprId,
        output: HirExprId,
    ) -> ExprFacts {
        let effect_origin = self.expr(effect).origin;
        let effect = self.lower_type_expr(effect, effect_origin);
        let input_origin = self.expr(input).origin;
        let input = self.lower_type_expr(input, input_origin);
        let output_origin = self.expr(output).origin;
        let output = self.lower_type_expr(output, output_origin);
        let ty = self.alloc_ty(HirTyKind::Handler {
            effect,
            input,
            output,
        });
        ExprFacts::new(ty, EffectRow::empty())
    }

    fn expected_array_contract(&self) -> (Option<SliceRange<HirDim>>, Option<HirTyId>, bool) {
        let expected_array = self.expected_ty().and_then(|expected| {
            let expected_inner = peel_mut_ty(self, expected);
            match self.ty(expected_inner).kind {
                HirTyKind::Array { dims, item } => Some((dims, item)),
                HirTyKind::Seq { item } => Some((SliceRange::EMPTY, item)),
                _ => None,
            }
        });
        let expected_dims = expected_array.as_ref().map(|(dims, _)| dims.clone());
        let expected_item = expected_array.as_ref().map(|(_, item)| *item);
        let expected_seq = expected_dims.as_ref().is_some_and(SliceRange::is_empty);
        (expected_dims, expected_item, expected_seq)
    }

    fn merge_array_item_ty(&mut self, origin: HirOrigin, item_ty: &mut HirTyId, found: HirTyId) {
        let builtins = self.builtins();
        if *item_ty == builtins.unknown {
            *item_ty = found;
        } else {
            self.type_mismatch(origin, *item_ty, found);
        }
    }

    fn check_array_literal_expected_len(
        &mut self,
        expected_dims: Option<&SliceRange<HirDim>>,
        items: &[HirArrayItem],
        has_runtime_spread: bool,
        known_len: u32,
    ) {
        let Some(expected_dims) = expected_dims else {
            return;
        };
        let dims_vec = self.dims(expected_dims.clone());
        if dims_vec.len() != 1 {
            return;
        }
        let HirDim::Int(expected_len) = dims_vec[0] else {
            return;
        };
        let span = items.first().map_or_else(
            || Span::new(0, 0),
            |array_item| self.expr(array_item.expr).origin.span,
        );
        if has_runtime_spread {
            self.diag(
                span,
                DiagKind::ArrayLiteralLengthUnknownFromRuntimeSpread,
                "",
            );
        } else if expected_len != known_len {
            self.diag(span, DiagKind::ArrayLiteralLengthMismatch, "");
        }
    }

    pub(super) fn check_index_args(
        &mut self,
        origin: HirOrigin,
        args: SliceRange<HirExprId>,
        effects: &mut EffectRow,
    ) -> usize {
        let builtins = self.builtins();
        let index_exprs = self.expr_ids(args);
        if index_exprs.is_empty() {
            self.diag(
                origin.span,
                DiagKind::InvalidIndexArgCount,
                "index expression requires at least one argument",
            );
        }
        for index_expr in &index_exprs {
            let facts = super::exprs::check_expr(self, *index_expr);
            effects.union_with(&facts.effects);
            let index_origin = self.expr(*index_expr).origin;
            self.type_mismatch(index_origin, builtins.int_, facts.ty);
        }
        index_exprs.len()
    }
}
