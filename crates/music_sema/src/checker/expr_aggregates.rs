use std::collections::{BTreeMap, BTreeSet};

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirAccessKind, HirArrayItem, HirBinaryOp, HirDim, HirExprId, HirExprKind, HirOrigin,
    HirPartialRangeKind, HirRecordItem, HirTyField, HirTyId, HirTyKind,
};
use music_names::{Ident, Symbol};

use crate::api::{ConstraintKind, ExprFacts};
use crate::effects::EffectRow;

use super::decls::module_export_for_expr;
use super::exprs::peel_mut_ty;
use super::state::DataDef;
use super::{CheckPass, DiagKind};

type ExprIdList = Vec<HirExprId>;
type TyIdList = Vec<HirTyId>;

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
                self.diag(origin.span, DiagKind::InvalidIndexArity, "");
            }
            item
        } else if let HirTyKind::Seq { item } = self.ty(peel_mut_ty(self, base_facts.ty)).kind {
            if arg_count != 1 {
                self.diag(origin.span, DiagKind::InvalidIndexArity, "");
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
                self.push_expected_ty(item_ty);
                let facts = super::exprs::check_expr(self, array_item.expr);
                let _ = self.pop_expected_ty();
                effects.union_with(&facts.effects);
                if item_ty == builtins.unknown {
                    item_ty = facts.ty;
                } else {
                    let origin = self.expr(array_item.expr).origin;
                    self.type_mismatch(origin, item_ty, facts.ty);
                }
                known_len = known_len.saturating_add(1);
                continue;
            }

            let spread_facts = super::exprs::check_expr(self, array_item.expr);
            effects.union_with(&spread_facts.effects);
            let spread_origin = self.expr(array_item.expr).origin;
            let spread_ty = peel_mut_ty(self, spread_facts.ty);
            match self.ty(spread_ty).kind {
                HirTyKind::Tuple { items } => {
                    let item_tys = self.ty_ids(items);
                    for found in item_tys {
                        self.merge_array_item_ty(spread_origin, &mut item_ty, found);
                        known_len = known_len.saturating_add(1);
                    }
                }
                HirTyKind::Array { dims, item } => {
                    let dims_vec = self.dims(dims);
                    if dims_vec.is_empty() {
                        has_runtime_spread = true;
                        self.merge_array_item_ty(spread_origin, &mut item_ty, item);
                        continue;
                    }
                    if dims_vec.len() != 1 {
                        self.diag(
                            spread_origin.span,
                            DiagKind::ArraySpreadRequiresOneDimensionalArray,
                            "",
                        );
                        continue;
                    }
                    match dims_vec[0] {
                        HirDim::Int(len) => {
                            self.merge_array_item_ty(spread_origin, &mut item_ty, item);
                            known_len = known_len.saturating_add(len);
                        }
                        HirDim::Unknown | HirDim::Name(_) => {
                            has_runtime_spread = true;
                            self.merge_array_item_ty(spread_origin, &mut item_ty, item);
                        }
                    }
                }
                HirTyKind::Seq { item }
                | HirTyKind::Range { bound: item }
                | HirTyKind::ClosedRange { bound: item }
                | HirTyKind::PartialRangeFrom { bound: item }
                | HirTyKind::PartialRangeUpTo { bound: item }
                | HirTyKind::PartialRangeThru { bound: item } => {
                    has_runtime_spread = true;
                    self.merge_array_item_ty(spread_origin, &mut item_ty, item);
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
                _ => self.diag(spread_origin.span, DiagKind::InvalidArraySpreadSource, ""),
            }
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

    fn resolve_rangeable_evidence(
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

    pub(super) fn check_record_expr(&mut self, items: SliceRange<HirRecordItem>) -> ExprFacts {
        let mut effects = EffectRow::empty();
        let expected_record = self.expected_ty().and_then(|expected| {
            let expected_inner = peel_mut_ty(self, expected);
            match self.ty(expected_inner).kind {
                HirTyKind::Record { fields } => Some(
                    self.ty_fields(fields)
                        .into_iter()
                        .map(|field| (field.name, field.ty))
                        .collect::<BTreeMap<_, _>>(),
                ),
                _ => None,
            }
        });

        let mut seen_explicit = BTreeSet::<Box<str>>::new();
        let mut fields = BTreeMap::<Box<str>, HirTyField>::new();
        for record_item in self.record_items(items) {
            if record_item.spread {
                let facts = super::exprs::check_expr(self, record_item.value);
                effects.union_with(&facts.effects);
                let origin = self.expr(record_item.value).origin;
                let spread_ty = peel_mut_ty(self, facts.ty);
                let HirTyKind::Record {
                    fields: spread_fields,
                } = self.ty(spread_ty).kind
                else {
                    self.diag(origin.span, DiagKind::InvalidRecordSpreadSource, "");
                    continue;
                };
                for spread_field in self.ty_fields(spread_fields) {
                    let key: Box<str> = self.resolve_symbol(spread_field.name).into();
                    let _ = fields.insert(key, spread_field);
                }
                continue;
            }

            let Some(name) = record_item.name else {
                let facts = super::exprs::check_expr(self, record_item.value);
                effects.union_with(&facts.effects);
                continue;
            };
            let expected_field_ty = expected_record
                .as_ref()
                .and_then(|map| map.get(&name.name).copied())
                .unwrap_or_else(|| self.builtins().unknown);
            self.push_expected_ty(expected_field_ty);
            let facts = super::exprs::check_expr(self, record_item.value);
            let _ = self.pop_expected_ty();
            effects.union_with(&facts.effects);

            let key: Box<str> = self.resolve_symbol(name.name).into();
            if !seen_explicit.insert(key.clone()) {
                let span = self.expr(record_item.value).origin.span;
                self.diag(span, DiagKind::DuplicateRecordField, "");
            }
            let _ = fields.insert(key, HirTyField::new(name.name, facts.ty));
        }
        let fields = self.alloc_ty_fields(fields.into_values());
        let ty = self.alloc_ty(HirTyKind::Record { fields });
        ExprFacts::new(ty, effects)
    }
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
                return ExprFacts::new(ty, effects);
            }
        }

        let base_ty = peel_mut_ty(self, base_facts.ty);
        let ty = self
            .record_like_field_ty(base_ty, self.resolve_symbol(name.name))
            .unwrap_or_else(|| {
                self.check_non_record_field_expr(expr_id, origin, base, access, name)
            });
        ExprFacts::new(ty, effects)
    }

    pub(super) fn check_record_update_expr(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        items: SliceRange<HirRecordItem>,
    ) -> ExprFacts {
        let base_facts = super::exprs::check_expr(self, base);
        let mut effects = base_facts.effects.clone();
        let base_ty = peel_mut_ty(self, base_facts.ty);
        let mut fields = self.record_like_fields(base_ty).unwrap_or_else(|| {
            self.diag(origin.span, DiagKind::InvalidRecordUpdateTarget, "");
            BTreeMap::new()
        });
        for record_item in self.record_items(items) {
            if record_item.spread {
                let facts = super::exprs::check_expr(self, record_item.value);
                effects.union_with(&facts.effects);
                let spread_origin = self.expr(record_item.value).origin;
                let spread_ty = peel_mut_ty(self, facts.ty);
                let Some(spread_fields) = self.record_like_fields(spread_ty) else {
                    self.diag(spread_origin.span, DiagKind::InvalidRecordSpreadSource, "");
                    continue;
                };
                for (field_name, field_ty) in spread_fields {
                    let _prev = fields.insert(field_name, field_ty);
                }
                continue;
            }

            let expected = record_item
                .name
                .and_then(|name| fields.get(self.resolve_symbol(name.name)).copied())
                .unwrap_or_else(|| self.builtins().unknown);
            self.push_expected_ty(expected);
            let facts = super::exprs::check_expr(self, record_item.value);
            let _ = self.pop_expected_ty();
            effects.union_with(&facts.effects);
            if let Some(name) = record_item.name {
                let _prev = fields.insert(self.resolve_symbol(name.name).into(), facts.ty);
            }
        }

        ExprFacts::new(
            self.rebuild_record_like_ty(origin, base_ty, fields),
            effects,
        )
    }
}

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
            HirBinaryOp::Pipe
                | HirBinaryOp::Or
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

    pub(super) fn check_variant_expr(
        &mut self,
        tag: Ident,
        args: SliceRange<HirExprId>,
    ) -> ExprFacts {
        let builtins = self.builtins();
        if let Some(facts) = self.check_sum_constructor_variant(tag, args) {
            return facts;
        }

        let mut effects = EffectRow::empty();
        let expected_ty = self
            .expected_ty()
            .and_then(|ty| self.variant_context_ty(ty));
        let expected_ty = expected_ty.or_else(|| self.infer_variant_context_ty(tag));
        let Some(expected_ty) = expected_ty else {
            self.check_exprs_collect_effects(self.expr_ids(args), &mut effects);
            return ExprFacts::new(builtins.unknown, effects);
        };

        let data_def = self.expected_data_def(expected_ty);
        let Some(data_def) = data_def else {
            self.check_exprs_collect_effects(self.expr_ids(args), &mut effects);
            self.diag(tag.span, DiagKind::VariantMissingDataContext, "");
            return ExprFacts::new(builtins.unknown, effects);
        };

        let tag_name = self.resolve_symbol(tag.name).to_owned();
        let Some(variant) = data_def.variant(&tag_name) else {
            self.check_exprs_collect_effects(self.expr_ids(args), &mut effects);
            self.diag(
                tag.span,
                DiagKind::UnknownDataVariant,
                &format!("unknown data variant `{tag_name}`"),
            );
            return ExprFacts::new(expected_ty, effects);
        };

        let expected_payload = variant.payload();
        let arg_exprs = self.expr_ids(args);
        let expected_args: TyIdList =
            expected_payload.map_or_else(Vec::new, |payload_ty| match &self.ty(payload_ty).kind {
                HirTyKind::Tuple { items } => self.ty_ids(*items),
                _ => vec![payload_ty],
            });

        self.typecheck_positional_args(
            tag.span,
            &expected_args,
            arg_exprs,
            &mut effects,
            DiagKind::VariantConstructorArityMismatch,
        );

        ExprFacts::new(expected_ty, effects)
    }
}

impl CheckPass<'_, '_, '_> {
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

    fn check_sum_constructor_variant(
        &mut self,
        tag: Ident,
        args: SliceRange<HirExprId>,
    ) -> Option<ExprFacts> {
        let builtins = self.builtins();
        let mut effects = EffectRow::empty();
        let expected_sum_ty = self.expected_ty().and_then(|ty| {
            let inner = peel_mut_ty(self, ty);
            matches!(self.ty(inner).kind, HirTyKind::Sum { .. }).then_some(inner)
        })?;
        let HirTyKind::Sum { left, right } = self.ty(expected_sum_ty).kind else {
            return Some(ExprFacts::new(builtins.unknown, effects));
        };
        let tag_name = self.resolve_symbol(tag.name);
        let chosen = match tag_name {
            "Left" => Some(left),
            "Right" => Some(right),
            _ => None,
        }?;

        let _ = self.ensure_sum_data_def(left, right);
        let arg_exprs = self.expr_ids(args);
        let expected_args: TyIdList = match &self.ty(chosen).kind {
            HirTyKind::Tuple { items } => self.ty_ids(*items),
            _ => vec![chosen],
        };
        self.typecheck_positional_args(
            tag.span,
            &expected_args,
            arg_exprs,
            &mut effects,
            DiagKind::SumConstructorArityMismatch,
        );
        Some(ExprFacts::new(expected_sum_ty, effects))
    }

    fn typecheck_positional_args(
        &mut self,
        diag_span: Span,
        expected_args: &[HirTyId],
        arg_exprs: ExprIdList,
        effects: &mut EffectRow,
        arity_diag: DiagKind,
    ) {
        let builtins = self.builtins();
        if expected_args.len() != arg_exprs.len() {
            self.diag(diag_span, arity_diag, "");
        }
        for (index, arg) in arg_exprs.into_iter().enumerate() {
            let expected = expected_args
                .get(index)
                .copied()
                .unwrap_or(builtins.unknown);
            self.push_expected_ty(expected);
            let facts = super::exprs::check_expr(self, arg);
            let _ = self.pop_expected_ty();
            effects.union_with(&facts.effects);
            let origin = self.expr(arg).origin;
            self.type_mismatch(origin, expected, facts.ty);
        }
    }

    fn check_exprs_collect_effects(&mut self, exprs: ExprIdList, effects: &mut EffectRow) {
        for expr in exprs {
            let facts = super::exprs::check_expr(self, expr);
            effects.union_with(&facts.effects);
        }
    }

    fn variant_context_ty(&self, ty: HirTyId) -> Option<HirTyId> {
        self.expected_data_def(ty).map(|_| ty)
    }

    fn expected_data_def(&self, ty: HirTyId) -> Option<&DataDef> {
        match self.ty(ty).kind {
            HirTyKind::Bool => self.data_def("Bool"),
            HirTyKind::Named { name, .. } => self.data_def(self.resolve_symbol(name)),
            _ => None,
        }
    }

    fn infer_variant_context_ty(&mut self, tag: Ident) -> Option<HirTyId> {
        let tag_name = self.resolve_symbol(tag.name).to_owned();
        let mut matches = self
            .data_defs()
            .iter()
            .filter_map(|(name, data)| data.variant(&tag_name).is_some().then_some(name.clone()))
            .collect::<Vec<Box<str>>>();

        match matches.len() {
            0 => {
                self.diag(
                    tag.span,
                    DiagKind::UnknownDataVariant,
                    &format!("unknown data variant `{tag_name}`"),
                );
                None
            }
            1 => {
                let data_name = matches.pop()?;
                let name = self.intern(data_name.as_ref());
                let args = self.alloc_ty_list([]);
                Some(self.alloc_ty(HirTyKind::Named { name, args }))
            }
            _ => {
                self.diag(tag.span, DiagKind::AmbiguousVariantTag, "");
                None
            }
        }
    }
}

impl CheckPass<'_, '_, '_> {
    fn check_index_args(
        &mut self,
        origin: HirOrigin,
        args: SliceRange<HirExprId>,
        effects: &mut EffectRow,
    ) -> usize {
        let builtins = self.builtins();
        let index_exprs = self.expr_ids(args);
        if index_exprs.is_empty() {
            self.diag(origin.span, DiagKind::IndexRequiresArgument, "");
        }
        for index_expr in &index_exprs {
            let facts = super::exprs::check_expr(self, *index_expr);
            effects.union_with(&facts.effects);
            let index_origin = self.expr(*index_expr).origin;
            self.type_mismatch(index_origin, builtins.int_, facts.ty);
        }
        index_exprs.len()
    }

    fn check_non_record_field_expr(
        &mut self,
        expr_id: HirExprId,
        origin: HirOrigin,
        base: HirExprId,
        access: HirAccessKind,
        name: Ident,
    ) -> HirTyId {
        let builtins = self.builtins();
        let base_facts = super::exprs::check_expr(self, base);
        let base_ty = peel_mut_ty(self, base_facts.ty);
        match self.ty(base_ty).kind {
            HirTyKind::Module => {
                if let Some((surface, export)) = module_export_for_expr(self, base, name) {
                    if let Some(target) = export.module_target.clone() {
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
                    scheme.ty
                } else {
                    builtins.any
                }
            }
            HirTyKind::Record { .. } | HirTyKind::Range { .. } => {
                let field_name = self.resolve_symbol(name.name).to_owned();
                self.diag(
                    origin.span,
                    DiagKind::UnknownField,
                    &format!("unknown field `{field_name}`"),
                );
                builtins.unknown
            }
            _ => {
                if matches!(access, HirAccessKind::Direct) {
                    self.diag(origin.span, DiagKind::InvalidFieldAccess, "");
                } else {
                    self.diag(origin.span, DiagKind::InvalidOptionalFieldAccess, "");
                }
                builtins.unknown
            }
        }
    }

    fn rebuild_record_like_ty(
        &mut self,
        origin: HirOrigin,
        base_ty: HirTyId,
        fields: BTreeMap<Box<str>, HirTyId>,
    ) -> HirTyId {
        (match self.ty(base_ty).kind {
            HirTyKind::Range { bound } => {
                if let Some(found) = fields.get("lowerBound").copied() {
                    self.type_mismatch(origin, bound, found);
                }
                if let Some(found) = fields.get("upperBound").copied() {
                    self.type_mismatch(origin, bound, found);
                }
                Some(self.alloc_ty(HirTyKind::Range { bound }))
            }
            HirTyKind::ClosedRange { bound } => {
                if let Some(found) = fields.get("lowerBound").copied() {
                    self.type_mismatch(origin, bound, found);
                }
                if let Some(found) = fields.get("upperBound").copied() {
                    self.type_mismatch(origin, bound, found);
                }
                Some(self.alloc_ty(HirTyKind::ClosedRange { bound }))
            }
            HirTyKind::PartialRangeFrom { bound } => {
                if let Some(found) = fields.get("lowerBound").copied() {
                    self.type_mismatch(origin, bound, found);
                }
                Some(self.alloc_ty(HirTyKind::PartialRangeFrom { bound }))
            }
            HirTyKind::PartialRangeUpTo { bound } => {
                if let Some(found) = fields.get("upperBound").copied() {
                    self.type_mismatch(origin, bound, found);
                }
                Some(self.alloc_ty(HirTyKind::PartialRangeUpTo { bound }))
            }
            HirTyKind::PartialRangeThru { bound } => {
                if let Some(found) = fields.get("upperBound").copied() {
                    self.type_mismatch(origin, bound, found);
                }
                Some(self.alloc_ty(HirTyKind::PartialRangeThru { bound }))
            }
            _ => None,
        })
        .unwrap_or_else(|| {
            let fields = fields
                .into_iter()
                .map(|(name, ty)| HirTyField::new(self.intern(name.as_ref()), ty))
                .collect::<Vec<_>>();
            let fields = self.alloc_ty_fields(fields);
            self.alloc_ty(HirTyKind::Record { fields })
        })
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
                    self.diag(origin.span, DiagKind::WriteRequiresMutValue, "");
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
                    self.diag(origin.span, DiagKind::InvalidIndexArity, "");
                }
                item
            }
            HirTyKind::Array { .. } => {
                self.diag(origin.span, DiagKind::WriteRequiresMutArray, "");
                builtins.unknown
            }
            HirTyKind::Seq { item } => {
                if arg_count != 1 {
                    self.diag(origin.span, DiagKind::InvalidIndexArity, "");
                }
                if self.is_mut_ty(base_facts.ty) {
                    item
                } else {
                    self.diag(origin.span, DiagKind::WriteRequiresMutArray, "");
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
                        self.diag(
                            origin.span,
                            DiagKind::UnknownField,
                            &format!("unknown field `{field_name}`"),
                        );
                        builtins.unknown
                    },
                    |field| field.ty,
                ),
            HirTyKind::Record { .. } => {
                self.diag(origin.span, DiagKind::WriteRequiresMutRecord, "");
                builtins.unknown
            }
            _ => {
                self.diag(origin.span, DiagKind::InvalidFieldUpdateTarget, "");
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
            | HirBinaryOp::Ne
            | HirBinaryOp::Lt
            | HirBinaryOp::Gt
            | HirBinaryOp::Le
            | HirBinaryOp::Ge => builtins.bool_,
            HirBinaryOp::Assign
            | HirBinaryOp::Pipe
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

    fn record_like_field_ty(&self, ty: HirTyId, field_name: &str) -> Option<HirTyId> {
        self.record_like_fields(ty)
            .and_then(|fields| fields.get(field_name).copied())
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
}
