use std::collections::{BTreeMap, BTreeSet};

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{
    HirArrayItem, HirDim, HirExprId, HirOrigin, HirRecordItem, HirTyField, HirTyId, HirTyKind,
};
use music_names::Ident;

use crate::api::ExprFacts;
use crate::effects::EffectRow;

use super::exprs::peel_mut_ty;
use super::state::DataDef;
use super::{CheckPass, DiagKind};

type ExprIdList = Vec<HirExprId>;
type TyIdList = Vec<HirTyId>;

impl CheckPass<'_, '_, '_> {
    pub(super) fn check_array_expr(&mut self, items: SliceRange<HirArrayItem>) -> ExprFacts {
        let builtins = self.builtins();
        let mut effects = EffectRow::empty();
        let (expected_dims, expected_item) = self.expected_array_contract();
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
                _ => self.diag(spread_origin.span, DiagKind::InvalidArraySpreadSource, ""),
            }
        }

        self.check_array_literal_expected_len(
            expected_dims.as_ref(),
            &items_vec,
            has_runtime_spread,
            known_len,
        );

        let dims = expected_dims.unwrap_or_else(|| self.alloc_dims([HirDim::Unknown]));
        let ty = self.alloc_ty(HirTyKind::Array {
            dims,
            item: item_ty,
        });
        ExprFacts::new(ty, effects)
    }

    pub(super) fn check_array_ty_expr(
        &mut self,
        dims: SliceRange<HirDim>,
        item: HirExprId,
    ) -> ExprFacts {
        let origin = self.expr(item).origin;
        let item_ty = self.lower_type_expr(item, origin);
        let ty = self.alloc_ty(HirTyKind::Array {
            dims,
            item: item_ty,
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

        let tag_name = self.resolve_symbol(tag.name);
        let Some(variant) = data_def.variant(tag_name) else {
            self.check_exprs_collect_effects(self.expr_ids(args), &mut effects);
            self.diag(tag.span, DiagKind::UnknownDataVariant, "");
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

    fn expected_array_contract(&self) -> (Option<SliceRange<HirDim>>, Option<HirTyId>) {
        let expected_array = self.expected_ty().and_then(|expected| {
            let expected_inner = peel_mut_ty(self, expected);
            match self.ty(expected_inner).kind {
                HirTyKind::Array { dims, item } => Some((dims, item)),
                _ => None,
            }
        });
        let expected_dims = expected_array.as_ref().map(|(dims, _)| dims.clone());
        let expected_item = expected_array.as_ref().map(|(_, item)| *item);
        (expected_dims, expected_item)
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
            HirTyKind::Named { name, .. } => self.data_def(self.resolve_symbol(name)),
            _ => None,
        }
    }

    fn infer_variant_context_ty(&mut self, tag: Ident) -> Option<HirTyId> {
        let tag_name = self.resolve_symbol(tag.name);
        let mut matches = self
            .data_defs()
            .iter()
            .filter_map(|(name, data)| data.variant(tag_name).is_some().then_some(name.clone()))
            .collect::<Vec<Box<str>>>();

        match matches.len() {
            0 => {
                self.diag(tag.span, DiagKind::UnknownDataVariant, "");
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
