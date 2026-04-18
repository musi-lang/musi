use std::collections::{BTreeMap, BTreeSet};

use music_arena::SliceRange;
use music_hir::{HirExprId, HirOrigin, HirRecordItem, HirTyField, HirTyId, HirTyKind};

use crate::api::ExprFacts;
use crate::effects::EffectRow;

use super::exprs::peel_mut_ty;
use super::{CheckPass, DiagKind};

impl CheckPass<'_, '_, '_> {
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
                    self.diag(
                        origin.span,
                        DiagKind::InvalidSpreadSource,
                        "record spread source must be record value",
                    );
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
                    self.diag(
                        spread_origin.span,
                        DiagKind::InvalidSpreadSource,
                        "record spread source must be record value",
                    );
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

    pub(super) fn rebuild_record_like_ty(
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

    pub(super) fn record_like_fields(&self, ty: HirTyId) -> Option<BTreeMap<Box<str>, HirTyId>> {
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

    pub(super) fn record_like_field_ty(&self, ty: HirTyId, field_name: &str) -> Option<HirTyId> {
        self.record_like_fields(ty)
            .and_then(|fields| fields.get(field_name).copied())
    }
}
