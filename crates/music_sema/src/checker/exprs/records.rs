use std::collections::{BTreeMap, BTreeSet, HashMap};

use music_arena::SliceRange;
use music_hir::{HirExprId, HirOrigin, HirRecordItem, HirTyField, HirTyId, HirTyKind};
use music_names::Symbol;

use crate::api::ExprFacts;
use crate::effects::EffectRow;

use super::super::{CheckPass, DiagKind};
use super::peel_mut_ty;

type RecordItemRange = SliceRange<HirRecordItem>;
type RecordFieldMap = BTreeMap<Box<str>, HirTyId>;
type RecordTyFieldMap = BTreeMap<Box<str>, HirTyField>;
type SeenRecordFieldSet = BTreeSet<Box<str>>;

struct RecordExprFields {
    fields: RecordTyFieldMap,
    inferred_record_ty: Option<HirTyId>,
}

impl CheckPass<'_, '_, '_> {
    pub(super) fn check_record_expr(&mut self, items: RecordItemRange) -> ExprFacts {
        let expected_ty = self
            .expected_ty()
            .map(|expected| peel_mut_ty(self, expected));
        let expected_record =
            expected_ty.and_then(|expected| self.expected_record_fields(expected));
        let (record_fields, effects) =
            self.collect_record_expr_fields(items, expected_record.as_ref());
        self.report_missing_record_fields(expected_record.as_ref(), &record_fields.fields);
        self.record_expr_facts(
            expected_ty,
            expected_record.is_some(),
            record_fields,
            effects,
        )
    }

    fn expected_record_fields(&mut self, expected: HirTyId) -> Option<RecordFieldMap> {
        let expected_inner = peel_mut_ty(self, expected);
        match self.ty(expected_inner).kind {
            HirTyKind::Record { fields } => Some(
                self.ty_fields(fields)
                    .into_iter()
                    .map(|field| (self.resolve_symbol(field.name).into(), field.ty))
                    .collect(),
            ),
            HirTyKind::Named { name, args } => self.data_record_fields(name, args),
            _ => None,
        }
    }

    fn collect_record_expr_fields(
        &mut self,
        items: RecordItemRange,
        expected_record: Option<&RecordFieldMap>,
    ) -> (RecordExprFields, EffectRow) {
        let mut effects = EffectRow::empty();
        let mut seen_explicit = SeenRecordFieldSet::new();
        let mut fields = RecordTyFieldMap::new();
        let mut inferred_record_ty = None;
        for record_item in self.record_items(items) {
            self.check_record_item(
                &record_item,
                expected_record,
                &mut fields,
                &mut seen_explicit,
                &mut inferred_record_ty,
                &mut effects,
            );
        }
        (
            RecordExprFields {
                fields,
                inferred_record_ty,
            },
            effects,
        )
    }

    fn check_record_item(
        &mut self,
        record_item: &HirRecordItem,
        expected_record: Option<&RecordFieldMap>,
        fields: &mut RecordTyFieldMap,
        seen_explicit: &mut SeenRecordFieldSet,
        inferred_record_ty: &mut Option<HirTyId>,
        effects: &mut EffectRow,
    ) {
        if record_item.spread {
            self.check_record_spread_item(record_item, fields, inferred_record_ty, effects);
        } else if let Some(name) = record_item.name {
            self.check_record_named_item(
                record_item,
                name.name,
                expected_record,
                fields,
                seen_explicit,
                effects,
            );
        } else {
            let facts = super::check_expr(self, record_item.value);
            effects.union_with(&facts.effects);
        }
    }

    fn check_record_spread_item(
        &mut self,
        record_item: &HirRecordItem,
        fields: &mut RecordTyFieldMap,
        inferred_record_ty: &mut Option<HirTyId>,
        effects: &mut EffectRow,
    ) {
        let facts = super::check_expr(self, record_item.value);
        effects.union_with(&facts.effects);
        let origin = self.expr(record_item.value).origin;
        let spread_ty = peel_mut_ty(self, facts.ty);
        if matches!(self.ty(spread_ty).kind, HirTyKind::Named { .. }) {
            *inferred_record_ty = Some(spread_ty);
        }
        let Some(spread_fields) = self.record_like_fields(spread_ty) else {
            self.diag_spread_source(origin);
            return;
        };
        for (key, spread_ty) in spread_fields {
            let name = self.intern(key.as_ref());
            let _ = fields.insert(key, HirTyField::new(name, spread_ty));
        }
    }

    fn check_record_named_item(
        &mut self,
        record_item: &HirRecordItem,
        name: Symbol,
        expected_record: Option<&RecordFieldMap>,
        fields: &mut RecordTyFieldMap,
        seen_explicit: &mut SeenRecordFieldSet,
        effects: &mut EffectRow,
    ) {
        let expected = self.expected_record_field_ty(name, expected_record, fields);
        self.report_unknown_record_field(name, record_item, expected_record);
        self.push_expected_ty(expected);
        let facts = super::check_expr(self, record_item.value);
        let _ = self.pop_expected_ty();
        effects.union_with(&facts.effects);
        let origin = self.expr(record_item.value).origin;
        self.type_mismatch(origin, expected, facts.ty);
        self.insert_checked_record_field(name, record_item.value, facts.ty, fields, seen_explicit);
    }

    fn expected_record_field_ty(
        &self,
        name: Symbol,
        expected_record: Option<&RecordFieldMap>,
        fields: &RecordTyFieldMap,
    ) -> HirTyId {
        expected_record
            .and_then(|map| map.get(self.resolve_symbol(name)).copied())
            .or_else(|| fields.get(self.resolve_symbol(name)).map(|field| field.ty))
            .unwrap_or_else(|| self.builtins().unknown)
    }

    fn report_unknown_record_field(
        &mut self,
        name: Symbol,
        record_item: &HirRecordItem,
        expected_record: Option<&RecordFieldMap>,
    ) {
        if expected_record.is_none_or(|map| map.contains_key(self.resolve_symbol(name))) {
            return;
        }
        let field_name = self.resolve_symbol(name).to_owned();
        self.diag_message(
            record_item.name.expect("record name checked").span,
            DiagKind::UnknownField,
            format!("unknown field `{field_name}`"),
            format!("unknown field `{field_name}`"),
        );
    }

    fn insert_checked_record_field(
        &mut self,
        name: Symbol,
        value: HirExprId,
        ty: HirTyId,
        fields: &mut RecordTyFieldMap,
        seen_explicit: &mut SeenRecordFieldSet,
    ) {
        let key: Box<str> = self.resolve_symbol(name).into();
        if !seen_explicit.insert(key.clone()) {
            let span = self.expr(value).origin.span;
            self.diag(span, DiagKind::DuplicateRecordField, "");
        }
        let _ = fields.insert(key, HirTyField::new(name, ty));
    }

    fn report_missing_record_fields(
        &mut self,
        expected_record: Option<&RecordFieldMap>,
        fields: &RecordTyFieldMap,
    ) {
        let Some(expected) = expected_record else {
            return;
        };
        for field_name in expected.keys() {
            if !fields.contains_key(field_name) {
                self.diag_missing_record_field(field_name);
            }
        }
    }

    fn diag_missing_record_field(&mut self, field_name: &str) {
        let span = self.expr(self.root_expr_id()).origin.span;
        self.diag_message(
            span,
            DiagKind::MissingRecordField,
            format!("missing field `{field_name}`"),
            format!("field `{field_name}` required here"),
        );
    }

    fn record_expr_facts(
        &mut self,
        expected_ty: Option<HirTyId>,
        has_expected_record: bool,
        record_fields: RecordExprFields,
        effects: EffectRow,
    ) -> ExprFacts {
        if let Some(expected_ty) = expected_ty
            && has_expected_record
            && matches!(self.ty(expected_ty).kind, HirTyKind::Named { .. })
        {
            return ExprFacts::new(expected_ty, effects);
        }
        if let Some(facts) = self.inferred_record_expr_facts(&record_fields, &effects) {
            return facts;
        }
        let fields = self.alloc_ty_fields(record_fields.fields.into_values());
        let ty = self.alloc_ty(HirTyKind::Record { fields });
        ExprFacts::new(ty, effects)
    }

    fn inferred_record_expr_facts(
        &mut self,
        record_fields: &RecordExprFields,
        effects: &EffectRow,
    ) -> Option<ExprFacts> {
        let inferred_record_ty = record_fields.inferred_record_ty?;
        let expected = self.record_like_fields(inferred_record_ty)?;
        let all_expected_found = expected
            .keys()
            .all(|field_name| record_fields.fields.contains_key(field_name));
        let no_extra_fields = record_fields
            .fields
            .keys()
            .all(|field_name| expected.contains_key(field_name));
        (all_expected_found && no_extra_fields)
            .then(|| ExprFacts::new(inferred_record_ty, effects.clone()))
    }

    fn diag_spread_source(&mut self, origin: HirOrigin) {
        self.diag(
            origin.span,
            DiagKind::InvalidSpreadSource,
            "record spread source must be record value",
        );
    }

    pub(super) fn check_record_update_expr(
        &mut self,
        origin: HirOrigin,
        base: HirExprId,
        items: RecordItemRange,
    ) -> ExprFacts {
        let base_facts = super::check_expr(self, base);
        let mut effects = base_facts.effects.clone();
        let base_ty = peel_mut_ty(self, base_facts.ty);
        let mut fields = self.record_like_fields(base_ty).unwrap_or_else(|| {
            self.diag(origin.span, DiagKind::InvalidRecordUpdateTarget, "");
            BTreeMap::new()
        });
        for record_item in self.record_items(items) {
            if record_item.spread {
                let facts = super::check_expr(self, record_item.value);
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
            let facts = super::check_expr(self, record_item.value);
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
            HirTyKind::Named { .. } if self.record_like_fields(base_ty).is_some() => Some(base_ty),
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

    pub(in super::super) fn record_like_fields(
        &mut self,
        ty: HirTyId,
    ) -> Option<BTreeMap<Box<str>, HirTyId>> {
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
            HirTyKind::Named { name, args } => self.data_record_fields(name, args),
            _ => None,
        }
    }

    pub(super) fn record_like_field_ty(
        &mut self,
        ty: HirTyId,
        field_name: &str,
    ) -> Option<HirTyId> {
        self.record_like_fields(ty)
            .and_then(|fields| fields.get(field_name).copied())
    }

    fn data_record_fields(
        &mut self,
        name: Symbol,
        args: SliceRange<HirTyId>,
    ) -> Option<BTreeMap<Box<str>, HirTyId>> {
        let data = self.data_def(self.resolve_symbol(name))?.clone();
        let variant = data.record_shape_variant()?;
        let ty_args = self.ty_ids(args);
        let subst = data
            .type_params()
            .iter()
            .copied()
            .zip(ty_args)
            .collect::<HashMap<_, _>>();
        Some(
            variant
                .field_names()
                .iter()
                .zip(variant.field_tys())
                .filter_map(|(field_name, field_ty)| {
                    field_name.as_ref().map(|field_name| {
                        (field_name.clone(), self.substitute_ty(*field_ty, &subst))
                    })
                })
                .collect(),
        )
    }
}
