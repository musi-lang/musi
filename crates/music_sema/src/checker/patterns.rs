use std::collections::{BTreeMap, BTreeSet};

use music_arena::SliceRange;
use music_base::Span;
use music_hir::{HirPatId, HirPatKind, HirRecordPatField, HirTyId, HirTyKind};
use music_names::Ident;
use music_names::NameBindingId;

use crate::api::{PatFacts, SemaDataVariantDef};

use super::exprs::check_expr;
use super::{CheckPass, DiagKind, PassBase};

pub fn bound_name_from_pat(ctx: &PassBase<'_, '_, '_>, pat: HirPatId) -> Option<Ident> {
    match ctx.pat(pat).kind {
        HirPatKind::Bind { name } => Some(name),
        _ => None,
    }
}

pub fn pat_is_irrefutable(ctx: &PassBase<'_, '_, '_>, pat: HirPatId) -> bool {
    match ctx.pat(pat).kind {
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Bind { .. } => true,
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => ctx
            .pat_ids(items)
            .into_iter()
            .all(|item| pat_is_irrefutable(ctx, item)),
        HirPatKind::Record { fields } => ctx.record_pat_fields(fields).into_iter().all(|field| {
            field
                .value
                .is_none_or(|value| pat_is_irrefutable(ctx, value))
        }),
        HirPatKind::As { pat, .. } => pat_is_irrefutable(ctx, pat),
        HirPatKind::Lit { .. } | HirPatKind::Variant { .. } | HirPatKind::Or { .. } => false,
    }
}

pub fn bind_pat(ctx: &mut CheckPass<'_, '_, '_>, pat: HirPatId, ty: HirTyId) {
    ctx.bind_pat_inner(pat, ty);
}

impl CheckPass<'_, '_, '_> {
    fn bind_pat_inner(&mut self, pat: HirPatId, ty: HirTyId) {
        let builtins = self.builtins();
        let pat_node = self.pat(pat);
        self.set_pat_facts(pat, PatFacts::new(ty));
        match pat_node.kind {
            HirPatKind::Error | HirPatKind::Wildcard => {}
            HirPatKind::Bind { name } => {
                if let Some(binding) = self.binding_id_for_decl(name) {
                    self.insert_binding_type(binding, ty);
                }
            }
            HirPatKind::Lit { expr } => {
                let facts = check_expr(self, expr);
                let origin = self.expr(expr).origin;
                self.type_mismatch(origin, ty, facts.ty);
            }
            HirPatKind::Tuple { items } => {
                self.bind_tuple_pat(items, ty, builtins.unknown);
            }
            HirPatKind::Array { items } => {
                let item_ty = match self.ty(ty).kind {
                    HirTyKind::Array { item, .. } => item,
                    _ => builtins.unknown,
                };
                for item in self.pat_ids(items) {
                    self.bind_pat_inner(item, item_ty);
                }
            }
            HirPatKind::Record { fields } => {
                self.bind_record_pat(fields, ty, builtins.unknown);
            }
            HirPatKind::Variant { .. } => {
                let HirPatKind::Variant { tag, args } = pat_node.kind else {
                    return;
                };
                self.bind_variant_pat(pat_node.origin.span, ty, tag, args);
            }
            HirPatKind::Or { left, right } => {
                self.bind_or_pat(pat_node.origin.span, ty, left, right);
            }
            HirPatKind::As { pat, name } => {
                self.bind_pat_inner(pat, ty);
                if let Some(binding) = self.binding_id_for_decl(name) {
                    self.insert_binding_type(binding, ty);
                }
            }
        }
    }

    fn bind_tuple_pat(&mut self, items: SliceRange<HirPatId>, ty: HirTyId, fallback: HirTyId) {
        let tuple_items = match self.ty(ty).kind {
            HirTyKind::Tuple { items: tuple_items }
                if self.ty_ids(tuple_items).len() == self.pat_ids(items).len() =>
            {
                Some(self.ty_ids(tuple_items))
            }
            _ => None,
        };
        for (idx, item) in self.pat_ids(items).into_iter().enumerate() {
            let item_ty = tuple_items
                .as_ref()
                .and_then(|items| items.get(idx).copied())
                .unwrap_or(fallback);
            self.bind_pat_inner(item, item_ty);
        }
    }

    fn bind_record_pat(
        &mut self,
        fields: SliceRange<HirRecordPatField>,
        ty: HirTyId,
        fallback: HirTyId,
    ) {
        let record_fields = match self.ty(ty).kind {
            HirTyKind::Record { fields } => self
                .ty_fields(fields)
                .into_iter()
                .map(|field| (field.name, field.ty))
                .collect::<BTreeMap<_, _>>(),
            HirTyKind::Range { bound } | HirTyKind::ClosedRange { bound } => BTreeMap::from([
                (self.intern("lowerBound"), bound),
                (self.intern("upperBound"), bound),
            ]),
            HirTyKind::PartialRangeFrom { bound } => {
                BTreeMap::from([(self.intern("lowerBound"), bound)])
            }
            HirTyKind::PartialRangeUpTo { bound } | HirTyKind::PartialRangeThru { bound } => {
                BTreeMap::from([(self.intern("upperBound"), bound)])
            }
            _ => BTreeMap::new(),
        };
        for field in self.record_pat_fields(fields) {
            let field_ty = record_fields
                .get(&field.name.name)
                .copied()
                .unwrap_or(fallback);
            if let Some(value) = field.value {
                self.bind_pat_inner(value, field_ty);
            } else if let Some(binding) = self.binding_id_for_decl(field.name) {
                self.insert_binding_type(binding, field_ty);
            }
        }
    }

    fn bind_variant_pat(
        &mut self,
        span: Span,
        ty: HirTyId,
        tag: Ident,
        args: SliceRange<HirPatId>,
    ) {
        let builtins = self.builtins();
        let expected_payload = match self.ty(ty).kind {
            HirTyKind::Sum { left, right } => {
                let tag_name = self.resolve_symbol(tag.name);
                let chosen = match tag_name {
                    "Left" => Some(left),
                    "Right" => Some(right),
                    _ => None,
                };
                if chosen.is_some() {
                    let _sum_def = self.ensure_sum_data_def(left, right);
                }
                chosen
            }
            HirTyKind::Named { name, .. } => {
                let data_name = self.resolve_symbol(name);
                let tag_name = self.resolve_symbol(tag.name);
                self.data_def(data_name)
                    .and_then(|data| data.variant(tag_name))
                    .and_then(SemaDataVariantDef::payload)
            }
            HirTyKind::Bool => {
                let tag_name = self.resolve_symbol(tag.name);
                self.data_def("Bool")
                    .and_then(|data| data.variant(tag_name))
                    .and_then(SemaDataVariantDef::payload)
            }
            _ => None,
        };
        let expected_args: Vec<HirTyId> =
            expected_payload.map_or_else(Vec::new, |payload_ty| match &self.ty(payload_ty).kind {
                HirTyKind::Tuple { items } => self.ty_ids(*items),
                _ => vec![payload_ty],
            });
        let args_vec = self.pat_ids(args);
        if expected_args.len() != args_vec.len() {
            self.diag(span, DiagKind::VariantPatternArityMismatch, "");
        }
        for (index, arg) in args_vec.into_iter().enumerate() {
            let expected = expected_args
                .get(index)
                .copied()
                .unwrap_or(builtins.unknown);
            self.bind_pat_inner(arg, expected);
        }
    }

    fn bind_or_pat(&mut self, span: Span, ty: HirTyId, left: HirPatId, right: HirPatId) {
        let left_binders = binders_in_pat(self, left);
        let right_binders = binders_in_pat(self, right);
        if left_binders != right_binders {
            self.diag(span, DiagKind::OrPatternBindersMismatch, "");
        }
        self.bind_pat_inner(left, ty);
        self.bind_pat_inner(right, ty);
    }
}

pub(super) fn binders_in_pat(
    ctx: &CheckPass<'_, '_, '_>,
    pat: HirPatId,
) -> BTreeSet<NameBindingId> {
    let mut out = BTreeSet::<NameBindingId>::new();
    collect_binders(ctx, pat, &mut out);
    out
}

fn collect_binders(ctx: &CheckPass<'_, '_, '_>, pat: HirPatId, out: &mut BTreeSet<NameBindingId>) {
    match ctx.pat(pat).kind {
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Lit { .. } => {}
        HirPatKind::Bind { name } => {
            if let Some(binding) = ctx.binding_id_for_decl(name) {
                let _ = out.insert(binding);
            }
        }
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => {
            for item in ctx.pat_ids(items) {
                collect_binders(ctx, item, out);
            }
        }
        HirPatKind::Record { fields } => {
            for field in ctx.record_pat_fields(fields) {
                if let Some(value) = field.value {
                    collect_binders(ctx, value, out);
                } else if let Some(binding) = ctx.binding_id_for_decl(field.name) {
                    let _ = out.insert(binding);
                }
            }
        }
        HirPatKind::Variant { args, .. } => {
            for arg in ctx.pat_ids(args) {
                collect_binders(ctx, arg, out);
            }
        }
        HirPatKind::Or { left, right } => {
            collect_binders(ctx, left, out);
            collect_binders(ctx, right, out);
        }
        HirPatKind::As { pat, name } => {
            collect_binders(ctx, pat, out);
            if let Some(binding) = ctx.binding_id_for_decl(name) {
                let _ = out.insert(binding);
            }
        }
    }
}
