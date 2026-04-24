use std::collections::{BTreeMap, BTreeSet};

use music_arena::SliceRange;
use music_base::{Span, diag::DiagContext};
use music_hir::{HirPatId, HirPatKind, HirRecordPatField, HirTyId, HirTyKind, HirVariantPatArg};
use music_names::Ident;
use music_names::NameBindingId;

use crate::api::PatFacts;

use super::exprs::check_expr;
use super::{CheckPass, DiagKind, PassBase};

type VariantFieldNames = [Option<Box<str>>];

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
        let record_fields = self
            .record_like_fields(ty)
            .unwrap_or_default()
            .into_iter()
            .map(|(name, ty)| (self.intern(name.as_ref()), ty))
            .collect::<BTreeMap<_, _>>();
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
        args: SliceRange<HirVariantPatArg>,
    ) {
        let builtins = self.builtins();
        let (expected_args, field_names) = self.variant_pat_expected_args(ty, tag);
        let args_vec = self.variant_pat_args(args);
        let named_variant = field_names.iter().any(Option::is_some);
        let named_args = args_vec
            .iter()
            .any(|arg| self.variant_pat_arg_name(arg, &field_names).is_some());
        if named_variant {
            self.bind_named_variant_pat(
                span,
                args_vec,
                &expected_args,
                &field_names,
                named_args,
                builtins.unknown,
            );
            return;
        }
        if named_args {
            self.diag(span, DiagKind::VariantNamedFieldsUnexpected, "");
        }
        if expected_args.len() != args_vec.len() {
            self.diag(span, DiagKind::VariantPatternArityMismatch, "");
        }
        for (index, arg) in args_vec.into_iter().enumerate() {
            let expected = expected_args
                .get(index)
                .copied()
                .unwrap_or(builtins.unknown);
            self.bind_pat_inner(arg.pat, expected);
        }
    }

    fn variant_pat_expected_args(
        &mut self,
        ty: HirTyId,
        tag: Ident,
    ) -> (Vec<HirTyId>, Vec<Option<Box<str>>>) {
        match self.ty(ty).kind {
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
                let expected_args =
                    chosen.map_or_else(Vec::new, |payload_ty| match &self.ty(payload_ty).kind {
                        HirTyKind::Tuple { items } => self.ty_ids(*items),
                        _ => vec![payload_ty],
                    });
                (expected_args, Vec::new())
            }
            HirTyKind::Named { name, .. } => {
                let data_name = self.resolve_symbol(name);
                let tag_name = self.resolve_symbol(tag.name);
                self.data_def(data_name)
                    .and_then(|data| data.variant(tag_name))
                    .map(|variant| (variant.field_tys().to_vec(), variant.field_names().to_vec()))
                    .unwrap_or_default()
            }
            HirTyKind::Bool => {
                let tag_name = self.resolve_symbol(tag.name);
                self.data_def("Bool")
                    .and_then(|data| data.variant(tag_name))
                    .map(|variant| (variant.field_tys().to_vec(), variant.field_names().to_vec()))
                    .unwrap_or_default()
            }
            _ => (Vec::new(), Vec::new()),
        }
    }

    fn bind_named_variant_pat(
        &mut self,
        span: Span,
        args_vec: Vec<HirVariantPatArg>,
        expected_args: &[HirTyId],
        field_names: &VariantFieldNames,
        named_args: bool,
        fallback: HirTyId,
    ) {
        if !named_args {
            self.diag(span, DiagKind::VariantNamedFieldsRequired, "");
        }
        let mut seen = BTreeSet::new();
        for arg in args_vec {
            let Some(name) = self.variant_pat_arg_name(&arg, field_names) else {
                self.bind_pat_inner(arg.pat, fallback);
                continue;
            };
            if !seen.insert(name.name) {
                let field_name = self.resolve_symbol(name.name).to_owned();
                self.diag_with(
                    name.span,
                    DiagKind::DuplicateVariantField,
                    DiagContext::new().with("field", field_name),
                );
            }
            let expected =
                self.variant_field_expected_ty(name, expected_args, field_names, fallback);
            self.bind_pat_inner(arg.pat, expected);
        }
        for field_name in field_names.iter().flatten() {
            if !seen.contains(&self.intern(field_name)) {
                self.diag_with(
                    span,
                    DiagKind::MissingVariantField,
                    DiagContext::new().with("field", field_name),
                );
            }
        }
    }

    fn variant_field_expected_ty(
        &mut self,
        name: Ident,
        expected_args: &[HirTyId],
        field_names: &VariantFieldNames,
        fallback: HirTyId,
    ) -> HirTyId {
        let field_name = self.resolve_symbol(name.name).to_owned();
        field_names
            .iter()
            .position(|field| field.as_deref() == Some(field_name.as_str()))
            .and_then(|index| expected_args.get(index).copied())
            .unwrap_or_else(|| {
                self.diag_with(
                    name.span,
                    DiagKind::UnknownVariantField,
                    DiagContext::new().with("field", field_name),
                );
                fallback
            })
    }

    fn variant_pat_arg_name(
        &self,
        arg: &HirVariantPatArg,
        field_names: &VariantFieldNames,
    ) -> Option<Ident> {
        if let Some(name) = arg.name {
            return Some(name);
        }
        let HirPatKind::Bind { name } = self.pat(arg.pat).kind else {
            return None;
        };
        let binder_name = self.resolve_symbol(name.name);
        field_names
            .iter()
            .flatten()
            .any(|field_name| field_name.as_ref() == binder_name)
            .then_some(name)
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
            for arg in ctx.variant_pat_args(args) {
                collect_binders(ctx, arg.pat, out);
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
