use music_hir::{HirOrigin, HirTyId, HirVariantDef, HirVariantFieldDef};

use super::{DataVariantDef, PassBase};

use super::const_eval::data_variant_tag;

pub(super) type VariantPayloadInfo = (Option<HirTyId>, Box<[HirTyId]>, Box<[Option<Box<str>>]>);

pub(super) fn lower_variant_payload(
    ctx: &mut PassBase<'_, '_, '_>,
    fields: &[HirVariantFieldDef],
) -> VariantPayloadInfo {
    if fields.is_empty() {
        return (None, Box::default(), Box::default());
    }
    let field_tys = fields
        .iter()
        .map(|field| {
            let origin = ctx.expr(field.ty).origin;
            ctx.lower_type_expr(field.ty, origin)
        })
        .collect::<Vec<_>>();
    let field_names = fields
        .iter()
        .map(|field| field.name.map(|name| ctx.resolve_symbol(name.name).into()))
        .collect::<Vec<Option<Box<str>>>>()
        .into_boxed_slice();
    let payload = (field_tys.len() == 1).then(|| field_tys[0]);
    (payload, field_tys.into_boxed_slice(), field_names)
}

pub(super) struct LoweredDataVariant {
    pub(super) tag: Box<str>,
    pub(super) tag_value: i64,
    pub(super) origin: HirOrigin,
    pub(super) def: DataVariantDef,
}

pub(super) fn lower_data_variant(
    ctx: &mut PassBase<'_, '_, '_>,
    variant_index: usize,
    variant: HirVariantDef,
) -> LoweredDataVariant {
    let tag: Box<str> = ctx.resolve_symbol(variant.name.name).into();
    let default_tag = i64::try_from(variant_index).unwrap_or(i64::MAX);
    let tag_value = data_variant_tag(ctx, variant.value, default_tag);
    let variant_fields = ctx.variant_fields(variant.fields);
    let (payload, field_tys, field_names) = lower_variant_payload(ctx, &variant_fields);
    let result_ty = variant
        .result
        .map(|expr| ctx.lower_type_expr(expr, variant.origin));
    LoweredDataVariant {
        tag,
        tag_value,
        origin: variant.origin,
        def: DataVariantDef::new(tag_value, payload, result_ty, field_tys, field_names),
    }
}

pub(super) fn variant_payload_style_is_mixed(fields: &[HirVariantFieldDef]) -> bool {
    let has_named = fields.iter().any(|field| field.name.is_some());
    let has_positional = fields.iter().any(|field| field.name.is_none());
    has_named && has_positional
}
