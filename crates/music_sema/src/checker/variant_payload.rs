use music_hir::{HirTyId, HirVariantFieldDef};

use super::PassBase;

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
