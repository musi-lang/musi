use super::{
    HirArg, HirExprId, HirTyKind, Ident, Interner, IrExprKind, LowerCtx, SemaDataVariantDef,
    SemaModule, SliceRange, lower_expr, lowering_invariant_violation,
};

pub(crate) fn lower_variant_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    tag: Ident,
    args: SliceRange<HirArg>,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let ty = sema
        .try_expr_ty(expr_id)
        .unwrap_or_else(|| lowering_invariant_violation("expr type missing for variant"));
    let data_def = match &sema.ty(ty).kind {
        HirTyKind::Named { name, .. } => {
            let data_name = interner.resolve(*name);
            sema.data_def(data_name)
        }
        HirTyKind::Sum { left, right } => {
            let synth_name = format!("__sum__{}_{}", left.raw(), right.raw());
            sema.data_def(synth_name.as_str())
        }
        _ => None,
    };
    let Some(data_def) = data_def else {
        lowering_invariant_violation("variant without data type definition");
    };
    let tag_name = interner.resolve(tag.name);
    let tag_index = data_def.variant_index(tag_name).unwrap_or(u16::MAX);
    let variant_count = u16::try_from(data_def.variant_count()).unwrap_or(u16::MAX);
    if tag_index == u16::MAX {
        lowering_invariant_violation("unknown variant tag");
    }

    let Some(variant) = data_def.variant(tag_name) else {
        lowering_invariant_violation("unknown variant payload metadata");
    };
    let arg_exprs = ordered_variant_arg_exprs(sema, variant, args, interner);
    let lowered_args = arg_exprs
        .into_iter()
        .map(|expr| lower_expr(ctx, expr))
        .collect::<Vec<_>>()
        .into_boxed_slice();
    let field_count = u16::try_from(lowered_args.len()).unwrap_or(u16::MAX);
    let _ = variant_count;
    IrExprKind::VariantNew {
        data_key: data_def.key().clone(),
        tag_index,
        tag_value: variant.tag(),
        field_count,
        args: lowered_args,
    }
}

pub(crate) fn ordered_variant_arg_exprs(
    sema: &SemaModule,
    variant: &SemaDataVariantDef,
    args: SliceRange<HirArg>,
    interner: &Interner,
) -> Vec<HirExprId> {
    let arg_nodes = sema.module().store.args.get(args);
    if !variant.field_names().iter().any(Option::is_some) {
        return arg_nodes.iter().map(|arg| arg.expr).collect();
    }
    let mut ordered = vec![None; variant.field_names().len()];
    for arg in arg_nodes {
        let Some(name) = arg.name else {
            lowering_invariant_violation("named variant arg missing field name after sema");
        };
        let name = interner.resolve(name.name);
        let Some(index) = variant
            .field_names()
            .iter()
            .position(|field| field.as_deref() == Some(name))
        else {
            lowering_invariant_violation("unknown named variant field after sema");
        };
        ordered[index] = Some(arg.expr);
    }
    ordered
        .into_iter()
        .map(|expr| {
            expr.unwrap_or_else(|| {
                lowering_invariant_violation("missing named variant field after sema")
            })
        })
        .collect()
}
