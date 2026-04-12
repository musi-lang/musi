use super::*;

type RecordItemRange = SliceRange<HirRecordItem>;

struct RecordUpdateLayout<'a> {
    result_ty: HirTyId,
    result_indices: &'a BTreeMap<Box<str>, u16>,
    result_count: u16,
    base_fields: Box<[IrRecordLayoutField]>,
    result_fields: Box<[IrRecordLayoutField]>,
}

pub(super) fn lower_record_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: RecordItemRange,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let ty = sema
        .try_expr_ty(expr_id)
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for record literal"));
    let Some((indices, _layout, field_count)) = record_layout_for_ty(sema, ty, interner) else {
        return Err("record without record type".into());
    };
    let origin = to_ir_origin(sema, expr_id);
    let (prelude, sources) = collect_record_sources(ctx, origin, items, &indices)?;
    let lowered_fields = lower_ordered_record_fields(interner, field_count, &indices, &sources)?;

    let mut exprs = prelude;
    exprs.push(IrExpr::new(
        origin,
        IrExprKind::Record {
            ty_name: render_ty_name(sema, ty, interner),
            field_count,
            fields: lowered_fields.into_boxed_slice(),
        },
    ));
    Ok(IrExprKind::Sequence {
        exprs: exprs.into_boxed_slice(),
    })
}

pub(super) fn lower_record_update_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    items: RecordItemRange,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let base_ty = sema
        .try_expr_ty(base)
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for record update base"));
    let Some((_base_indices, base_fields, _base_count)) =
        record_layout_for_ty(sema, base_ty, interner)
    else {
        return Err("record update without record base".into());
    };
    let result_ty = sema
        .try_expr_ty(expr_id)
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for record update result"));
    let Some((result_indices, result_fields, result_count)) =
        record_layout_for_ty(sema, result_ty, interner)
    else {
        return Err("record update without record result".into());
    };
    let update_items = sema.module().store.record_items.get(items);
    let layout = RecordUpdateLayout {
        result_ty,
        result_indices: &result_indices,
        result_count,
        base_fields,
        result_fields,
    };
    if !update_items.iter().any(|record_item| record_item.spread) {
        return lower_record_update_without_spread(ctx, base, update_items, layout);
    }
    lower_record_update_with_spread(ctx, expr_id, base, update_items, layout)
}

fn to_ir_origin(sema: &SemaModule, expr_id: HirExprId) -> IrOrigin {
    let origin = sema.module().store.exprs.get(expr_id).origin;
    IrOrigin::new(origin.source_id, origin.span)
}

type RecordSourceMap = BTreeMap<Box<str>, IrExpr>;
type RecordSourceResult = Result<(Vec<IrExpr>, RecordSourceMap), Box<str>>;

fn collect_record_sources(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    items: RecordItemRange,
    indices: &BTreeMap<Box<str>, u16>,
) -> RecordSourceResult {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let mut prelude = Vec::<IrExpr>::new();
    let mut sources = BTreeMap::<Box<str>, IrExpr>::new();
    for record_item in sema.module().store.record_items.get(items) {
        let temp_expr = lower_item_temp(ctx, origin, record_item.value, &mut prelude);
        if record_item.spread {
            let spread_ty = sema
                .try_expr_ty(record_item.value)
                .unwrap_or_else(|| invalid_lowering_path("expr type missing for record spread"));
            let Some((spread_indices, _spread_layout, _spread_count)) =
                record_layout_for_ty(sema, spread_ty, interner)
            else {
                return Err("record spread without record type".into());
            };
            for (symbol, index) in spread_indices {
                if !indices.contains_key(&symbol) {
                    continue;
                }
                let _ = sources.insert(
                    symbol,
                    IrExpr::new(
                        origin,
                        IrExprKind::RecordGet {
                            base: Box::new(temp_expr.clone()),
                            index,
                        },
                    ),
                );
            }
            continue;
        }
        let Some(name) = record_item.name else {
            return Err("record item without name".into());
        };
        if !indices.contains_key(interner.resolve(name.name)) {
            return Err("record item name missing from record type".into());
        }
        let _ = sources.insert(interner.resolve(name.name).into(), temp_expr);
    }
    Ok((prelude, sources))
}

fn lower_item_temp(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    value_expr: HirExprId,
    prelude: &mut Vec<IrExpr>,
) -> IrExpr {
    let temp = fresh_temp(ctx);
    prelude.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp,
            value: Box::new(lower_expr(ctx, value_expr)),
        },
    ));
    IrExpr::new(origin, IrExprKind::Temp { temp })
}

fn lower_ordered_record_fields(
    _interner: &Interner,
    field_count: u16,
    indices: &BTreeMap<Box<str>, u16>,
    sources: &BTreeMap<Box<str>, IrExpr>,
) -> Result<Vec<IrRecordField>, Box<str>> {
    let mut name_by_index = vec![None::<Box<str>>; usize::from(field_count)];
    for (name, index) in indices {
        let idx = usize::from(*index);
        if idx >= name_by_index.len() {
            continue;
        }
        name_by_index[idx] = Some(name.clone());
    }
    let mut lowered_fields = Vec::<IrRecordField>::new();
    for (idx, name) in name_by_index.into_iter().enumerate() {
        let Some(name) = name else {
            return Err("record layout missing symbol".into());
        };
        let Some(expr) = sources.get(name.as_ref()).cloned() else {
            return Err("record missing field value".into());
        };
        lowered_fields.push(IrRecordField::new(
            name,
            u16::try_from(idx).unwrap_or(u16::MAX),
            expr,
        ));
    }
    Ok(lowered_fields)
}

fn lower_record_update_without_spread(
    ctx: &mut LowerCtx<'_>,
    base: HirExprId,
    update_items: &[HirRecordItem],
    layout: RecordUpdateLayout<'_>,
) -> Result<IrExprKind, Box<str>> {
    let RecordUpdateLayout {
        result_ty,
        result_indices,
        result_count,
        base_fields,
        result_fields,
    } = layout;
    let sema = ctx.sema;
    let interner = ctx.interner;
    let mut updates = Vec::new();
    for record_item in update_items {
        let Some(name) = record_item.name else {
            return Err("record update item without name".into());
        };
        let Some(index) = result_indices.get(interner.resolve(name.name)).copied() else {
            return Err("record update field missing from record type".into());
        };
        updates.push(IrRecordField::new(
            interner.resolve(name.name),
            index,
            lower_expr(ctx, record_item.value),
        ));
    }
    Ok(IrExprKind::RecordUpdate {
        ty_name: render_ty_name(sema, result_ty, interner),
        field_count: result_count,
        base: Box::new(lower_expr(ctx, base)),
        base_fields,
        result_fields,
        updates: updates.into_boxed_slice(),
    })
}

fn lower_record_update_with_spread(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    update_items: &[HirRecordItem],
    layout: RecordUpdateLayout<'_>,
) -> Result<IrExprKind, Box<str>> {
    let RecordUpdateLayout {
        result_ty,
        result_indices,
        result_count,
        base_fields,
        result_fields,
    } = layout;
    let sema = ctx.sema;
    let interner = ctx.interner;
    let origin = to_ir_origin(sema, expr_id);
    let mut prelude = Vec::<IrExpr>::new();
    let base_expr = lower_item_temp(ctx, origin, base, &mut prelude);
    let mut updates = Vec::<IrRecordField>::new();
    for record_item in update_items {
        let temp_expr = lower_item_temp(ctx, origin, record_item.value, &mut prelude);
        if record_item.spread {
            let spread_ty = sema.try_expr_ty(record_item.value).unwrap_or_else(|| {
                invalid_lowering_path("expr type missing for record update spread")
            });
            let Some((spread_indices, _spread_fields, _spread_count)) =
                record_layout_for_ty(sema, spread_ty, interner)
            else {
                return Err("record update spread without record type".into());
            };
            for (symbol, spread_index) in spread_indices {
                let Some(result_index) = result_indices.get(symbol.as_ref()).copied() else {
                    continue;
                };
                updates.push(IrRecordField::new(
                    symbol,
                    result_index,
                    IrExpr::new(
                        origin,
                        IrExprKind::RecordGet {
                            base: Box::new(temp_expr.clone()),
                            index: spread_index,
                        },
                    ),
                ));
            }
            continue;
        }
        let Some(name) = record_item.name else {
            return Err("record update item without name".into());
        };
        let Some(index) = result_indices.get(interner.resolve(name.name)).copied() else {
            return Err("record update field missing from record type".into());
        };
        updates.push(IrRecordField::new(
            interner.resolve(name.name),
            index,
            temp_expr,
        ));
    }
    prelude.push(IrExpr::new(
        origin,
        IrExprKind::RecordUpdate {
            ty_name: render_ty_name(sema, result_ty, interner),
            field_count: result_count,
            base: Box::new(base_expr),
            base_fields,
            result_fields,
            updates: updates.into_boxed_slice(),
        },
    ));
    Ok(IrExprKind::Sequence {
        exprs: prelude.into_boxed_slice(),
    })
}
