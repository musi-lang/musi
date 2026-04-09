use super::*;

struct RecordUpdateLayout<'a> {
    result_ty: HirTyId,
    result_indices: &'a BTreeMap<Symbol, u16>,
    result_count: u16,
    base_fields: Box<[IrRecordLayoutField]>,
    result_fields: Box<[IrRecordLayoutField]>,
}

pub(super) fn lower_record_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirRecordItem>,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let ty = sema
        .try_expr_ty(expr_id)
        .expect("expr type missing for record literal");
    let Some((indices, _layout, field_count)) = record_layout_for_ty(sema, ty, interner) else {
        invalid_lowering_path("record without record type");
    };
    let origin = to_ir_origin(sema, expr_id);
    let (prelude, sources) = collect_record_sources(ctx, origin, items, &indices);
    let lowered_fields = lower_ordered_record_fields(interner, field_count, &indices, &sources);

    let mut exprs = prelude;
    exprs.push(IrExpr {
        origin,
        kind: IrExprKind::Record {
            ty_name: render_ty_name(sema, ty, interner),
            field_count,
            fields: lowered_fields.into_boxed_slice(),
        },
    });
    IrExprKind::Sequence {
        exprs: exprs.into_boxed_slice(),
    }
}

pub(super) fn lower_record_update_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    items: SliceRange<HirRecordItem>,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let base_ty = sema
        .try_expr_ty(base)
        .expect("expr type missing for record update base");
    let Some((_base_indices, base_fields, _base_count)) =
        record_layout_for_ty(sema, base_ty, interner)
    else {
        invalid_lowering_path("record update without record base");
    };
    let result_ty = sema
        .try_expr_ty(expr_id)
        .expect("expr type missing for record update result");
    let Some((result_indices, result_fields, result_count)) =
        record_layout_for_ty(sema, result_ty, interner)
    else {
        invalid_lowering_path("record update without record result");
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
    IrOrigin {
        source_id: origin.source_id,
        span: origin.span,
    }
}

fn collect_record_sources(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    items: SliceRange<HirRecordItem>,
    indices: &BTreeMap<Symbol, u16>,
) -> (Vec<IrExpr>, BTreeMap<Symbol, IrExpr>) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let mut prelude = Vec::<IrExpr>::new();
    let mut sources = BTreeMap::<Symbol, IrExpr>::new();
    for record_item in sema.module().store.record_items.get(items) {
        let temp_expr = lower_item_temp(ctx, origin, record_item.value, &mut prelude);
        if record_item.spread {
            let spread_ty = sema
                .try_expr_ty(record_item.value)
                .expect("expr type missing for record spread");
            let Some((spread_indices, _spread_layout, _spread_count)) =
                record_layout_for_ty(sema, spread_ty, interner)
            else {
                invalid_lowering_path("record spread without record type");
            };
            for (symbol, index) in spread_indices {
                if !indices.contains_key(&symbol) {
                    continue;
                }
                let _ = sources.insert(
                    symbol,
                    IrExpr {
                        origin,
                        kind: IrExprKind::RecordGet {
                            base: Box::new(temp_expr.clone()),
                            index,
                        },
                    },
                );
            }
            continue;
        }
        let Some(name) = record_item.name else {
            invalid_lowering_path("record item without name");
        };
        if !indices.contains_key(&name.name) {
            invalid_lowering_path("record item name missing from record type");
        }
        let _ = sources.insert(name.name, temp_expr);
    }
    (prelude, sources)
}

fn lower_item_temp(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    value_expr: HirExprId,
    prelude: &mut Vec<IrExpr>,
) -> IrExpr {
    let temp = fresh_temp(ctx);
    prelude.push(IrExpr {
        origin,
        kind: IrExprKind::TempLet {
            temp,
            value: Box::new(lower_expr(ctx, value_expr)),
        },
    });
    IrExpr {
        origin,
        kind: IrExprKind::Temp { temp },
    }
}

fn lower_ordered_record_fields(
    interner: &Interner,
    field_count: u16,
    indices: &BTreeMap<Symbol, u16>,
    sources: &BTreeMap<Symbol, IrExpr>,
) -> Vec<IrRecordField> {
    let mut symbol_by_index = vec![None::<Symbol>; usize::from(field_count)];
    for (symbol, index) in indices {
        let idx = usize::from(*index);
        if idx >= symbol_by_index.len() {
            continue;
        }
        symbol_by_index[idx] = Some(*symbol);
    }
    let mut lowered_fields = Vec::<IrRecordField>::new();
    for (idx, symbol) in symbol_by_index.into_iter().enumerate() {
        let Some(symbol) = symbol else {
            invalid_lowering_path("record layout missing symbol");
        };
        let Some(expr) = sources.get(&symbol).cloned() else {
            invalid_lowering_path("record missing field value");
        };
        lowered_fields.push(IrRecordField {
            name: interner.resolve(symbol).into(),
            index: u16::try_from(idx).unwrap_or(u16::MAX),
            expr,
        });
    }
    lowered_fields
}

fn lower_record_update_without_spread(
    ctx: &mut LowerCtx<'_>,
    base: HirExprId,
    update_items: &[HirRecordItem],
    layout: RecordUpdateLayout<'_>,
) -> IrExprKind {
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
            invalid_lowering_path("record update item without name");
        };
        let Some(index) = result_indices.get(&name.name).copied() else {
            invalid_lowering_path("record update field missing from record type");
        };
        updates.push(IrRecordField {
            name: interner.resolve(name.name).into(),
            index,
            expr: lower_expr(ctx, record_item.value),
        });
    }
    IrExprKind::RecordUpdate {
        ty_name: render_ty_name(sema, result_ty, interner),
        field_count: result_count,
        base: Box::new(lower_expr(ctx, base)),
        base_fields,
        result_fields,
        updates: updates.into_boxed_slice(),
    }
}

fn lower_record_update_with_spread(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    update_items: &[HirRecordItem],
    layout: RecordUpdateLayout<'_>,
) -> IrExprKind {
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
            let spread_ty = sema
                .try_expr_ty(record_item.value)
                .expect("expr type missing for record update spread");
            let Some((spread_indices, _spread_fields, _spread_count)) =
                record_layout_for_ty(sema, spread_ty, interner)
            else {
                invalid_lowering_path("record update spread without record type");
            };
            for (symbol, spread_index) in spread_indices {
                let Some(result_index) = result_indices.get(&symbol).copied() else {
                    continue;
                };
                updates.push(IrRecordField {
                    name: interner.resolve(symbol).into(),
                    index: result_index,
                    expr: IrExpr {
                        origin,
                        kind: IrExprKind::RecordGet {
                            base: Box::new(temp_expr.clone()),
                            index: spread_index,
                        },
                    },
                });
            }
            continue;
        }
        let Some(name) = record_item.name else {
            invalid_lowering_path("record update item without name");
        };
        let Some(index) = result_indices.get(&name.name).copied() else {
            invalid_lowering_path("record update field missing from record type");
        };
        updates.push(IrRecordField {
            name: interner.resolve(name.name).into(),
            index,
            expr: temp_expr,
        });
    }
    prelude.push(IrExpr {
        origin,
        kind: IrExprKind::RecordUpdate {
            ty_name: render_ty_name(sema, result_ty, interner),
            field_count: result_count,
            base: Box::new(base_expr),
            base_fields,
            result_fields,
            updates: updates.into_boxed_slice(),
        },
    });
    IrExprKind::Sequence {
        exprs: prelude.into_boxed_slice(),
    }
}
