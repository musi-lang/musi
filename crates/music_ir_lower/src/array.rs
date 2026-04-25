use super::{
    HirArrayItem, HirDim, HirExprId, HirTyId, HirTyKind, Interner, IrExpr, IrExprKind, IrLit,
    IrOrigin, IrSeqPart, LowerCtx, SemaModule, SliceRange, fresh_temp, lower_expr,
    lowering_invariant_violation, render_ty_name,
};

pub(crate) fn lower_array_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirArrayItem>,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let array_items = sema.module().store.array_items.get(items);
    if !array_items.iter().any(|array_item| array_item.spread) {
        return Ok(IrExprKind::Array {
            ty_name: render_ty_name(
                sema,
                sema.try_expr_ty(expr_id).unwrap_or_else(|| {
                    lowering_invariant_violation("expr type missing for array literal")
                }),
                interner,
            ),
            items: array_items
                .iter()
                .map(|array_item| lower_expr(ctx, array_item.expr))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        });
    }

    let origin = expr_origin(sema, expr_id);
    let mut prelude = Vec::<IrExpr>::new();
    let mut parts = Vec::<IrSeqPart>::new();
    let mut has_runtime_spread = false;
    for array_item in array_items {
        let temp_expr = lower_item_temp(ctx, origin, array_item.expr, &mut prelude);
        if !array_item.spread {
            parts.push(IrSeqPart::Expr(temp_expr));
            continue;
        }
        has_runtime_spread |=
            append_array_spread_parts(ctx, origin, array_item.expr, &temp_expr, &mut parts)?;
    }

    prelude.push(IrExpr::new(
        origin,
        array_tail_kind(sema, interner, expr_id, has_runtime_spread, parts)?,
    ));
    Ok(IrExprKind::Sequence {
        exprs: prelude.into_boxed_slice(),
    })
}

pub(crate) fn expr_origin(sema: &SemaModule, expr_id: HirExprId) -> IrOrigin {
    let origin = sema.module().store.exprs.get(expr_id).origin;
    IrOrigin::new(origin.source_id, origin.span)
}

pub(crate) fn lower_item_temp(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    expr_id: HirExprId,
    prelude: &mut Vec<IrExpr>,
) -> IrExpr {
    let temp_id = fresh_temp(ctx);
    prelude.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp: temp_id,
            value: Box::new(lower_expr(ctx, expr_id)),
        },
    ));
    IrExpr::new(origin, IrExprKind::Temp { temp: temp_id })
}

pub(crate) fn append_array_spread_parts(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    spread_expr: HirExprId,
    temp_expr: &IrExpr,
    parts: &mut Vec<IrSeqPart>,
) -> Result<bool, Box<str>> {
    let sema = ctx.sema;
    let spread_ty = sema
        .try_expr_ty(spread_expr)
        .unwrap_or_else(|| lowering_invariant_violation("expr type missing for array spread"));
    match &sema.ty(spread_ty).kind {
        HirTyKind::Tuple { items } => {
            for (index, _) in sema.module().store.ty_ids.get(*items).iter().enumerate() {
                let Ok(index_u32) = u32::try_from(index) else {
                    continue;
                };
                parts.push(IrSeqPart::Expr(project_index(
                    origin,
                    temp_expr.clone(),
                    index_u32,
                )));
            }
            Ok(false)
        }
        HirTyKind::Array { dims, .. } => {
            append_array_dim_spread_parts(sema, origin, dims, temp_expr, parts)
        }
        HirTyKind::Seq { .. } => {
            parts.push(IrSeqPart::Spread(temp_expr.clone()));
            Ok(true)
        }
        HirTyKind::Range { bound } => {
            let result_ty_name = range_sequence_type_name(sema, *bound, ctx.interner);
            let constraint_answer = sema
                .expr_constraint_answers(spread_expr)
                .and_then(|items| items.first())
                .map(|item| super::lower_constraint_answer_expr(ctx, origin, item));
            let Some(constraint_answer) = constraint_answer else {
                return Err(super::lower_errors::lowering_error(
                    "range spread evidence missing",
                ));
            };
            parts.push(IrSeqPart::Spread(IrExpr::new(
                origin,
                IrExprKind::RangeMaterialize {
                    range: Box::new(temp_expr.clone()),
                    evidence: Box::new(constraint_answer),
                    result_ty_name,
                },
            )));
            Ok(true)
        }
        _ => Err(super::lower_errors::lowering_error(
            "array spread source is not tuple/array",
        )),
    }
}

pub(crate) fn range_sequence_type_name(
    sema: &SemaModule,
    item: HirTyId,
    interner: &Interner,
) -> Box<str> {
    format!("[]{}", render_ty_name(sema, item, interner)).into()
}

pub(crate) fn append_array_dim_spread_parts(
    sema: &SemaModule,
    origin: IrOrigin,
    dims: &SliceRange<HirDim>,
    temp_expr: &IrExpr,
    parts: &mut Vec<IrSeqPart>,
) -> Result<bool, Box<str>> {
    let dims_vec = sema.module().store.dims.get(dims.clone());
    if dims_vec.is_empty() {
        parts.push(IrSeqPart::Spread(temp_expr.clone()));
        return Ok(true);
    }
    if dims_vec.len() != 1 {
        return Err(super::lower_errors::lowering_error(
            "array spread needs 1D array",
        ));
    }
    match dims_vec[0] {
        HirDim::Int(len) => {
            for index_u32 in 0..len {
                parts.push(IrSeqPart::Expr(project_index(
                    origin,
                    temp_expr.clone(),
                    index_u32,
                )));
            }
            Ok(false)
        }
        HirDim::Unknown | HirDim::Name(_) => {
            parts.push(IrSeqPart::Spread(temp_expr.clone()));
            Ok(true)
        }
    }
}

pub(crate) fn project_index(origin: IrOrigin, base: IrExpr, index_u32: u32) -> IrExpr {
    IrExpr::new(
        origin,
        IrExprKind::Index {
            base: Box::new(base),
            indices: vec![IrExpr::new(
                origin,
                IrExprKind::Lit(IrLit::Int {
                    raw: index_u32.to_string().into(),
                }),
            )]
            .into_boxed_slice(),
        },
    )
}

pub(crate) fn array_tail_kind(
    sema: &SemaModule,
    interner: &Interner,
    expr_id: HirExprId,
    has_runtime_spread: bool,
    parts: Vec<IrSeqPart>,
) -> Result<IrExprKind, Box<str>> {
    if has_runtime_spread {
        return Ok(IrExprKind::ArrayCat {
            ty_name: render_ty_name(
                sema,
                sema.try_expr_ty(expr_id).unwrap_or_else(|| {
                    lowering_invariant_violation("expr type missing for array cat")
                }),
                interner,
            ),
            parts: parts.into_boxed_slice(),
        });
    }
    let items = parts
        .into_iter()
        .map(|part| match part {
            IrSeqPart::Expr(expr) => Some(expr),
            IrSeqPart::Spread(_) => None,
        })
        .collect::<Option<Vec<_>>>()
        .map(Vec::into_boxed_slice);
    let Some(items) = items else {
        return Err(super::lower_errors::lowering_error(
            "array spread lowering invariant",
        ));
    };
    Ok(IrExprKind::Array {
        ty_name: render_ty_name(
            sema,
            sema.try_expr_ty(expr_id).unwrap_or_else(|| {
                lowering_invariant_violation("expr type missing for array literal")
            }),
            interner,
        ),
        items,
    })
}
