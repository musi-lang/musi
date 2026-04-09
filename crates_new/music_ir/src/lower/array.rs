use super::*;

pub(super) fn lower_array_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirArrayItem>,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let array_items = sema.module().store.array_items.get(items);
    if !array_items.iter().any(|array_item| array_item.spread) {
        return IrExprKind::Array {
            ty_name: render_ty_name(sema, sema.expr_ty(expr_id), interner),
            items: array_items
                .iter()
                .map(|array_item| lower_expr(ctx, array_item.expr))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        };
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
        match append_array_spread_parts(sema, origin, array_item.expr, &temp_expr, &mut parts) {
            Ok(runtime_spread) => {
                has_runtime_spread |= runtime_spread;
            }
            Err(kind) => return kind,
        }
    }

    prelude.push(IrExpr {
        origin,
        kind: array_tail_kind(sema, interner, expr_id, has_runtime_spread, parts),
    });
    IrExprKind::Sequence {
        exprs: prelude.into_boxed_slice(),
    }
}

fn expr_origin(sema: &SemaModule, expr_id: HirExprId) -> IrOrigin {
    let origin = sema.module().store.exprs.get(expr_id).origin;
    IrOrigin {
        source_id: origin.source_id,
        span: origin.span,
    }
}

fn lower_item_temp(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    expr_id: HirExprId,
    prelude: &mut Vec<IrExpr>,
) -> IrExpr {
    let temp = fresh_temp(ctx);
    prelude.push(IrExpr {
        origin,
        kind: IrExprKind::TempLet {
            temp,
            value: Box::new(lower_expr(ctx, expr_id)),
        },
    });
    IrExpr {
        origin,
        kind: IrExprKind::Temp { temp },
    }
}

fn append_array_spread_parts(
    sema: &SemaModule,
    origin: IrOrigin,
    spread_expr: HirExprId,
    temp_expr: &IrExpr,
    parts: &mut Vec<IrSeqPart>,
) -> Result<bool, IrExprKind> {
    let spread_ty = sema.expr_ty(spread_expr);
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
        _ => Err(IrExprKind::Unsupported {
            description: "array spread source is not tuple/array".into(),
        }),
    }
}

fn append_array_dim_spread_parts(
    sema: &SemaModule,
    origin: IrOrigin,
    dims: &SliceRange<HirDim>,
    temp_expr: &IrExpr,
    parts: &mut Vec<IrSeqPart>,
) -> Result<bool, IrExprKind> {
    let dims_vec = sema.module().store.dims.get(dims.clone());
    if dims_vec.is_empty() {
        parts.push(IrSeqPart::Spread(temp_expr.clone()));
        return Ok(true);
    }
    if dims_vec.len() != 1 {
        return Err(IrExprKind::Unsupported {
            description: "array spread requires 1D array".into(),
        });
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

fn project_index(origin: IrOrigin, base: IrExpr, index_u32: u32) -> IrExpr {
    IrExpr {
        origin,
        kind: IrExprKind::Index {
            base: Box::new(base),
            index: Box::new(IrExpr {
                origin,
                kind: IrExprKind::Lit(IrLit::Int {
                    raw: index_u32.to_string().into(),
                }),
            }),
        },
    }
}

fn array_tail_kind(
    sema: &SemaModule,
    interner: &Interner,
    expr_id: HirExprId,
    has_runtime_spread: bool,
    parts: Vec<IrSeqPart>,
) -> IrExprKind {
    if has_runtime_spread {
        return IrExprKind::ArrayCat {
            ty_name: render_ty_name(sema, sema.expr_ty(expr_id), interner),
            parts: parts.into_boxed_slice(),
        };
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
        return IrExprKind::Unsupported {
            description: "array spread lowering invariant".into(),
        };
    };
    IrExprKind::Array {
        ty_name: render_ty_name(sema, sema.expr_ty(expr_id), interner),
        items,
    }
}
