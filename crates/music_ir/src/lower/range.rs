use super::*;

pub(super) fn lower_range_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    let ty = ctx
        .sema
        .try_expr_ty(expr_id)
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for range"));
    let ty_name = render_ty_name(ctx.sema, ty, ctx.interner);
    let kind = match op {
        HirBinaryOp::Range {
            include_lower,
            include_upper,
        } => IrRangeKind::bounded(*include_lower, *include_upper),
        _ => invalid_lowering_path("invalid range op"),
    };
    IrExprKind::Range {
        ty_name,
        kind,
        lower: lower_boxed_expr(ctx, left),
        upper: lower_boxed_expr(ctx, right),
        bounds_evidence: None,
    }
}

pub(super) fn lower_partial_range_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    kind: HirPartialRangeKind,
    expr: HirExprId,
) -> IrExprKind {
    let ty = ctx
        .sema
        .try_expr_ty(expr_id)
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for partial range"));
    let ty_name = render_ty_name(ctx.sema, ty, ctx.interner);
    let origin = IrOrigin::new(
        ctx.sema.module().store.exprs.get(expr_id).origin.source_id,
        ctx.sema.module().store.exprs.get(expr_id).origin.span,
    );
    let bounds_evidence = ctx
        .sema
        .expr_constraint_answers(expr_id)
        .and_then(|items| items.get(1))
        .map(|item| Box::new(lower_constraint_answer_expr(ctx, origin, item)));
    let bound = lower_boxed_expr(ctx, expr);
    let range_kind = match kind {
        HirPartialRangeKind::From { include_lower } => IrRangeKind::from(include_lower),
        HirPartialRangeKind::UpTo { include_upper } => IrRangeKind::up_to(include_upper),
    };
    IrExprKind::Range {
        ty_name,
        kind: range_kind,
        lower: bound.clone(),
        upper: bound,
        bounds_evidence,
    }
}

pub(super) fn lower_in_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    let origin = IrOrigin::new(
        ctx.sema.module().store.exprs.get(expr_id).origin.source_id,
        ctx.sema.module().store.exprs.get(expr_id).origin.span,
    );
    let evidence = ctx
        .sema
        .expr_constraint_answers(expr_id)
        .and_then(|items| items.first())
        .map_or_else(
            || invalid_lowering_path("range membership evidence missing"),
            |item| lower_constraint_answer_expr(ctx, origin, item),
        );
    IrExprKind::RangeContains {
        value: lower_boxed_expr(ctx, left),
        range: lower_boxed_expr(ctx, right),
        evidence: Box::new(evidence),
    }
}
