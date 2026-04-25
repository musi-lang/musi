use super::{
    HirExprId, IrExpr, IrExprKind, IrSeqPart, LowerCtx, SpreadMode, lower_expr, lower_origin,
    lower_spread_args, ordered_call_args, resolve_request_target,
};

pub(super) fn lower_request_expr(
    ctx: &mut LowerCtx<'_>,
    expr: HirExprId,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let (effect_key, op_index, callee, args) = resolve_request_target(sema, interner, expr)?;
    let args_nodes = ordered_call_args(sema, interner, callee, sema.module().store.args.get(args));
    if !args_nodes.iter().any(|arg| arg.spread) {
        let lowered_args = args_nodes
            .iter()
            .map(|arg| lower_expr(ctx, arg.expr))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        return Ok(IrExprKind::Request {
            effect_key,
            op_index,
            args: lowered_args,
        });
    }

    let origin = lower_origin(sema, expr);
    let (prelude, parts, has_runtime_spread) =
        lower_spread_args(ctx, origin, &args_nodes, SpreadMode::Request)?;
    let mut exprs = prelude;
    exprs.push(IrExpr::new(
        origin,
        if has_runtime_spread {
            IrExprKind::RequestSeq {
                effect_key,
                op_index,
                args: parts.into_boxed_slice(),
            }
        } else {
            let args = parts
                .into_iter()
                .map(|part| match part {
                    IrSeqPart::Expr(expr) => Some(expr),
                    IrSeqPart::Spread(_) => None,
                })
                .collect::<Option<Vec<_>>>()
                .map(Vec::into_boxed_slice);
            let Some(args) = args else {
                return Err(super::lower_errors::lowering_error(
                    "ask spread lowering invariant",
                ));
            };
            IrExprKind::Request {
                effect_key,
                op_index,
                args,
            }
        },
    ));
    Ok(IrExprKind::Sequence {
        exprs: exprs.into_boxed_slice(),
    })
}
