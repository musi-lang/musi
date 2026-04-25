use super::{
    ComptimeValue, DefinitionKey, ExprMemberKind, HirArg, HirDim, HirExprId, HirExprKind, HirParam,
    HirParamRange, HirPatKind, HirTyId, HirTyKind, Ident, Interner, IrArg, IrCallable, IrExpr,
    IrExprKind, IrIntrinsicKind, IrLit, IrOrigin, IrParam, IrSeqPart, LowerCtx, ModuleKey,
    NameBindingId, NameSite, SemaModule, SliceRange, Symbol, decl_binding_id, fresh_temp,
    hidden_constraint_answer_params_for_binding, lower_constraint_answer_expr, lower_errors,
    lower_expr, lowering_invariant_violation, pop_constraint_answer_bindings,
    push_constraint_answer_bindings, render_ty_name, render_type_value_expr_name, toplevel,
    use_binding_id,
};

mod args;
mod comptime;
mod dot;
mod intrinsics;
mod request;

use args::{
    SpreadMode, lower_origin, lower_spread_args, ordered_call_args, resolve_request_target,
};
use comptime::lower_comptime_call_expr;
use dot::{lower_dot_callable_call_expr, resolve_dot_callable_call_target};
use intrinsics::{lower_ffi_pointer_intrinsic, lower_std_cmp_intrinsic};
pub(crate) fn lower_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &SliceRange<HirArg>,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let arg_nodes = ordered_call_args(
        sema,
        interner,
        callee,
        sema.module().store.args.get(args.clone()),
    );
    if let Some(intrinsic) = lower_std_cmp_intrinsic(ctx, callee, &arg_nodes) {
        return intrinsic;
    }
    if let Some(intrinsic) = lower_ffi_pointer_intrinsic(ctx, callee, &arg_nodes) {
        return intrinsic;
    }
    if let Some(dot_callable) = resolve_dot_callable_call_target(sema, callee) {
        return lower_dot_callable_call_expr(ctx, callee, &arg_nodes, &dot_callable, interner);
    }

    if !arg_nodes.iter().any(|arg| arg.spread) {
        if let Some(specialized) = lower_comptime_call_expr(ctx, callee, &arg_nodes)? {
            return Ok(specialized);
        }
        return Ok(IrExprKind::Call {
            callee: Box::new(lower_expr(ctx, callee)),
            args: arg_nodes
                .iter()
                .map(|arg| IrArg::new(false, lower_expr(ctx, arg.expr)))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        });
    }

    let origin = lower_origin(sema, callee);
    let mut prelude = Vec::<IrExpr>::new();
    let callee_temp = fresh_temp(ctx);
    prelude.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp: callee_temp,
            value: Box::new(lower_expr(ctx, callee)),
        },
    ));
    let callee_expr = IrExpr::new(origin, IrExprKind::Temp { temp: callee_temp });

    let (arg_prelude, parts, has_runtime_spread) =
        lower_spread_args(ctx, origin, &arg_nodes, SpreadMode::Call)?;
    prelude.extend(arg_prelude);

    prelude.push(IrExpr::new(
        origin,
        if has_runtime_spread {
            IrExprKind::CallParts {
                callee: Box::new(callee_expr),
                args: parts.into_boxed_slice(),
            }
        } else {
            let args = parts
                .into_iter()
                .map(|part| match part {
                    IrSeqPart::Expr(expr) => Some(IrArg::new(false, expr)),
                    IrSeqPart::Spread(_) => None,
                })
                .collect::<Option<Vec<_>>>()
                .map(Vec::into_boxed_slice);
            let Some(args) = args else {
                return Err(super::lower_errors::lowering_error(
                    "call spread lowering invariant",
                ));
            };
            IrExprKind::Call {
                callee: Box::new(callee_expr),
                args,
            }
        },
    ));

    Ok(IrExprKind::Sequence {
        exprs: prelude.into_boxed_slice(),
    })
}

pub(crate) fn lower_request_expr(
    ctx: &mut LowerCtx<'_>,
    expr: HirExprId,
) -> Result<IrExprKind, Box<str>> {
    request::lower_request_expr(ctx, expr)
}
