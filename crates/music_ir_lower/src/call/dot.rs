use super::{
    ExprMemberKind, HirArg, HirExprId, HirExprKind, Interner, IrArg, IrExpr, IrExprKind, IrSeqPart,
    LowerCtx, ModuleKey, NameBindingId, SemaModule, SpreadMode, Symbol, fresh_temp, lower_expr,
    lower_origin, lower_spread_args,
};

pub(super) struct DotCallableCallTarget {
    pub(super) receiver: HirExprId,
    pub(super) binding: Option<NameBindingId>,
    pub(super) name: Symbol,
    pub(super) import_record_target: Option<ModuleKey>,
}

pub(super) fn lower_dot_callable_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    arg_nodes: &[HirArg],
    dot_callable: &DotCallableCallTarget,
    interner: &Interner,
) -> Result<IrExprKind, Box<str>> {
    let origin = lower_origin(ctx.sema, callee);
    let callee_expr = IrExpr::new(
        origin,
        IrExprKind::Name {
            binding: dot_callable.binding,
            name: interner.resolve(dot_callable.name).into(),
            import_record_target: dot_callable.import_record_target.clone(),
        },
    );

    if !arg_nodes.iter().any(|arg| arg.spread) {
        let mut lowered_args = Vec::with_capacity(arg_nodes.len().saturating_add(1));
        lowered_args.push(IrArg::new(false, lower_expr(ctx, dot_callable.receiver)));
        lowered_args.extend(
            arg_nodes
                .iter()
                .map(|arg| IrArg::new(false, lower_expr(ctx, arg.expr))),
        );
        return Ok(IrExprKind::Call {
            callee: Box::new(callee_expr),
            args: lowered_args.into_boxed_slice(),
        });
    }

    let mut prelude = Vec::<IrExpr>::new();
    let receiver_temp = fresh_temp(ctx);
    prelude.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp: receiver_temp,
            value: Box::new(lower_expr(ctx, dot_callable.receiver)),
        },
    ));

    let (arg_prelude, arg_parts, has_runtime_spread) =
        lower_spread_args(ctx, origin, arg_nodes, SpreadMode::Call)?;
    prelude.extend(arg_prelude);

    let mut parts = Vec::with_capacity(arg_parts.len().saturating_add(1));
    parts.push(IrSeqPart::Expr(IrExpr::new(
        origin,
        IrExprKind::Temp {
            temp: receiver_temp,
        },
    )));
    parts.extend(arg_parts);

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
                    "dot-callable spread lowering invariant",
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

pub(super) fn resolve_dot_callable_call_target(
    sema: &SemaModule,
    callee: HirExprId,
) -> Option<DotCallableCallTarget> {
    let HirExprKind::Field { base, .. } = sema.module().store.exprs.get(callee).kind else {
        return None;
    };
    let fact = sema.expr_member_fact(callee)?;
    if !is_dot_callable_member(fact.kind) {
        return None;
    }

    Some(DotCallableCallTarget {
        receiver: base,
        binding: fact.binding,
        name: fact.name,
        import_record_target: fact
            .import_record_target
            .clone()
            .or_else(|| sema.expr_import_record_target(callee).cloned()),
    })
}

const fn is_dot_callable_member(kind: ExprMemberKind) -> bool {
    matches!(
        kind,
        ExprMemberKind::DotCallable | ExprMemberKind::AttachedMethod
    )
}
