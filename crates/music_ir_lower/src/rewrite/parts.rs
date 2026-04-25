use super::super::{
    IrArg, IrAssignTarget, IrExpr, IrExprKind, IrHandleOp, IrLoweredMatchArm, IrOrigin,
    IrRecordField, IrSeqPart, IrTempId, LowerCtx, LoweredMatchArmList, NameBindingId,
    rewrite_recursive_binding_refs,
};

pub(super) fn rewrite_seq_parts(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    parts: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrSeqPart]> {
    parts
        .into_vec()
        .into_iter()
        .map(|part| match part {
            IrSeqPart::Expr(expr) => IrSeqPart::Expr(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            )),
            IrSeqPart::Spread(expr) => IrSeqPart::Spread(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            )),
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn rewrite_sequence_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    exprs: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Sequence {
        exprs: exprs
            .into_vec()
            .into_iter()
            .map(|item| {
                rewrite_recursive_binding_refs(ctx, origin, item, binding, callable_name, captures)
            })
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

pub(super) fn rewrite_tuple_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    items: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Tuple {
        ty_name,
        items: rewrite_expr_slice(ctx, origin, items, binding, callable_name, captures),
    }
}

pub(super) fn rewrite_array_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    items: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Array {
        ty_name,
        items: rewrite_expr_slice(ctx, origin, items, binding, callable_name, captures),
    }
}

pub(super) fn rewrite_array_cat_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ty_name: Box<str>,
    parts: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::ArrayCat {
        ty_name,
        parts: rewrite_seq_parts(ctx, origin, parts, binding, callable_name, captures),
    }
}

pub(super) fn rewrite_record_get_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    index: u16,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::RecordGet {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        index,
    }
}

pub(super) fn rewrite_temp_let_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    temp: IrTempId,
    value: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TempLet {
        temp,
        value: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            value,
            binding,
            callable_name,
            captures,
        )),
    }
}

pub(super) fn rewrite_assign_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    target: IrAssignTarget,
    value: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Assign {
        target: Box::new(rewrite_assign_target(
            ctx,
            origin,
            target,
            binding,
            callable_name,
            captures,
        )),
        value: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            value,
            binding,
            callable_name,
            captures,
        )),
    }
}

pub(super) fn rewrite_index_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    indices: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Index {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        indices: rewrite_expr_slice(ctx, origin, indices, binding, callable_name, captures),
    }
}

pub(super) fn rewrite_module_load_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    spec: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::ModuleLoad {
        spec: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            spec,
            binding,
            callable_name,
            captures,
        )),
    }
}

pub(super) fn rewrite_module_get_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    name: Box<str>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::ModuleGet {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        name,
    }
}

pub(super) fn rewrite_not_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: IrExpr,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Not {
        expr: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            expr,
            binding,
            callable_name,
            captures,
        )),
    }
}

pub(super) fn rewrite_ty_test_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    ty_name: Box<str>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TyTest {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        ty_name,
    }
}

pub(super) fn rewrite_ty_cast_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    ty_name: Box<str>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::TyCast {
        base: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            base,
            binding,
            callable_name,
            captures,
        )),
        ty_name,
    }
}

pub(super) fn rewrite_case_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    scrutinee: IrExpr,
    arms: LoweredMatchArmList,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Match {
        scrutinee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            scrutinee,
            binding,
            callable_name,
            captures,
        )),
        arms: rewrite_match_arms(ctx, origin, arms, binding, callable_name, captures),
    }
}

pub(super) fn rewrite_call_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    callee: IrExpr,
    args: Box<[IrArg]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Call {
        callee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            callee,
            binding,
            callable_name,
            captures,
        )),
        args: rewrite_call_args(ctx, origin, args, binding, callable_name, captures),
    }
}

pub(super) fn rewrite_call_parts_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    callee: IrExpr,
    args: Box<[IrSeqPart]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::CallParts {
        callee: Box::new(rewrite_recursive_binding_refs(
            ctx,
            origin,
            callee,
            binding,
            callable_name,
            captures,
        )),
        args: rewrite_seq_parts(ctx, origin, args, binding, callable_name, captures),
    }
}

pub(super) fn rewrite_resume_kind(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    expr: Option<IrExpr>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrExprKind {
    IrExprKind::Resume {
        expr: expr.map(|expr| {
            Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                expr,
                binding,
                callable_name,
                captures,
            ))
        }),
    }
}

pub(super) fn rewrite_record_fields(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    fields: Box<[IrRecordField]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrRecordField]> {
    fields
        .into_vec()
        .into_iter()
        .map(|field| {
            IrRecordField::new(
                field.name,
                field.index,
                rewrite_recursive_binding_refs(
                    ctx,
                    origin,
                    field.expr,
                    binding,
                    callable_name,
                    captures,
                ),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn rewrite_call_args(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    args: Box<[IrArg]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrArg]> {
    args.into_vec()
        .into_iter()
        .map(|arg| {
            IrArg::new(
                arg.spread,
                rewrite_recursive_binding_refs(
                    ctx,
                    origin,
                    arg.expr,
                    binding,
                    callable_name,
                    captures,
                ),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn rewrite_match_arms(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    arms: LoweredMatchArmList,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> LoweredMatchArmList {
    arms.into_vec()
        .into_iter()
        .map(|arm| IrLoweredMatchArm {
            guard: arm.guard.map(|guard| {
                rewrite_recursive_binding_refs(ctx, origin, guard, binding, callable_name, captures)
            }),
            expr: rewrite_recursive_binding_refs(
                ctx,
                origin,
                arm.expr,
                binding,
                callable_name,
                captures,
            ),
            ..arm
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn rewrite_handle_ops(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    ops: Box<[IrHandleOp]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrHandleOp]> {
    ops.into_vec()
        .into_iter()
        .map(|op| {
            IrHandleOp::new(
                op.op_index,
                op.name,
                rewrite_recursive_binding_refs(
                    ctx,
                    origin,
                    op.closure,
                    binding,
                    callable_name,
                    captures,
                ),
            )
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn rewrite_expr_slice(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    exprs: Box<[IrExpr]>,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> Box<[IrExpr]> {
    exprs
        .into_vec()
        .into_iter()
        .map(|expr| {
            rewrite_recursive_binding_refs(ctx, origin, expr, binding, callable_name, captures)
        })
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(super) fn rewrite_assign_target(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    target: IrAssignTarget,
    binding: NameBindingId,
    callable_name: &str,
    captures: &[NameBindingId],
) -> IrAssignTarget {
    match target {
        IrAssignTarget::Binding {
            binding,
            name,
            import_record_target,
        } => IrAssignTarget::Binding {
            binding,
            name,
            import_record_target,
        },
        IrAssignTarget::Index { base, indices } => IrAssignTarget::Index {
            base: Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                *base,
                binding,
                callable_name,
                captures,
            )),
            indices: rewrite_expr_slice(ctx, origin, indices, binding, callable_name, captures),
        },
        IrAssignTarget::RecordField { base, index } => IrAssignTarget::RecordField {
            base: Box::new(rewrite_recursive_binding_refs(
                ctx,
                origin,
                *base,
                binding,
                callable_name,
                captures,
            )),
            index,
        },
    }
}
