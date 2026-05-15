use super::pats::lower_case_patterns;
use super::{
    HirArg, HirArrayItem, HirBinaryOp, HirExprId, HirExprKind, HirLetMods, HirLitId, HirLitKind,
    HirParamRange, HirPatId, HirPatKind, HirPrefixOp, HirRecordItemRange, HirTemplatePart,
    HirTyKind, Ident, IrBinaryOp, IrCasePattern, IrExpr, IrExprKind, IrLit, IrLoweredMatchArm,
    IrOrigin, IrTempId, LowerCtx, LoweringResult, NameBindingId, NameSite, SemaModule, SliceRange,
    array, assign, binary, bind_expr_constraint_evidence, call, destructure, is_type_value_expr,
    lower_comptime_value, lower_field_expr, lower_in_expr, lower_index_expr, lower_lambda_expr,
    lower_local_callable_let, lower_match_expr, lower_partial_range_expr, lower_range_expr,
    lower_variant_expr, lowering_invariant_violation, record, render_ty_name,
    render_type_value_expr_name,
};
use music_hir::HirExpr;

pub(crate) fn lower_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId) -> IrExpr {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let expr = sema.module().store.exprs.get(expr_id);
    let origin = IrOrigin::new(expr.origin.source_id, expr.origin.span);
    if is_type_value_expr(sema, expr_id, interner) {
        return IrExpr::new(
            origin,
            IrExprKind::TypeValue {
                ty_name: render_type_value_expr_name(sema, expr_id, interner),
            },
        );
    }
    if let HirExprKind::Apply { callee, args } = &expr.kind {
        let type_args = ctx
            .sema
            .module()
            .store
            .expr_ids
            .get(*args)
            .iter()
            .copied()
            .map(|arg| render_type_value_expr_name(ctx.sema, arg, ctx.interner))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        let lowered = IrExpr::new(
            origin,
            IrExprKind::TypeApply {
                callee: Box::new(lower_expr(ctx, *callee)),
                type_args,
            },
        );
        return bind_expr_constraint_evidence(ctx, expr_id, origin, lowered);
    }
    if let HirExprKind::Unsafe { body } = &expr.kind {
        let lowered = lower_expr_with_origin(ctx, *body, origin);
        return bind_expr_constraint_evidence(ctx, expr_id, origin, lowered);
    }
    if let HirExprKind::Pin { value, name, body } = &expr.kind {
        let lowered = lower_pin_expr(ctx, origin, *value, *name, *body);
        return bind_expr_constraint_evidence(ctx, expr_id, origin, lowered);
    }
    let kind = match &expr.kind {
        HirExprKind::Name { .. }
        | HirExprKind::Lit { .. }
        | HirExprKind::Sequence { .. }
        | HirExprKind::Tuple { .. }
        | HirExprKind::Array { .. }
        | HirExprKind::Template { .. }
        | HirExprKind::Record { .. } => lower_value_expr(ctx, expr_id, origin, &expr.kind),
        HirExprKind::Let { .. }
        | HirExprKind::Binary { .. }
        | HirExprKind::PartialRange { .. }
        | HirExprKind::Call { .. }
        | HirExprKind::Prefix { .. }
        | HirExprKind::Index { .. }
        | HirExprKind::Field { .. }
        | HirExprKind::RecordUpdate { .. } => {
            lower_operation_expr(ctx, expr_id, origin, &expr.kind)
        }
        HirExprKind::Match { .. }
        | HirExprKind::If { .. }
        | HirExprKind::Variant { .. }
        | HirExprKind::Lambda { .. } => lower_control_expr(ctx, expr_id, origin, &expr.kind),
        HirExprKind::TypeTest { .. }
        | HirExprKind::TypeCast { .. }
        | HirExprKind::Import { .. } => lower_misc_expr(ctx, expr_id, &expr.kind),
        other => lowering_invariant_violation(format!("missing IR lowering for {other:?}")),
    };
    bind_expr_constraint_evidence(ctx, expr_id, origin, IrExpr::new(origin, kind))
}

pub(crate) fn lower_value_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    kind: &HirExprKind,
) -> IrExprKind {
    match kind {
        HirExprKind::Name { name } => lower_name_expr(ctx, expr_id, *name),
        HirExprKind::Lit { lit } => lower_lit_expr(ctx.sema, *lit),
        HirExprKind::Sequence { exprs } => lower_sequence_expr(ctx, *exprs),
        HirExprKind::Tuple { items } => lower_tuple_expr(ctx, expr_id, *items),
        HirExprKind::Array { items } => lower_array_expr(ctx, expr_id, items.clone())
            .unwrap_or_else(|description| lowering_invariant_violation(description)),
        HirExprKind::Template { parts } => lower_template_expr(ctx, origin, parts.clone()),
        HirExprKind::Record { items } => lower_record_expr(ctx, expr_id, items.clone())
            .unwrap_or_else(|description| lowering_invariant_violation(description)),
        _ => lowering_invariant_violation("value expr dispatcher mismatch"),
    }
}

pub(crate) fn lower_operation_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    kind: &HirExprKind,
) -> IrExprKind {
    match kind {
        HirExprKind::Let {
            mods,
            pat,
            has_param_clause,
            params,
            value,
            ..
        } => lower_let_expr(ctx, origin, *mods, *pat, *has_param_clause, params, *value),
        HirExprKind::Binary { op, left, right } => {
            lower_binary_expr(ctx, expr_id, op, *left, *right)
        }
        HirExprKind::PartialRange { kind, expr } => {
            lower_partial_range_expr(ctx, expr_id, *kind, *expr)
        }
        HirExprKind::Call { callee, args } => lower_call_expr(ctx, *callee, args)
            .unwrap_or_else(|description| lowering_invariant_violation(description)),
        HirExprKind::Prefix { op, expr } => lower_prefix_expr(ctx, expr_id, op, *expr, origin),
        HirExprKind::Index { base, args } => lower_index_expr(ctx, *base, *args),
        HirExprKind::Field { base, name, .. } => {
            lower_field_expr(ctx, expr_id, *base, name.name, kind)
        }
        HirExprKind::RecordUpdate { base, items } => {
            lower_record_update_expr(ctx, expr_id, *base, items.clone())
                .unwrap_or_else(|description| lowering_invariant_violation(description))
        }
        _ => lowering_invariant_violation("operation expr dispatcher mismatch"),
    }
}

pub(crate) fn lower_control_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    kind: &HirExprKind,
) -> IrExprKind {
    match kind {
        HirExprKind::Match { scrutinee, arms } => lower_match_expr(ctx, *scrutinee, arms),
        HirExprKind::If {
            condition,
            then_expr,
            else_expr,
        } => IrExprKind::If {
            condition: lower_boxed_expr(ctx, *condition),
            then_expr: lower_boxed_expr(ctx, *then_expr),
            else_expr: lower_boxed_expr(ctx, *else_expr),
        },
        HirExprKind::Variant { tag, args } => lower_variant_expr(ctx, expr_id, *tag, args.clone()),
        HirExprKind::Lambda { params, body, .. } => lower_lambda_expr(ctx, origin, params, *body),
        _ => lowering_invariant_violation("control expr dispatcher mismatch"),
    }
}

pub(crate) fn lower_misc_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    kind: &HirExprKind,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    match kind {
        HirExprKind::TypeTest { base, .. } => {
            let ty_name = sema.type_test_target(expr_id).map_or_else(
                || Box::<str>::from("Unknown"),
                |target| render_ty_name(sema, target, interner),
            );
            IrExprKind::TyTest {
                base: lower_boxed_expr(ctx, *base),
                ty_name,
            }
        }
        HirExprKind::TypeCast { base, .. } => {
            let target = sema.type_test_target(expr_id).unwrap_or_else(|| {
                sema.try_expr_ty(expr_id).unwrap_or_else(|| {
                    lowering_invariant_violation("expr type missing for type cast")
                })
            });
            IrExprKind::TyCast {
                base: lower_boxed_expr(ctx, *base),
                ty_name: render_ty_name(sema, target, interner),
            }
        }
        HirExprKind::Import { arg } => {
            if sema.expr_import_record_target(expr_id).is_some() {
                IrExprKind::Unit
            } else if let Some(items) = import_tuple_items(sema, *arg) {
                let items = items.to_vec();
                IrExprKind::Tuple {
                    ty_name: render_ty_name(
                        sema,
                        sema.try_expr_ty(expr_id).unwrap_or_else(|| {
                            lowering_invariant_violation("expr type missing for import tuple")
                        }),
                        interner,
                    ),
                    items: items
                        .into_iter()
                        .map(|item| lower_import_arg_expr(ctx, item))
                        .collect::<Vec<_>>()
                        .into_boxed_slice(),
                }
            } else {
                IrExprKind::ModuleLoad {
                    spec: lower_boxed_expr(ctx, *arg),
                }
            }
        }
        _ => lowering_invariant_violation("misc expr dispatcher mismatch"),
    }
}

fn import_tuple_items(sema: &SemaModule, arg: HirExprId) -> Option<&[HirExprId]> {
    match sema.module().store.exprs.get(arg).kind {
        HirExprKind::Tuple { items } | HirExprKind::Sequence { exprs: items } => {
            Some(sema.module().store.expr_ids.get(items))
        }
        _ => None,
    }
}

fn lower_import_arg_expr(ctx: &mut LowerCtx<'_>, arg: HirExprId) -> IrExpr {
    let sema = ctx.sema;
    let origin = sema.module().store.exprs.get(arg).origin;
    let origin = IrOrigin::new(origin.source_id, origin.span);
    if sema
        .resolved()
        .imports
        .iter()
        .any(|import| import.span == origin.span)
    {
        IrExpr::new(origin, IrExprKind::Unit)
    } else {
        IrExpr::new(
            origin,
            IrExprKind::ModuleLoad {
                spec: Box::new(lower_expr(ctx, arg)),
            },
        )
    }
}

pub(crate) fn lower_prefix_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    op: &HirPrefixOp,
    expr: HirExprId,
    origin: IrOrigin,
) -> IrExprKind {
    match op {
        HirPrefixOp::Known => lower_comptime_prefix_expr(ctx, expr_id, expr, origin),
        HirPrefixOp::Mut => lower_expr_with_origin(ctx, expr, origin).kind,
        HirPrefixOp::Not => IrExprKind::Not {
            expr: lower_boxed_expr(ctx, expr),
        },
        HirPrefixOp::Neg => {
            let ty = ctx
                .sema
                .try_expr_ty(expr_id)
                .unwrap_or_else(|| lowering_invariant_violation("expr type missing for prefix op"));
            let (zero, op) = match &ctx.sema.ty(ty).kind {
                HirTyKind::Float | HirTyKind::Float32 | HirTyKind::Float64 => (
                    IrExpr::new(origin, IrExprKind::Lit(IrLit::Float { raw: "0.0".into() })),
                    IrBinaryOp::FSub,
                ),
                HirTyKind::Int
                | HirTyKind::Nat
                | HirTyKind::Int8
                | HirTyKind::Int16
                | HirTyKind::Int32
                | HirTyKind::Int64
                | HirTyKind::Nat8
                | HirTyKind::Nat16
                | HirTyKind::Nat32
                | HirTyKind::Nat64
                | HirTyKind::NatLit(_) => (
                    IrExpr::new(origin, IrExprKind::Lit(IrLit::Int { raw: "0".into() })),
                    IrBinaryOp::ISub,
                ),
                other => {
                    lowering_invariant_violation(format!("invalid neg operand type {other:?}"))
                }
            };
            let expr = lower_expr(ctx, expr);
            IrExprKind::Binary {
                op,
                left: Box::new(zero),
                right: Box::new(expr),
            }
        }
    }
}

pub(crate) fn lower_comptime_prefix_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    expr: HirExprId,
    origin: IrOrigin,
) -> IrExprKind {
    if let Some(value) = ctx.sema.expr_comptime_value(expr_id) {
        return lower_comptime_value(ctx, value);
    }
    lower_expr_with_origin(ctx, expr, origin).kind
}

pub(crate) fn lower_expr_with_origin(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
) -> IrExpr {
    let mut lowered = lower_expr(ctx, expr_id);
    lowered.origin = origin;
    lowered
}

pub(crate) fn lower_boxed_expr(ctx: &mut LowerCtx<'_>, expr_id: HirExprId) -> Box<IrExpr> {
    Box::new(lower_expr(ctx, expr_id))
}

pub(crate) fn lower_template_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    parts: SliceRange<HirTemplatePart>,
) -> IrExprKind {
    let mut rendered = Vec::<IrExpr>::new();
    for part in ctx.sema.module().store.template_parts.get(parts) {
        match part {
            HirTemplatePart::Text { value } => rendered.push(IrExpr::new(
                origin,
                IrExprKind::Lit(IrLit::String {
                    value: value.clone(),
                }),
            )),
            HirTemplatePart::Expr { expr } => rendered.push(lower_expr(ctx, *expr)),
        }
    }
    let mut iter = rendered.into_iter();
    let Some(mut acc) = iter.next() else {
        return IrExprKind::Lit(IrLit::String { value: "".into() });
    };
    for expr in iter {
        acc = IrExpr::new(
            origin,
            IrExprKind::Binary {
                op: IrBinaryOp::StrCat,
                left: Box::new(acc),
                right: Box::new(expr),
            },
        );
    }
    acc.kind
}

pub(crate) fn lower_name_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    ident: Ident,
) -> IrExprKind {
    let sema = ctx.sema;
    if let Some(binding) = use_binding_id(sema, ident)
        && let Some(value) = ctx.comptime_bindings.get(&binding).cloned()
    {
        return lower_comptime_value(ctx, &value);
    }
    let binding = use_binding_id(sema, ident);
    IrExprKind::Name {
        binding,
        name: ctx.interner.resolve(ident.name).into(),
        import_record_target: sema
            .expr_import_record_target(expr_id)
            .cloned()
            .or_else(|| {
                binding.and_then(|binding| sema.binding_import_record_target(binding).cloned())
            }),
    }
}

pub(crate) fn lower_lit_expr(sema: &SemaModule, lit_id: HirLitId) -> IrExprKind {
    let lit = &sema.module().store.lits.get(lit_id).kind;
    IrExprKind::Lit(match lit {
        HirLitKind::Int { raw } => IrLit::Int { raw: raw.clone() },
        HirLitKind::Float { raw } => IrLit::Float { raw: raw.clone() },
        HirLitKind::String { value } => IrLit::String {
            value: value.clone(),
        },
        HirLitKind::Rune { value } => IrLit::Rune { value: *value },
    })
}

pub(crate) fn lower_sequence_expr(
    ctx: &mut LowerCtx<'_>,
    exprs: SliceRange<HirExprId>,
) -> IrExprKind {
    lower_sequence_expr_ids(ctx, ctx.sema.module().store.expr_ids.get(exprs))
}

fn lower_sequence_expr_ids(ctx: &mut LowerCtx<'_>, expr_ids: &[HirExprId]) -> IrExprKind {
    let sema = ctx.sema;
    let mut lowered_exprs = Vec::<IrExpr>::new();
    let mut deferred_cleanups = Vec::<DeferredCleanup>::new();
    for (index, expr_id) in expr_ids.iter().copied().enumerate() {
        let expr = sema.module().store.exprs.get(expr_id);
        if let HirExprKind::Defer { cleanup, guard } = expr.kind {
            let origin = IrOrigin::new(expr.origin.source_id, expr.origin.span);
            lowered_exprs.push(IrExpr::new(origin, IrExprKind::Unit));
            deferred_cleanups.push(DeferredCleanup {
                origin,
                cleanup,
                guard,
            });
            continue;
        }
        if let HirExprKind::Let {
            mods,
            pat,
            has_param_clause,
            params,
            value,
            ..
        } = &expr.kind
            && let Some(fallback) = mods.fallback
            && !pat_is_irrefutable_for_let_lowering(sema, *pat)
        {
            let is_callable =
                *has_param_clause || !sema.module().store.params.get(params.clone()).is_empty();
            if is_callable {
                lowering_invariant_violation(
                    "refutable let-else lowering does not support local callable declarations",
                );
            }
            lowered_exprs.push(lower_refutable_let_else_match(
                ctx,
                expr,
                *pat,
                *value,
                fallback,
                &expr_ids[index + 1..],
            ));
            break;
        }
        lowered_exprs.push(lower_expr(ctx, expr_id));
    }
    if deferred_cleanups.is_empty() {
        return IrExprKind::Sequence {
            exprs: lowered_exprs.into_boxed_slice(),
        };
    }

    let deferred_result = fresh_temp(ctx);
    let tail_expr = lowered_exprs
        .pop()
        .unwrap_or_else(|| lowering_invariant_violation("defer sequence missing tail expr"));
    lowered_exprs.push(IrExpr::new(
        tail_expr.origin,
        IrExprKind::TempLet {
            temp: deferred_result,
            value: Box::new(tail_expr),
        },
    ));
    for deferred_cleanup in deferred_cleanups.into_iter().rev() {
        lowered_exprs.push(lower_deferred_cleanup_expr(ctx, deferred_cleanup));
    }
    lowered_exprs.push(IrExpr::new(
        lowered_exprs.last().map_or_else(
            || lowering_invariant_violation("defer sequence missing temp origin"),
            |expr| expr.origin,
        ),
        IrExprKind::Temp {
            temp: deferred_result,
        },
    ));
    IrExprKind::Sequence {
        exprs: lowered_exprs.into_boxed_slice(),
    }
}

fn lower_refutable_let_else_match(
    ctx: &mut LowerCtx<'_>,
    expr: &HirExpr,
    pat: HirPatId,
    value: HirExprId,
    fallback: HirExprId,
    trailing_expr_ids: &[HirExprId],
) -> IrExpr {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let origin = IrOrigin::new(expr.origin.source_id, expr.origin.span);
    let continuation = if trailing_expr_ids.is_empty() {
        IrExpr::new(origin, IrExprKind::Unit)
    } else {
        IrExpr::new(origin, lower_sequence_expr_ids(ctx, trailing_expr_ids))
    };
    let mut arms = lower_case_patterns(sema, pat, interner)
        .into_iter()
        .map(|pattern| IrLoweredMatchArm {
            pattern,
            guard: None,
            expr: continuation.clone(),
        })
        .collect::<Vec<_>>();
    if arms.is_empty() {
        lowering_invariant_violation("refutable let-else pattern lowered to zero match arms");
    }
    arms.push(IrLoweredMatchArm {
        pattern: IrCasePattern::Wildcard,
        guard: None,
        expr: lower_expr(ctx, fallback),
    });
    IrExpr::new(
        origin,
        IrExprKind::Match {
            scrutinee: Box::new(lower_expr(ctx, value)),
            arms: arms.into_boxed_slice(),
        },
    )
}

#[derive(Clone, Copy)]
struct DeferredCleanup {
    origin: IrOrigin,
    cleanup: HirExprId,
    guard: Option<HirExprId>,
}

fn lower_deferred_cleanup_expr(
    ctx: &mut LowerCtx<'_>,
    deferred_cleanup: DeferredCleanup,
) -> IrExpr {
    if let Some(guard) = deferred_cleanup.guard {
        return IrExpr::new(
            deferred_cleanup.origin,
            IrExprKind::If {
                condition: lower_boxed_expr(ctx, guard),
                then_expr: lower_boxed_expr(ctx, deferred_cleanup.cleanup),
                else_expr: Box::new(IrExpr::new(deferred_cleanup.origin, IrExprKind::Unit)),
            },
        );
    }
    lower_expr(ctx, deferred_cleanup.cleanup)
}

pub(crate) fn lower_tuple_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirExprId>,
) -> IrExprKind {
    let sema = ctx.sema;
    IrExprKind::Tuple {
        ty_name: render_ty_name(
            sema,
            sema.try_expr_ty(expr_id).unwrap_or_else(|| {
                lowering_invariant_violation("expr type missing for tuple literal")
            }),
            ctx.interner,
        ),
        items: lower_expr_list(ctx, items),
    }
}

pub(crate) fn lower_array_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: SliceRange<HirArrayItem>,
) -> LoweringResult {
    array::lower_array_expr(ctx, expr_id, items)
}

pub(crate) fn lower_record_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    items: HirRecordItemRange,
) -> LoweringResult {
    record::lower_record_expr(ctx, expr_id, items)
}

pub(crate) fn lower_let_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    mods: HirLetMods,
    pat: HirPatId,
    has_param_clause: bool,
    params: &HirParamRange,
    value: HirExprId,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    if mods.fallback.is_some() && !pat_is_irrefutable_for_let_lowering(sema, pat) {
        lowering_invariant_violation(
            "let-else lowering for refutable patterns requires dedicated fallback-exit lowering",
        );
    }
    let is_callable =
        has_param_clause || !sema.module().store.params.get(params.clone()).is_empty();

    if is_callable {
        return lower_local_callable_let(ctx, mods, pat, params, value);
    }

    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Bind { name } => IrExprKind::Let {
            binding: decl_binding_id(sema, *name),
            name: interner.resolve(name.name).into(),
            value: Box::new(lower_expr(ctx, value)),
        },
        HirPatKind::Wildcard => IrExprKind::Let {
            binding: None,
            name: "_".into(),
            value: Box::new(lower_expr(ctx, value)),
        },
        _ => destructure::lower_destructure_let(ctx, origin, pat, value)
            .unwrap_or_else(|description| lowering_invariant_violation(description)),
    }
}

fn pat_is_irrefutable_for_let_lowering(sema: &SemaModule, pat: HirPatId) -> bool {
    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Bind { .. } => true,
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => sema
            .module()
            .store
            .pat_ids
            .get(*items)
            .iter()
            .all(|item| pat_is_irrefutable_for_let_lowering(sema, *item)),
        HirPatKind::Record { fields } => sema
            .module()
            .store
            .record_pat_fields
            .get(fields.clone())
            .iter()
            .all(|field| {
                field
                    .value
                    .is_none_or(|value| pat_is_irrefutable_for_let_lowering(sema, value))
            }),
        HirPatKind::As { pat, .. } => pat_is_irrefutable_for_let_lowering(sema, *pat),
        HirPatKind::Lit { .. } | HirPatKind::Variant { .. } | HirPatKind::Or { .. } => false,
    }
}

pub(crate) fn lower_pin_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    value: HirExprId,
    name: Ident,
    body: HirExprId,
) -> IrExpr {
    let binding_expr = IrExpr::new(
        origin,
        IrExprKind::Let {
            binding: decl_binding_id(ctx.sema, name),
            name: ctx.interner.resolve(name.name).into(),
            value: Box::new(lower_expr(ctx, value)),
        },
    );
    let body = lower_expr(ctx, body);
    IrExpr::new(
        origin,
        IrExprKind::Sequence {
            exprs: Box::new([binding_expr, body]),
        },
    )
}

pub const fn fresh_temp(ctx: &mut LowerCtx<'_>) -> IrTempId {
    let raw = ctx.next_temp_id;
    ctx.next_temp_id = ctx.next_temp_id.saturating_add(1);
    IrTempId::from_raw(raw)
}

pub(crate) fn lower_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &SliceRange<HirArg>,
) -> LoweringResult {
    call::lower_call_expr(ctx, callee, args)
}

pub(crate) fn lower_record_update_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    items: HirRecordItemRange,
) -> LoweringResult {
    record::lower_record_update_expr(ctx, expr_id, base, items)
}

pub(crate) fn lower_assign_expr(
    ctx: &mut LowerCtx<'_>,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    assign::lower_assign_expr(ctx, left, right)
        .unwrap_or_else(|description| lowering_invariant_violation(description))
}

pub(crate) fn lower_binary_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    op: &HirBinaryOp,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    let interner = ctx.interner;
    if matches!(op, HirBinaryOp::Assign) {
        return lower_assign_expr(ctx, left, right);
    }
    if matches!(op, HirBinaryOp::Range { .. }) {
        return lower_range_expr(ctx, expr_id, op, left, right);
    }
    if matches!(op, HirBinaryOp::In) {
        return lower_in_expr(ctx, expr_id, left, right);
    }
    if matches!(op, HirBinaryOp::And | HirBinaryOp::Or) {
        let left_ty =
            ctx.sema.ty(ctx.sema.try_expr_ty(left).unwrap_or_else(|| {
                lowering_invariant_violation("expr type missing for logical left")
            }));
        if matches!(left_ty.kind, HirTyKind::Bool) {
            let left = lower_boxed_expr(ctx, left);
            let right = lower_boxed_expr(ctx, right);
            return if matches!(op, HirBinaryOp::And) {
                IrExprKind::BoolAnd { left, right }
            } else {
                IrExprKind::BoolOr { left, right }
            };
        }
    }
    IrExprKind::Binary {
        op: binary::lower_binary_op(ctx, op, left, right, interner),
        left: lower_boxed_expr(ctx, left),
        right: lower_boxed_expr(ctx, right),
    }
}

pub(crate) fn lower_expr_list(
    ctx: &mut LowerCtx<'_>,
    exprs: SliceRange<HirExprId>,
) -> Box<[IrExpr]> {
    let sema = ctx.sema;
    sema.module()
        .store
        .expr_ids
        .get(exprs)
        .iter()
        .copied()
        .map(|expr| lower_expr(ctx, expr))
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

pub(crate) fn decl_binding_id(sema: &SemaModule, ident: Ident) -> Option<NameBindingId> {
    let site = NameSite::new(sema.module().source_id, ident.span);
    sema.resolved()
        .names
        .bindings
        .iter()
        .find_map(|(id, binding)| (binding.site == site).then_some(id))
}

pub(crate) fn use_binding_id(sema: &SemaModule, ident: Ident) -> Option<NameBindingId> {
    sema.resolved()
        .names
        .refs
        .get(&NameSite::new(sema.module().source_id, ident.span))
        .copied()
}
