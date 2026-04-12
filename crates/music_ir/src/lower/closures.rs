use super::*;

pub(super) struct LoweredParams {
    pub(super) params: Vec<IrParam>,
    pub(super) bindings: Vec<NameBindingId>,
}

pub(super) struct ClosureCallableInput<'a> {
    pub(super) origin: IrOrigin,
    pub(super) prefix: &'a str,
    pub(super) body_id: HirExprId,
    pub(super) body: IrExpr,
    pub(super) params: LoweredParams,
    pub(super) binding: Option<NameBindingId>,
    pub(super) name: Option<Box<str>>,
    pub(super) callable_module_target: Option<ModuleKey>,
    pub(super) rewrite_recursive_self: bool,
}

pub(super) fn lower_local_callable_let(
    ctx: &mut LowerCtx<'_>,
    mods: HirLetMods,
    pat: HirPatId,
    params: &HirParamRange,
    value: HirExprId,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;

    let (binding, name) = match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Bind { name } => (
            decl_binding_id(sema, *name),
            interner.resolve(name.name).into(),
        ),
        HirPatKind::Wildcard => (None, Box::<str>::from("_")),
        other => {
            invalid_lowering_path(format!("local callable let pattern {other:?}"));
        }
    };

    let mut body = lower_expr(ctx, value);
    body.origin = IrOrigin {
        source_id: sema.module().store.exprs.get(value).origin.source_id,
        span: sema.module().store.exprs.get(value).origin.span,
    };
    let closure_value = lower_closure_callable(
        ctx,
        ClosureCallableInput {
            origin: body.origin,
            prefix: "localfn",
            body_id: value,
            body,
            params: lower_user_params(ctx, params),
            binding,
            name: (binding.is_some() && name.as_ref() != "_").then_some(name.clone()),
            callable_module_target: sema.expr_module_target(value).cloned(),
            rewrite_recursive_self: mods.is_rec,
        },
    );

    IrExprKind::Let {
        binding,
        name,
        value: Box::new(closure_value),
    }
}

pub(super) fn lower_lambda_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    params: &HirParamRange,
    body: HirExprId,
) -> IrExprKind {
    let lowered_body = lower_expr(ctx, body);
    lower_closure_callable(
        ctx,
        ClosureCallableInput {
            origin,
            prefix: "lambda",
            body_id: body,
            body: lowered_body,
            params: lower_user_params(ctx, params),
            binding: None,
            name: None,
            callable_module_target: Some(ctx.module_key.clone()),
            rewrite_recursive_self: false,
        },
    )
    .kind
}

pub(super) fn lower_user_params(ctx: &LowerCtx<'_>, params: &HirParamRange) -> LoweredParams {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let mut lowered = Vec::new();
    let mut bindings = Vec::new();
    for param in sema.module().store.params.get(params.clone()) {
        let binding = decl_binding_id(sema, param.name)
            .unwrap_or_else(|| invalid_lowering_path("param binding missing"));
        bindings.push(binding);
        lowered.push(IrParam::new(binding, interner.resolve(param.name.name)));
    }
    LoweredParams {
        params: lowered,
        bindings,
    }
}

pub(super) fn lower_named_params(ctx: &LowerCtx<'_>, params: &[Ident]) -> LoweredParams {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let mut lowered = Vec::new();
    let mut bindings = Vec::new();
    for param in params {
        let binding = decl_binding_id(sema, *param)
            .unwrap_or_else(|| invalid_lowering_path("named param binding missing in lowering"));
        bindings.push(binding);
        lowered.push(IrParam::new(binding, interner.resolve(param.name)));
    }
    LoweredParams {
        params: lowered,
        bindings,
    }
}

pub(super) fn lower_closure_callable(
    ctx: &mut LowerCtx<'_>,
    input: ClosureCallableInput<'_>,
) -> IrExpr {
    let sema = ctx.sema;
    let captures =
        compute_capture_bindings(ctx, &input.body, &input.params.bindings, input.binding);
    let body = if input.rewrite_recursive_self {
        if let Some(binding) = input.binding {
            super::rewrite_recursive_binding_refs(
                ctx,
                input.origin,
                input.body,
                binding,
                input.name.as_deref().unwrap_or("_"),
                &captures,
            )
        } else {
            input.body
        }
    } else {
        input.body
    };
    let capture_params = lower_capture_params(ctx, &captures);
    let capture_exprs = lower_capture_exprs(ctx, input.origin, &captures);

    let mut callable_params = Vec::new();
    callable_params.extend(capture_params);
    callable_params.extend(input.params.params);

    let callable_name = input
        .name
        .unwrap_or_else(|| fresh_lambda_name(ctx, input.prefix, input.origin));

    ctx.extra_callables.push(
        IrCallable::new(
            callable_name.clone(),
            callable_params.into_boxed_slice(),
            body,
        )
        .with_binding_opt(input.binding)
        .with_effects(
            sema.try_expr_effects(input.body_id)
                .unwrap_or_else(|| invalid_lowering_path("expr effects missing for closure body"))
                .clone(),
        )
        .with_module_target_opt(input.callable_module_target),
    );

    IrExpr::new(
        input.origin,
        IrExprKind::ClosureNew {
            callee: IrNameRef::new(callable_name)
                .with_binding_opt(input.binding)
                .with_module_target(ctx.module_key.clone()),
            captures: capture_exprs,
        },
    )
}

fn compute_capture_bindings(
    ctx: &LowerCtx<'_>,
    body: &IrExpr,
    user_params: &[NameBindingId],
    binder: Option<NameBindingId>,
) -> Vec<NameBindingId> {
    let mut used = HashSet::<NameBindingId>::new();
    collect_used_bindings(body, &mut used);

    let mut local = HashSet::<NameBindingId>::new();
    for param in user_params {
        let _ = local.insert(*param);
    }
    if let Some(binder) = binder {
        let _ = local.insert(binder);
    }
    collect_local_decl_bindings(body, &mut local);

    used.retain(|binding| !local.contains(binding) && !ctx.module_level_bindings.contains(binding));
    let mut captures = used.into_iter().collect::<Vec<_>>();
    captures.sort_by_key(|binding| binding.raw());
    captures
}

fn collect_used_bindings(expr: &IrExpr, out: &mut BoundNameSet) {
    collect::collect_used_bindings(expr, out);
}

fn collect_local_decl_bindings(expr: &IrExpr, out: &mut BoundNameSet) {
    collect::collect_local_decl_bindings(expr, out);
}

fn binding_name(ctx: &LowerCtx<'_>, binding: NameBindingId) -> Box<str> {
    let binding = ctx.sema.resolved().names.bindings.get(binding);
    ctx.interner.resolve(binding.name).into()
}

fn lower_capture_params(ctx: &LowerCtx<'_>, captures: &[NameBindingId]) -> Vec<IrParam> {
    captures
        .iter()
        .copied()
        .map(|binding| IrParam::new(binding, binding_name(ctx, binding)))
        .collect()
}

fn name_expr(ctx: &LowerCtx<'_>, origin: IrOrigin, binding: NameBindingId) -> IrExpr {
    IrExpr::new(
        origin,
        IrExprKind::Name {
            binding: Some(binding),
            name: binding_name(ctx, binding),
            module_target: None,
        },
    )
}

pub(super) fn lower_capture_exprs(
    ctx: &LowerCtx<'_>,
    origin: IrOrigin,
    captures: &[NameBindingId],
) -> Box<[IrExpr]> {
    captures
        .iter()
        .copied()
        .map(|binding| name_expr(ctx, origin, binding))
        .collect::<Vec<_>>()
        .into_boxed_slice()
}

fn fresh_lambda_name(ctx: &mut LowerCtx<'_>, prefix: &str, origin: IrOrigin) -> Box<str> {
    let id = ctx.next_lambda_id;
    ctx.next_lambda_id = ctx.next_lambda_id.saturating_add(1);
    format!(
        "{prefix}:{}:{}:{}:{id}",
        origin.source_id.raw(),
        origin.span.start,
        origin.span.end
    )
    .into()
}
