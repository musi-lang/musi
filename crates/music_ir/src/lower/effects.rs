use super::*;

pub(super) fn lower_request_expr(ctx: &mut LowerCtx<'_>, expr: HirExprId) -> LoweringResult {
    call::lower_request_expr(ctx, expr)
}

pub(super) fn lower_handler_literal_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    effect_name: Ident,
    clauses: SliceRange<HirHandleClause>,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let handler_name = interner.resolve(effect_name.name);
    let Some(effect) = sema.effect_def(handler_name) else {
        invalid_lowering_path("handle with unknown effect");
    };
    let origin = sema.module().store.exprs.get(expr_id).origin;
    let origin = IrOrigin::new(origin.source_id, origin.span);

    let clauses_vec = sema.module().store.handle_clauses.get(clauses).to_vec();
    let mut value_clause = None::<HirHandleClause>;
    let mut op_clauses = Vec::<HirHandleClause>::new();
    for clause in clauses_vec {
        let op_name = interner.resolve(clause.op.name);
        if op_name == "value" {
            value_clause = Some(clause);
        } else {
            op_clauses.push(clause);
        }
    }
    let Some(value_clause) = value_clause else {
        invalid_lowering_path("handle without value clause");
    };

    let value_closure = lower_handler_clause_closure(
        ctx,
        origin,
        "hdl.value",
        &[value_clause.op],
        value_clause.body,
    );

    let mut ops_by_index = vec![None::<IrHandleOp>; effect.op_count()];
    for clause in op_clauses {
        let op_name = interner.resolve(clause.op.name);
        let Some(op_index) = effect.op_index(op_name).map(usize::from) else {
            invalid_lowering_path("handle clause with unknown effect op");
        };
        let closure = lower_handler_clause_closure(
            ctx,
            origin,
            &format!("hdl.op.{op_name}"),
            sema.module().store.idents.get(clause.params),
            clause.body,
        );
        ops_by_index[op_index] = Some(IrHandleOp::new(
            u16::try_from(op_index).unwrap_or(u16::MAX),
            op_name,
            closure,
        ));
    }

    if ops_by_index.iter().any(Option::is_none) {
        invalid_lowering_path("handle missing op clause");
    }

    IrExprKind::HandlerLit {
        effect_key: effect.key().clone(),
        value: Box::new(value_closure),
        ops: ops_by_index
            .into_iter()
            .flatten()
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

pub(super) fn lower_handle_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    expr: HirExprId,
    handler: HirExprId,
) -> IrExprKind {
    let handler_expr = lower_expr(ctx, handler);
    let effect_key = if let IrExprKind::HandlerLit { effect_key, .. } = &handler_expr.kind {
        effect_key.clone()
    } else {
        lower_handler_effect_key(ctx, handler)
    };
    let _ = expr_id;
    IrExprKind::Handle {
        effect_key,
        handler: Box::new(handler_expr),
        body: Box::new(lower_expr(ctx, expr)),
    }
}

fn lower_handler_effect_key(ctx: &LowerCtx<'_>, handler: HirExprId) -> DefinitionKey {
    let sema = ctx.sema;
    let handler_ty = sema
        .try_expr_ty(handler)
        .unwrap_or_else(|| invalid_lowering_path("handler type missing"));
    let effect_name = match sema.ty(handler_ty).kind {
        HirTyKind::Handler { effect, .. } => match sema.ty(effect).kind {
            HirTyKind::Named { name, .. } => Box::<str>::from(ctx.interner.resolve(name)),
            _ => invalid_lowering_path("handler effect type missing"),
        },
        _ => invalid_lowering_path("invalid handler type"),
    };
    sema.effect_def(&effect_name).map_or_else(
        || invalid_lowering_path("handler effect missing"),
        |effect| effect.key().clone(),
    )
}

fn lower_handler_clause_closure(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    prefix: &str,
    params: &[Ident],
    body: HirExprId,
) -> IrExpr {
    let lowered_body = lower_expr(ctx, body);
    super::lower_closure_callable(
        ctx,
        super::ClosureCallableInput {
            origin,
            prefix,
            body_id: body,
            body: lowered_body,
            hidden_params: Vec::new(),
            hidden_param_names: Vec::new(),
            hidden_capture_exprs: Vec::new(),
            params: super::lower_named_params(ctx, params),
            binding: None,
            name: None,
            callable_module_target: ctx.sema.expr_module_target(body).cloned(),
            rewrite_recursive_self: false,
        },
    )
}
