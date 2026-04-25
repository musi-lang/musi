use super::{
    DefinitionKey, HirArg, HirDim, HirExprId, HirExprKind, HirTyId, HirTyKind, Interner, IrExpr,
    IrExprKind, IrLit, IrOrigin, IrSeqPart, LowerCtx, NameSite, SemaModule, SliceRange, Symbol,
    fresh_temp, lower_expr, lowering_invariant_violation, resolve_dot_callable_call_target,
};

pub(super) fn ordered_call_args(
    sema: &SemaModule,
    interner: &Interner,
    callee: HirExprId,
    args: &[HirArg],
) -> Vec<HirArg> {
    if !args.iter().any(|arg| arg.name.is_some()) {
        return args.to_vec();
    }

    let param_names = call_param_names(sema, interner, callee);
    if param_names.is_empty() {
        return args.to_vec();
    }

    let mut prefix = Vec::new();
    let mut named_start = 0usize;
    let mut in_named_suffix = false;

    for arg in args {
        if arg.name.is_some() {
            in_named_suffix = true;
            break;
        }
        let Some(next_index) = named_prefix_advance(sema, arg, named_start) else {
            return args.to_vec();
        };
        named_start = next_index;
        prefix.push(arg.clone());
    }

    let mut ordered = vec![None; param_names.len()];
    for arg in args.iter().skip(prefix.len()) {
        let Some(name) = arg.name else {
            return args.to_vec();
        };
        if arg.spread {
            return args.to_vec();
        }
        let Some(index) = param_names.iter().position(|param| *param == name.name) else {
            return args.to_vec();
        };
        if !in_named_suffix || index < named_start || ordered[index].is_some() {
            return args.to_vec();
        }
        ordered[index] = Some(arg.clone());
    }

    let mut reordered_args = prefix;
    for arg in ordered.into_iter().skip(named_start).flatten() {
        reordered_args.push(arg);
    }
    if reordered_args.len() == args.len() {
        reordered_args
    } else {
        args.to_vec()
    }
}

fn named_prefix_advance(sema: &SemaModule, arg: &HirArg, index: usize) -> Option<usize> {
    if !arg.spread {
        return Some(index.saturating_add(1));
    }

    let spread_ty = sema.try_expr_ty(arg.expr)?;
    match &sema.ty(spread_ty).kind {
        HirTyKind::Tuple { items } => {
            Some(index.saturating_add(sema.module().store.ty_ids.get(*items).len()))
        }
        HirTyKind::Array { dims, .. } => {
            let dims = sema.module().store.dims.get(dims.clone());
            (dims.len() == 1).then_some(())?;
            match dims[0] {
                HirDim::Int(len) => Some(index.saturating_add(usize::try_from(len).ok()?)),
                HirDim::Unknown | HirDim::Name(_) => None,
            }
        }
        _ => None,
    }
}

fn call_param_names(sema: &SemaModule, interner: &Interner, callee: HirExprId) -> Box<[Symbol]> {
    if let Some(target) = resolve_dot_callable_call_target(sema, callee)
        && let Some(binding) = target.binding
        && let Some(scheme) = sema.binding_scheme(binding)
    {
        return scheme.param_names.iter().copied().skip(1).collect();
    }

    match sema.module().store.exprs.get(callee).kind {
        HirExprKind::Name { name } => sema
            .resolved()
            .names
            .refs
            .get(&NameSite::new(sema.module().source_id, name.span))
            .copied()
            .and_then(|binding| sema.binding_scheme(binding))
            .map(|scheme| scheme.param_names.clone())
            .unwrap_or_default(),
        HirExprKind::Field { base, name, .. } => {
            if let HirExprKind::Name { name: effect_name } =
                sema.module().store.exprs.get(base).kind
                && let Some(effect) = sema.effect_def(interner.resolve(effect_name.name))
                && let Some(op) = effect.op(interner.resolve(name.name))
            {
                return op.param_names().to_vec().into_boxed_slice();
            }
            Box::default()
        }
        _ => Box::default(),
    }
}

#[derive(Clone, Copy)]
pub(super) enum SpreadMode {
    Call,
    Request,
}

impl SpreadMode {
    const fn runtime_any_message(self) -> &'static str {
        match self {
            Self::Call => "call runtime spread needs []Any",
            Self::Request => "ask runtime spread needs []Any",
        }
    }

    const fn dims_message(self) -> &'static str {
        match self {
            Self::Call => "call spread needs 1D array or tuple",
            Self::Request => "ask spread needs 1D array or tuple",
        }
    }

    const fn source_message(self) -> &'static str {
        match self {
            Self::Call => "call spread source is not tuple/array",
            Self::Request => "ask spread source is not tuple/array",
        }
    }
}

pub(super) fn lower_origin(sema: &SemaModule, expr: HirExprId) -> IrOrigin {
    let origin = sema.module().store.exprs.get(expr).origin;
    IrOrigin::new(origin.source_id, origin.span)
}

pub(super) fn resolve_request_target(
    sema: &SemaModule,
    interner: &Interner,
    expr: HirExprId,
) -> Result<(DefinitionKey, u16, HirExprId, SliceRange<HirArg>), Box<str>> {
    let HirExprKind::Call { callee, ref args } = sema.module().store.exprs.get(expr).kind else {
        return Err(super::lower_errors::lowering_error("ask without call"));
    };
    let HirExprKind::Field { base, name, .. } = sema.module().store.exprs.get(callee).kind else {
        return Err(super::lower_errors::lowering_error(
            "ask without effect op field access",
        ));
    };
    let HirExprKind::Name { name: effect_name } = sema.module().store.exprs.get(base).kind else {
        return Err(super::lower_errors::lowering_error(
            "ask without effect name",
        ));
    };
    let effect_name = interner.resolve(effect_name.name);
    let op_name = interner.resolve(name.name);
    let Some(effect) = sema.effect_def(effect_name) else {
        return Err(super::lower_errors::lowering_error(
            "ask with unknown effect",
        ));
    };
    let op_index = effect.op_index(op_name).unwrap_or(u16::MAX);
    if op_index == u16::MAX {
        return Err(super::lower_errors::lowering_error(
            "ask with unknown effect op",
        ));
    }
    Ok((effect.key().clone(), op_index, callee, args.clone()))
}

pub(super) fn lower_spread_args(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    args_nodes: &[HirArg],
    mode: SpreadMode,
) -> Result<(Vec<IrExpr>, Vec<IrSeqPart>, bool), Box<str>> {
    let mut prelude = Vec::<IrExpr>::new();
    let mut parts = Vec::<IrSeqPart>::new();
    let mut has_runtime_spread = false;
    for arg in args_nodes {
        let temp_id = fresh_temp(ctx);
        prelude.push(IrExpr::new(
            origin,
            IrExprKind::TempLet {
                temp: temp_id,
                value: Box::new(lower_expr(ctx, arg.expr)),
            },
        ));
        let temp_expr = IrExpr::new(origin, IrExprKind::Temp { temp: temp_id });
        if !arg.spread {
            parts.push(IrSeqPart::Expr(temp_expr));
            continue;
        }
        has_runtime_spread |=
            lower_spread_arg(ctx, arg.expr, &temp_expr, origin, &mut parts, mode)?;
    }
    Ok((prelude, parts, has_runtime_spread))
}

fn lower_spread_arg(
    ctx: &mut LowerCtx<'_>,
    spread_expr: HirExprId,
    temp_expr: &IrExpr,
    origin: IrOrigin,
    parts: &mut Vec<IrSeqPart>,
    mode: SpreadMode,
) -> Result<bool, Box<str>> {
    let sema = ctx.sema;
    let spread_ty = sema
        .try_expr_ty(spread_expr)
        .unwrap_or_else(|| lowering_invariant_violation("expr type missing for spread arg"));
    match &sema.ty(spread_ty).kind {
        HirTyKind::Tuple { items } => {
            for (index, _) in sema.module().store.ty_ids.get(*items).iter().enumerate() {
                let Ok(index_u32) = u32::try_from(index) else {
                    continue;
                };
                parts.push(IrSeqPart::Expr(index_expr(
                    origin,
                    temp_expr.clone(),
                    index_u32,
                )));
            }
            Ok(false)
        }
        HirTyKind::Array { dims, item } => {
            lower_spread_array_arg(sema, dims, *item, temp_expr, origin, parts, mode)
        }
        HirTyKind::Seq { item } => {
            if matches!(sema.ty(*item).kind, HirTyKind::Any) {
                parts.push(IrSeqPart::Spread(temp_expr.clone()));
                Ok(true)
            } else {
                Err(mode.runtime_any_message().into())
            }
        }
        HirTyKind::Range { bound } => {
            let result_ty_name =
                format!("[]{}", super::render_ty_name(sema, *bound, ctx.interner)).into();
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
        _ => Err(mode.source_message().into()),
    }
}

fn lower_spread_array_arg(
    sema: &SemaModule,
    dims: &SliceRange<HirDim>,
    item: HirTyId,
    temp_expr: &IrExpr,
    origin: IrOrigin,
    parts: &mut Vec<IrSeqPart>,
    mode: SpreadMode,
) -> Result<bool, Box<str>> {
    let dims_vec = sema.module().store.dims.get(dims.clone());
    if dims_vec.is_empty() {
        if matches!(sema.ty(item).kind, HirTyKind::Any) {
            parts.push(IrSeqPart::Spread(temp_expr.clone()));
            return Ok(true);
        }
        return Err(mode.runtime_any_message().into());
    }
    if dims_vec.len() != 1 {
        return Err(mode.dims_message().into());
    }
    match dims_vec[0] {
        HirDim::Int(len) => {
            for index_u32 in 0..len {
                parts.push(IrSeqPart::Expr(index_expr(
                    origin,
                    temp_expr.clone(),
                    index_u32,
                )));
            }
            Ok(false)
        }
        HirDim::Unknown | HirDim::Name(_) if matches!(sema.ty(item).kind, HirTyKind::Any) => {
            parts.push(IrSeqPart::Spread(temp_expr.clone()));
            Ok(true)
        }
        HirDim::Unknown | HirDim::Name(_) => Err(mode.runtime_any_message().into()),
    }
}

fn index_expr(origin: IrOrigin, base: IrExpr, index_u32: u32) -> IrExpr {
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
