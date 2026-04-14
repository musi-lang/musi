use super::*;

struct AttachedCallTarget {
    base: HirExprId,
    binding: Option<NameBindingId>,
    name: Symbol,
    module_target: Option<ModuleKey>,
}

pub(super) fn lower_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    args: &SliceRange<HirArg>,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let arg_nodes = sema.module().store.args.get(args.clone());
    if let Some(attached) = resolve_attached_call_target(sema, callee) {
        return lower_attached_call_expr(ctx, callee, arg_nodes, &attached, interner);
    }

    if !arg_nodes.iter().any(|arg| arg.spread) {
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
        lower_spread_args(ctx, origin, arg_nodes, SpreadMode::Call)?;
    prelude.extend(arg_prelude);

    prelude.push(IrExpr::new(
        origin,
        if has_runtime_spread {
            IrExprKind::CallSeq {
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
                return Err("call spread lowering invariant".into());
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

fn lower_attached_call_expr(
    ctx: &mut LowerCtx<'_>,
    callee: HirExprId,
    arg_nodes: &[HirArg],
    attached: &AttachedCallTarget,
    interner: &Interner,
) -> Result<IrExprKind, Box<str>> {
    let origin = lower_origin(ctx.sema, callee);
    let callee_expr = IrExpr::new(
        origin,
        IrExprKind::Name {
            binding: attached.binding,
            name: interner.resolve(attached.name).into(),
            module_target: attached.module_target.clone(),
        },
    );

    if !arg_nodes.iter().any(|arg| arg.spread) {
        let mut lowered_args = Vec::with_capacity(arg_nodes.len().saturating_add(1));
        lowered_args.push(IrArg::new(false, lower_expr(ctx, attached.base)));
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
            value: Box::new(lower_expr(ctx, attached.base)),
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
            IrExprKind::CallSeq {
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
                return Err("attached call spread lowering invariant".into());
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

fn resolve_attached_call_target(
    sema: &SemaModule,
    callee: HirExprId,
) -> Option<AttachedCallTarget> {
    let HirExprKind::Field { base, name, .. } = sema.module().store.exprs.get(callee).kind else {
        return None;
    };
    let binding = sema.expr_attached_binding(callee);
    let base_ty = sema.try_expr_ty(base)?;
    let base_ty = match sema.ty(base_ty).kind {
        HirTyKind::Mut { inner } => inner,
        _ => base_ty,
    };
    let callee_ty = sema.try_expr_ty(callee)?;
    if !is_attached_call_candidate(&sema.ty(base_ty).kind, &sema.ty(callee_ty).kind) {
        return None;
    }

    Some(AttachedCallTarget {
        base,
        binding,
        name: name.name,
        module_target: sema.expr_module_target(callee).cloned(),
    })
}

const fn is_attached_call_candidate(base_kind: &HirTyKind, callee_kind: &HirTyKind) -> bool {
    !matches!(base_kind, HirTyKind::Module) && matches!(callee_kind, HirTyKind::Arrow { .. })
}

#[cfg(test)]
mod tests {
    use music_arena::SliceRange;
    use music_hir::HirTyKind;

    use super::is_attached_call_candidate;

    #[test]
    fn attached_call_candidate_requires_non_module_base() {
        assert!(!is_attached_call_candidate(
            &HirTyKind::Module,
            &HirTyKind::Arrow {
                params: SliceRange::EMPTY,
                ret: music_hir::HirTyId::from_raw(0),
                is_effectful: false,
            },
        ));
    }

    #[test]
    fn attached_call_candidate_requires_arrow_callee() {
        assert!(!is_attached_call_candidate(
            &HirTyKind::Int,
            &HirTyKind::Int,
        ));
        assert!(is_attached_call_candidate(
            &HirTyKind::Int,
            &HirTyKind::Arrow {
                params: SliceRange::EMPTY,
                ret: music_hir::HirTyId::from_raw(0),
                is_effectful: false,
            },
        ));
    }
}

pub(super) fn lower_request_expr(
    ctx: &mut LowerCtx<'_>,
    expr: HirExprId,
) -> Result<IrExprKind, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let (effect_key, op_index, args) =
        resolve_request_target(sema, interner, expr).map_err(Box::<str>::from)?;
    let args_nodes = sema.module().store.args.get(args);
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
        lower_spread_args(ctx, origin, args_nodes, SpreadMode::Request)?;
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
                return Err("request spread lowering invariant".into());
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

#[derive(Clone, Copy)]
enum SpreadMode {
    Call,
    Request,
}

impl SpreadMode {
    const fn runtime_any_message(self) -> &'static str {
        match self {
            Self::Call => "call runtime spread requires []Any",
            Self::Request => "request runtime spread requires []Any",
        }
    }

    const fn dims_message(self) -> &'static str {
        match self {
            Self::Call => "call spread requires 1D array or tuple",
            Self::Request => "request spread requires 1D array or tuple",
        }
    }

    const fn source_message(self) -> &'static str {
        match self {
            Self::Call => "call spread source is not tuple/array",
            Self::Request => "request spread source is not tuple/array",
        }
    }
}

fn lower_origin(sema: &SemaModule, expr: HirExprId) -> IrOrigin {
    let origin = sema.module().store.exprs.get(expr).origin;
    IrOrigin::new(origin.source_id, origin.span)
}

fn resolve_request_target(
    sema: &SemaModule,
    interner: &Interner,
    expr: HirExprId,
) -> Result<(DefinitionKey, u16, SliceRange<HirArg>), &'static str> {
    let HirExprKind::Call { callee, ref args } = sema.module().store.exprs.get(expr).kind else {
        return Err("request without call");
    };
    let HirExprKind::Field { base, name, .. } = sema.module().store.exprs.get(callee).kind else {
        return Err("request without effect op field access");
    };
    let HirExprKind::Name { name: effect_name } = sema.module().store.exprs.get(base).kind else {
        return Err("request without effect name");
    };
    let effect_name = interner.resolve(effect_name.name);
    let op_name = interner.resolve(name.name);
    let Some(effect) = sema.effect_def(effect_name) else {
        return Err("request with unknown effect");
    };
    let op_index = effect.op_index(op_name).unwrap_or(u16::MAX);
    if op_index == u16::MAX {
        return Err("request with unknown effect op");
    }
    Ok((effect.key().clone(), op_index, args.clone()))
}

fn lower_spread_args(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    args_nodes: &[HirArg],
    mode: SpreadMode,
) -> Result<(Vec<IrExpr>, Vec<IrSeqPart>, bool), Box<str>> {
    let mut prelude = Vec::<IrExpr>::new();
    let mut parts = Vec::<IrSeqPart>::new();
    let mut has_runtime_spread = false;
    for arg in args_nodes {
        let temp = fresh_temp(ctx);
        prelude.push(IrExpr::new(
            origin,
            IrExprKind::TempLet {
                temp,
                value: Box::new(lower_expr(ctx, arg.expr)),
            },
        ));
        let temp_expr = IrExpr::new(origin, IrExprKind::Temp { temp });
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
        .unwrap_or_else(|| invalid_lowering_path("expr type missing for spread arg"));
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
        HirTyKind::Range { .. } => {
            let evidence = sema
                .expr_evidence(spread_expr)
                .and_then(|items| items.first())
                .map(|item| super::lower_evidence_expr(ctx, origin, item));
            let Some(evidence) = evidence else {
                return Err("range spread evidence missing".into());
            };
            parts.push(IrSeqPart::Spread(IrExpr::new(
                origin,
                IrExprKind::RangeMaterialize {
                    range: Box::new(temp_expr.clone()),
                    evidence: Box::new(evidence),
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
