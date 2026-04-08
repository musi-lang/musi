use super::*;

pub(super) fn lower_call_expr(ctx: &mut LowerCtx<'_>, callee: HirExprId, args: &SliceRange<HirArg>) -> IrExprKind {
    let sema = ctx.sema;
    let arg_nodes = sema.module().store.args.get(args.clone());
    if !arg_nodes.iter().any(|arg| arg.spread) {
        return IrExprKind::Call {
            callee: Box::new(lower_expr(ctx, callee)),
            args: arg_nodes
                .iter()
                .map(|arg| IrArg {
                    spread: false,
                    expr: lower_expr(ctx, arg.expr),
                })
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        };
    }

    let origin = lower_origin(sema, callee);
    let mut prelude = Vec::<IrExpr>::new();
    let callee_temp = fresh_temp(ctx);
    prelude.push(IrExpr {
        origin,
        kind: IrExprKind::TempLet {
            temp: callee_temp,
            value: Box::new(lower_expr(ctx, callee)),
        },
    });
    let callee_expr = IrExpr {
        origin,
        kind: IrExprKind::Temp { temp: callee_temp },
    };

    let (arg_prelude, parts, has_runtime_spread) = match lower_spread_args(ctx, origin, arg_nodes, SpreadMode::Call)
    {
        Ok(value) => value,
        Err(kind) => return kind,
    };
    prelude.extend(arg_prelude);

    prelude.push(IrExpr {
        origin,
        kind: if has_runtime_spread {
            IrExprKind::CallSeq {
                callee: Box::new(callee_expr),
                args: parts.into_boxed_slice(),
            }
        } else {
            let args = parts
                .into_iter()
                .map(|part| match part {
                    IrSeqPart::Expr(expr) => Some(IrArg { spread: false, expr }),
                    IrSeqPart::Spread(_) => None,
                })
                .collect::<Option<Vec<_>>>()
                .map(Vec::into_boxed_slice);
            let Some(args) = args else {
                return IrExprKind::Unsupported {
                    description: "call spread lowering invariant".into(),
                };
            };
            IrExprKind::Call {
                callee: Box::new(callee_expr),
                args,
            }
        },
    });

    IrExprKind::Sequence {
        exprs: prelude.into_boxed_slice(),
    }
}

pub(super) fn lower_perform_expr(ctx: &mut LowerCtx<'_>, expr: HirExprId) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let (effect_key, op_index, args) = match resolve_perform_target(sema, interner, expr) {
        Ok(value) => value,
        Err(description) => {
            return IrExprKind::Unsupported {
                description: description.into(),
            };
        }
    };
    let args_nodes = sema.module().store.args.get(args);
    if !args_nodes.iter().any(|arg| arg.spread) {
        let lowered_args = args_nodes
            .iter()
            .map(|arg| lower_expr(ctx, arg.expr))
            .collect::<Vec<_>>()
            .into_boxed_slice();
        return IrExprKind::Perform {
            effect_key,
            op_index,
            args: lowered_args,
        };
    }

    let origin = lower_origin(sema, expr);
    let (prelude, parts, has_runtime_spread) = match lower_spread_args(ctx, origin, args_nodes, SpreadMode::Perform) {
        Ok(value) => value,
        Err(kind) => return kind,
    };
    let mut exprs = prelude;
    exprs.push(IrExpr {
        origin,
        kind: if has_runtime_spread {
            IrExprKind::PerformSeq {
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
                return IrExprKind::Unsupported {
                    description: "perform spread lowering invariant".into(),
                };
            };
            IrExprKind::Perform {
                effect_key,
                op_index,
                args,
            }
        },
    });
    IrExprKind::Sequence {
        exprs: exprs.into_boxed_slice(),
    }
}

#[derive(Clone, Copy)]
enum SpreadMode {
    Call,
    Perform,
}

impl SpreadMode {
    const fn runtime_any_message(self) -> &'static str {
        match self {
            Self::Call => "call runtime spread requires []Any",
            Self::Perform => "perform runtime spread requires []Any",
        }
    }

    const fn dims_message(self) -> &'static str {
        match self {
            Self::Call => "call spread requires 1D array or tuple",
            Self::Perform => "perform spread requires 1D array or tuple",
        }
    }

    const fn source_message(self) -> &'static str {
        match self {
            Self::Call => "call spread source is not tuple/array",
            Self::Perform => "perform spread source is not tuple/array",
        }
    }
}

fn lower_origin(sema: &SemaModule, expr: HirExprId) -> IrOrigin {
    let origin = sema.module().store.exprs.get(expr).origin;
    IrOrigin {
        source_id: origin.source_id,
        span: origin.span,
    }
}

fn resolve_perform_target(
    sema: &SemaModule,
    interner: &Interner,
    expr: HirExprId,
) -> Result<(DefinitionKey, u16, SliceRange<HirArg>), &'static str> {
    let HirExprKind::Call { callee, ref args } = sema.module().store.exprs.get(expr).kind else {
        return Err("perform without call");
    };
    let HirExprKind::Field { base, name, .. } = sema.module().store.exprs.get(callee).kind else {
        return Err("perform without effect op field access");
    };
    let HirExprKind::Name { name: effect_name } = sema.module().store.exprs.get(base).kind else {
        return Err("perform without effect name");
    };
    let effect_name = interner.resolve(effect_name.name);
    let op_name = interner.resolve(name.name);
    let Some(effect) = sema.effect_def(effect_name) else {
        return Err("perform with unknown effect");
    };
    let op_index = effect
        .ops
        .keys()
        .position(|entry| entry.as_ref() == op_name)
        .and_then(|index| u16::try_from(index).ok())
        .unwrap_or(u16::MAX);
    if op_index == u16::MAX {
        return Err("perform with unknown effect op");
    }
    Ok((effect.key.clone(), op_index, args.clone()))
}

fn lower_spread_args(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    args_nodes: &[HirArg],
    mode: SpreadMode,
) -> Result<(Vec<IrExpr>, Vec<IrSeqPart>, bool), IrExprKind> {
    let sema = ctx.sema;
    let mut prelude = Vec::<IrExpr>::new();
    let mut parts = Vec::<IrSeqPart>::new();
    let mut has_runtime_spread = false;
    for arg in args_nodes {
        let temp = fresh_temp(ctx);
        prelude.push(IrExpr {
            origin,
            kind: IrExprKind::TempLet {
                temp,
                value: Box::new(lower_expr(ctx, arg.expr)),
            },
        });
        let temp_expr = IrExpr {
            origin,
            kind: IrExprKind::Temp { temp },
        };
        if !arg.spread {
            parts.push(IrSeqPart::Expr(temp_expr));
            continue;
        }
        has_runtime_spread |= lower_spread_arg(sema, arg.expr, &temp_expr, origin, &mut parts, mode)?;
    }
    Ok((prelude, parts, has_runtime_spread))
}

fn lower_spread_arg(
    sema: &SemaModule,
    spread_expr: HirExprId,
    temp_expr: &IrExpr,
    origin: IrOrigin,
    parts: &mut Vec<IrSeqPart>,
    mode: SpreadMode,
) -> Result<bool, IrExprKind> {
    let spread_ty = sema.expr_ty(spread_expr);
    match &sema.ty(spread_ty).kind {
        HirTyKind::Tuple { items } => {
            for (index, _) in sema.module().store.ty_ids.get(*items).iter().enumerate() {
                let Ok(index_u32) = u32::try_from(index) else {
                    continue;
                };
                parts.push(IrSeqPart::Expr(index_expr(origin, temp_expr.clone(), index_u32)));
            }
            Ok(false)
        }
        HirTyKind::Array { dims, item } => lower_spread_array_arg(sema, dims, *item, temp_expr, origin, parts, mode),
        _ => Err(IrExprKind::Unsupported {
            description: mode.source_message().into(),
        }),
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
) -> Result<bool, IrExprKind> {
    let dims_vec = sema.module().store.dims.get(dims.clone());
    if dims_vec.is_empty() {
        if matches!(sema.ty(item).kind, HirTyKind::Any) {
            parts.push(IrSeqPart::Spread(temp_expr.clone()));
            return Ok(true);
        }
        return Err(IrExprKind::Unsupported {
            description: mode.runtime_any_message().into(),
        });
    }
    if dims_vec.len() != 1 {
        return Err(IrExprKind::Unsupported {
            description: mode.dims_message().into(),
        });
    }
    match dims_vec[0] {
        HirDim::Int(len) => {
            for index_u32 in 0..len {
                parts.push(IrSeqPart::Expr(index_expr(origin, temp_expr.clone(), index_u32)));
            }
            Ok(false)
        }
        HirDim::Unknown | HirDim::Name(_) if matches!(sema.ty(item).kind, HirTyKind::Any) => {
            parts.push(IrSeqPart::Spread(temp_expr.clone()));
            Ok(true)
        }
        HirDim::Unknown | HirDim::Name(_) => Err(IrExprKind::Unsupported {
            description: mode.runtime_any_message().into(),
        }),
    }
}

fn index_expr(origin: IrOrigin, base: IrExpr, index_u32: u32) -> IrExpr {
    IrExpr {
        origin,
        kind: IrExprKind::Index {
            base: Box::new(base),
            index: Box::new(IrExpr {
                origin,
                kind: IrExprKind::Lit(IrLit::Int {
                    raw: index_u32.to_string().into(),
                }),
            }),
        },
    }
}
