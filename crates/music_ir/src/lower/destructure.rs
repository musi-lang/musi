use super::*;

#[derive(Clone, Copy)]
struct IrrefutablePatInput<'a> {
    origin: IrOrigin,
    module_target: Option<&'a ModuleKey>,
}

pub(super) fn lower_destructure_let(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    pat: HirPatId,
    value: HirExprId,
) -> Result<IrExprKind, Box<str>> {
    let module_target = ctx.sema.expr_module_target(value);

    let value_expr = lower_expr(ctx, value);
    let temp = fresh_temp(ctx);
    let mut exprs = Vec::<IrExpr>::new();
    exprs.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp,
            value: Box::new(value_expr),
        },
    ));

    let base = IrExpr::new(origin, IrExprKind::Temp { temp });
    lower_irrefutable_pat_bindings(ctx, origin, module_target, pat, base, &mut exprs)?;
    exprs.push(IrExpr::new(origin, IrExprKind::Unit));
    Ok(IrExprKind::Sequence {
        exprs: exprs.into_boxed_slice(),
    })
}

fn lower_irrefutable_pat_bindings(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    module_target: Option<&ModuleKey>,
    pat: HirPatId,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) -> Result<(), Box<str>> {
    let sema = ctx.sema;
    let input = IrrefutablePatInput {
        origin,
        module_target,
    };
    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Error | HirPatKind::Wildcard => {}
        HirPatKind::Bind { name } => lower_irrefutable_bind(ctx, input, *name, base, out),
        HirPatKind::As { pat: inner, name } => {
            lower_irrefutable_as(ctx, input, *inner, *name, base, out)?;
        }
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => {
            lower_irrefutable_sequence(ctx, input, *items, base, out)?;
        }
        HirPatKind::Record { fields } => {
            lower_irrefutable_record(ctx, input, fields.clone(), pat, base, out)?;
        }
        other => return Err(format!("invalid local let pattern in lowering: {other:?}").into()),
    }
    Ok(())
}

fn lower_irrefutable_bind(
    ctx: &LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    name: Ident,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) {
    let sema = ctx.sema;
    let interner = ctx.interner;
    out.push(IrExpr::new(
        input.origin,
        IrExprKind::Let {
            binding: decl_binding_id(sema, name),
            name: interner.resolve(name.name).into(),
            value: Box::new(base),
        },
    ));
}

fn lower_irrefutable_as(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    inner: HirPatId,
    name: Ident,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) -> Result<(), Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let stored = store_in_temp(ctx, input.origin, base, out);
    out.push(IrExpr::new(
        input.origin,
        IrExprKind::Let {
            binding: decl_binding_id(sema, name),
            name: interner.resolve(name.name).into(),
            value: Box::new(stored.clone()),
        },
    ));
    lower_irrefutable_pat_bindings(ctx, input.origin, input.module_target, inner, stored, out)?;
    Ok(())
}

fn lower_irrefutable_sequence(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    items: SliceRange<HirPatId>,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) -> Result<(), Box<str>> {
    let sema = ctx.sema;
    let stored = store_in_temp(ctx, input.origin, base, out);
    for (index, item) in sema.module().store.pat_ids.get(items).iter().enumerate() {
        let Some(proj) = project_index_expr(input.origin, stored.clone(), index) else {
            continue;
        };
        lower_irrefutable_pat_bindings(ctx, input.origin, input.module_target, *item, proj, out)?;
    }
    Ok(())
}

fn lower_irrefutable_record(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    fields: SliceRange<HirRecordPatField>,
    pat: HirPatId,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) -> Result<(), Box<str>> {
    let sema = ctx.sema;
    let pat_ty = sema
        .try_pat_ty(pat)
        .unwrap_or_else(|| invalid_lowering_path("pattern type missing for destructuring"));
    match &sema.ty(pat_ty).kind {
        HirTyKind::Module => {
            lower_irrefutable_module_record(ctx, input.origin, input.module_target, fields, out)?;
        }
        HirTyKind::Record { .. } => {
            lower_irrefutable_value_record(ctx, input, fields, pat_ty, base, out)?;
        }
        _ => return Err("record destructuring without record base".into()),
    }
    Ok(())
}

fn lower_irrefutable_module_record(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    module_target: Option<&ModuleKey>,
    fields: SliceRange<HirRecordPatField>,
    out: &mut Vec<IrExpr>,
) -> Result<(), Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let Some(module_target) = module_target else {
        return Err("module destructuring without module target".into());
    };
    for field in sema.module().store.record_pat_fields.get(fields) {
        let name_text: Box<str> = interner.resolve(field.name.name).into();
        let proj = IrExpr::new(
            origin,
            IrExprKind::Name {
                binding: None,
                name: name_text.clone(),
                module_target: Some(module_target.clone()),
            },
        );
        lower_irrefutable_record_field(
            ctx,
            IrrefutablePatInput {
                origin,
                module_target: Some(module_target),
            },
            field,
            proj,
            name_text,
            out,
        )?;
    }
    Ok(())
}

fn lower_irrefutable_value_record(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    fields: SliceRange<HirRecordPatField>,
    pat_ty: HirTyId,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) -> Result<(), Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let stored = store_in_temp(ctx, input.origin, base, out);
    let Some((indices, _layout, _field_count)) = record_layout_for_ty(sema, pat_ty, interner)
    else {
        return Err("record destructuring without record layout".into());
    };
    for field in sema.module().store.record_pat_fields.get(fields) {
        let Some(index) = indices.get(&field.name.name).copied() else {
            return Err("record destructuring missing field".into());
        };
        let proj = IrExpr::new(
            input.origin,
            IrExprKind::RecordGet {
                base: Box::new(stored.clone()),
                index,
            },
        );
        let name_text: Box<str> = interner.resolve(field.name.name).into();
        lower_irrefutable_record_field(ctx, input, field, proj, name_text, out)?;
    }
    Ok(())
}

fn lower_irrefutable_record_field(
    ctx: &mut LowerCtx<'_>,
    input: IrrefutablePatInput<'_>,
    field: &HirRecordPatField,
    proj: IrExpr,
    name: Box<str>,
    out: &mut Vec<IrExpr>,
) -> Result<(), Box<str>> {
    if let Some(value_pat) = field.value {
        lower_irrefutable_pat_bindings(
            ctx,
            input.origin,
            input.module_target,
            value_pat,
            proj,
            out,
        )?;
        return Ok(());
    }
    out.push(IrExpr::new(
        input.origin,
        IrExprKind::Let {
            binding: decl_binding_id(ctx.sema, field.name),
            name,
            value: Box::new(proj),
        },
    ));
    Ok(())
}

fn project_index_expr(origin: IrOrigin, base: IrExpr, index: usize) -> Option<IrExpr> {
    let Ok(index_u32) = u32::try_from(index) else {
        return None;
    };
    Some(IrExpr::new(
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
    ))
}

fn store_in_temp(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    base: IrExpr,
    out: &mut Vec<IrExpr>,
) -> IrExpr {
    let temp = fresh_temp(ctx);
    out.push(IrExpr::new(
        origin,
        IrExprKind::TempLet {
            temp,
            value: Box::new(base),
        },
    ));
    IrExpr::new(origin, IrExprKind::Temp { temp })
}
