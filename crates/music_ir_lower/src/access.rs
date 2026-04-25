use super::{
    BTreeMap, ExprMemberKind, HirExprId, HirExprKind, HirTyId, HirTyKind, Interner, IrExprKind,
    IrNameRef, IrOrigin, IrRecordLayoutField, LowerCtx, RecordLayout, SemaModule, SliceRange,
    Symbol, lower_boxed_expr, lower_constraint_answer_expr, lower_expr,
    lowering_invariant_violation, use_binding_id,
};

pub(crate) fn record_layout_for_ty(
    sema: &SemaModule,
    ty: HirTyId,
    interner: &Interner,
) -> Option<RecordLayout> {
    let items = match &sema.ty(ty).kind {
        HirTyKind::Record { fields } => {
            let mut items = sema
                .module()
                .store
                .ty_fields
                .get(fields.clone())
                .iter()
                .map(|field| Box::<str>::from(interner.resolve(field.name)))
                .collect::<Vec<_>>();
            items.sort();
            items
        }
        HirTyKind::Named { name, .. } => {
            let data_name = interner.resolve(*name);
            let data_def = sema.data_def(data_name)?;
            let variant = data_def.record_shape_variant()?;
            let mut items = variant
                .field_names()
                .iter()
                .flatten()
                .cloned()
                .collect::<Vec<_>>();
            items.sort();
            items
        }
        HirTyKind::Range { .. } => {
            vec![
                "includeLower".into(),
                "includeUpper".into(),
                "lowerBound".into(),
                "upperBound".into(),
            ]
        }
        _ => return None,
    };

    let field_count = u16::try_from(items.len()).unwrap_or_else(|_| {
        lowering_invariant_violation("record field count exceeds u16 in lowering")
    });
    let mut indices = BTreeMap::<Box<str>, u16>::new();
    let layout = items
        .into_iter()
        .enumerate()
        .map(|(idx, name)| {
            let idx = u16::try_from(idx).unwrap_or_else(|_| {
                lowering_invariant_violation("record field index exceeds u16 in lowering")
            });
            let _ = indices.insert(name.clone(), idx);
            IrRecordLayoutField::new(name, idx)
        })
        .collect::<Vec<_>>()
        .into_boxed_slice();
    Some((indices, layout, field_count))
}

pub(crate) fn lower_index_expr(
    ctx: &mut LowerCtx<'_>,
    base: HirExprId,
    args: SliceRange<HirExprId>,
) -> IrExprKind {
    let sema = ctx.sema;
    let indices = sema.module().store.expr_ids.get(args);
    if indices.is_empty() {
        lowering_invariant_violation("index without argument");
    }
    IrExprKind::Index {
        base: lower_boxed_expr(ctx, base),
        indices: indices
            .iter()
            .copied()
            .map(|index| lower_expr(ctx, index))
            .collect::<Vec<_>>()
            .into_boxed_slice(),
    }
}

pub(crate) fn lower_field_expr(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
    symbol: Symbol,
    kind: &HirExprKind,
) -> IrExprKind {
    let sema = ctx.sema;
    let interner = ctx.interner;

    if let Some(fact) = sema.expr_member_fact(expr_id)
        && fact.kind == ExprMemberKind::ShapeMember
        && let Some(evidence) = sema
            .expr_constraint_answers(expr_id)
            .and_then(|items| items.first())
    {
        return IrExprKind::RecordGet {
            base: Box::new(lower_constraint_answer_expr(
                ctx,
                IrOrigin::new(
                    sema.module().store.exprs.get(expr_id).origin.source_id,
                    sema.module().store.exprs.get(expr_id).origin.span,
                ),
                evidence,
            )),
            index: fact.index.unwrap_or_default(),
        };
    }

    let base_binding_target = match sema.module().store.exprs.get(base).kind {
        HirExprKind::Name { name } => use_binding_id(sema, name)
            .and_then(|binding| sema.binding_import_record_target(binding))
            .cloned(),
        _ => None,
    };
    if let Some(import_record_target) = sema
        .expr_import_record_target(expr_id)
        .cloned()
        .or_else(|| sema.expr_import_record_target(base).cloned())
        .or(base_binding_target)
    {
        return IrExprKind::Name {
            binding: None,
            name: interner.resolve(symbol).into(),
            import_record_target: Some(import_record_target),
        };
    }

    let base_ty = sema
        .try_expr_ty(base)
        .unwrap_or_else(|| lowering_invariant_violation("expr type missing for field base"));
    let record_ty = match sema.ty(base_ty).kind {
        HirTyKind::Mut { inner } => inner,
        _ => base_ty,
    };
    if let Some((indices, _layout, _field_count)) = record_layout_for_ty(sema, record_ty, interner)
    {
        let Some(index) = indices.get(interner.resolve(symbol)).copied() else {
            lowering_invariant_violation("record field access missing field");
        };
        return IrExprKind::RecordGet {
            base: Box::new(lower_expr(ctx, base)),
            index,
        };
    }

    if let Some(closure) = lower_attached_method_field(ctx, expr_id, base) {
        return closure;
    }

    if let Some(import_record_target) = sema.expr_import_record_target(expr_id).cloned() {
        let expr_ty = sema.try_expr_ty(expr_id).unwrap_or_else(|| {
            lowering_invariant_violation("expr type missing for dot-callable field ref")
        });
        if matches!(sema.ty(expr_ty).kind, HirTyKind::Arrow { .. }) {
            return IrExprKind::ClosureNew {
                callee: IrNameRef::new(interner.resolve(symbol))
                    .with_import_record_target(import_record_target),
                captures: vec![lower_expr(ctx, base)].into_boxed_slice(),
            };
        }
    }

    lowering_invariant_violation(format!("{kind:?}"))
}

pub(crate) fn lower_attached_method_field(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    base: HirExprId,
) -> Option<IrExprKind> {
    let fact = ctx.sema.expr_member_fact(expr_id)?;
    let captures = match fact.kind {
        ExprMemberKind::AttachedMethodNamespace => Box::new([]),
        ExprMemberKind::DotCallable | ExprMemberKind::AttachedMethod => {
            vec![lower_expr(ctx, base)].into_boxed_slice()
        }
        _ => return None,
    };
    Some(IrExprKind::ClosureNew {
        callee: IrNameRef {
            binding: fact.binding,
            name: ctx.interner.resolve(fact.name).into(),
            import_record_target: fact
                .import_record_target
                .clone()
                .or_else(|| ctx.sema.expr_import_record_target(expr_id).cloned()),
        },
        captures,
    })
}
