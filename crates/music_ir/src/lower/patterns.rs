use super::*;

pub(super) fn collect_pattern_bindings(pattern: &IrCasePattern, out: &mut BoundNameSet) {
    match pattern {
        IrCasePattern::Wildcard | IrCasePattern::Lit(_) => {}
        IrCasePattern::Bind { binding, .. } => {
            let _ = out.insert(*binding);
        }
        IrCasePattern::Tuple { items } | IrCasePattern::Array { items } => {
            for item in items {
                collect_pattern_bindings(item, out);
            }
        }
        IrCasePattern::Record { fields } => {
            for field in fields {
                collect_pattern_bindings(&field.pat, out);
            }
        }
        IrCasePattern::Variant { args, .. } => {
            for arg in args {
                collect_pattern_bindings(arg, out);
            }
        }
        IrCasePattern::As { pat, binding, .. } => {
            let _ = out.insert(*binding);
            collect_pattern_bindings(pat, out);
        }
    }
}

pub(super) fn lower_case_expr(
    ctx: &mut LowerCtx<'_>,
    scrutinee: HirExprId,
    arms: &SliceRange<HirCaseArm>,
) -> IrExprKind {
    let sema = ctx.sema;
    let mut lowered = Vec::<IrLoweredCaseArm>::new();
    for arm in sema.module().store.case_arms.get(arms.clone()) {
        lowered.extend(lower_case_arm(ctx, arm));
    }
    if lowered.is_empty() {
        invalid_lowering_path("case without emit-compatible arms");
    }
    IrExprKind::Case {
        scrutinee: Box::new(lower_expr(ctx, scrutinee)),
        arms: lowered.into_boxed_slice(),
    }
}

fn lower_case_arm(ctx: &mut LowerCtx<'_>, arm: &HirCaseArm) -> Vec<IrLoweredCaseArm> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let patterns = lower_case_patterns(sema, arm.pat, interner);
    patterns
        .into_iter()
        .map(|pattern| IrLoweredCaseArm {
            pattern,
            guard: arm.guard.map(|guard| lower_expr(ctx, guard)),
            expr: lower_expr(ctx, arm.expr),
        })
        .collect()
}

fn lower_case_patterns(
    sema: &SemaModule,
    pat: HirPatId,
    interner: &Interner,
) -> Vec<IrCasePattern> {
    match &sema.module().store.pats.get(pat).kind {
        HirPatKind::Wildcard => vec![IrCasePattern::Wildcard],
        HirPatKind::Bind { name } => decl_binding_id(sema, *name)
            .map(|binding| {
                vec![IrCasePattern::Bind {
                    binding,
                    name: interner.resolve(name.name).into(),
                }]
            })
            .unwrap_or_default(),
        HirPatKind::Lit { expr } => lower_lit_pattern(sema, *expr).into_iter().collect(),
        HirPatKind::Tuple { items } => lower_product_patterns(
            sema,
            sema.module().store.pat_ids.get(*items),
            interner,
            |items| IrCasePattern::Tuple { items },
        ),
        HirPatKind::Array { items } => lower_product_patterns(
            sema,
            sema.module().store.pat_ids.get(*items),
            interner,
            |items| IrCasePattern::Array { items },
        ),
        HirPatKind::Record { fields } => lower_record_case_patterns(sema, pat, fields, interner),
        HirPatKind::Variant { tag, args } => {
            lower_variant_patterns(sema, pat, *tag, *args, interner)
        }
        HirPatKind::Or { left, right } => {
            let mut out = lower_case_patterns(sema, *left, interner);
            out.extend(lower_case_patterns(sema, *right, interner));
            out
        }
        HirPatKind::As { pat: inner, name } => decl_binding_id(sema, *name)
            .map(|binding| {
                lower_case_patterns(sema, *inner, interner)
                    .into_iter()
                    .map(|pat| IrCasePattern::As {
                        pat: Box::new(pat),
                        binding,
                        name: interner.resolve(name.name).into(),
                    })
                    .collect::<Vec<_>>()
            })
            .unwrap_or_default(),
        HirPatKind::Error => Vec::new(),
    }
}

fn lower_record_case_patterns(
    sema: &SemaModule,
    pat: HirPatId,
    fields: &SliceRange<HirRecordPatField>,
    interner: &Interner,
) -> Vec<IrCasePattern> {
    let Some((indices, _layout, _field_count)) = record_layout_for_ty(
        sema,
        sema.try_pat_ty(pat)
            .unwrap_or_else(|| invalid_lowering_path("pattern type missing for record case")),
        interner,
    ) else {
        return Vec::new();
    };
    let mut acc = vec![Vec::<IrCaseRecordField>::new()];
    for field in sema.module().store.record_pat_fields.get(fields.clone()) {
        let Some(index) = indices.get(&field.name.name).copied() else {
            return Vec::new();
        };
        let nested = field.value.map_or_else(
            || {
                decl_binding_id(sema, field.name)
                    .map(|binding| {
                        vec![IrCasePattern::Bind {
                            binding,
                            name: interner.resolve(field.name.name).into(),
                        }]
                    })
                    .unwrap_or_default()
            },
            |value| lower_case_patterns(sema, value, interner),
        );
        if nested.is_empty() {
            return Vec::new();
        }
        let mut next = Vec::<Vec<IrCaseRecordField>>::new();
        for prefix in &acc {
            for pat in &nested {
                let mut updated = prefix.clone();
                updated.push(IrCaseRecordField::new(index, pat.clone()));
                next.push(updated);
            }
        }
        acc = next;
    }
    acc.into_iter()
        .map(|fields| IrCasePattern::Record {
            fields: fields.into_boxed_slice(),
        })
        .collect()
}

fn lower_product_patterns<F>(
    sema: &SemaModule,
    items: &[HirPatId],
    interner: &Interner,
    ctor: F,
) -> Vec<IrCasePattern>
where
    F: Fn(Box<[IrCasePattern]>) -> IrCasePattern,
{
    let mut acc = vec![Vec::<IrCasePattern>::new()];
    for item in items.iter().copied() {
        let alts = lower_case_patterns(sema, item, interner);
        if alts.is_empty() {
            return Vec::new();
        }
        let mut next = Vec::<Vec<IrCasePattern>>::new();
        for prefix in &acc {
            for alt in &alts {
                let mut updated = prefix.clone();
                updated.push(alt.clone());
                next.push(updated);
            }
        }
        acc = next;
    }
    acc.into_iter()
        .map(|items| ctor(items.into_boxed_slice()))
        .collect()
}

fn lower_variant_patterns(
    sema: &SemaModule,
    pat: HirPatId,
    tag: Ident,
    args: SliceRange<HirPatId>,
    interner: &Interner,
) -> Vec<IrCasePattern> {
    let ty = sema
        .try_pat_ty(pat)
        .unwrap_or_else(|| invalid_lowering_path("pattern type missing for variant case"));
    let data = match &sema.ty(ty).kind {
        HirTyKind::Named { name, .. } => {
            let data_name = interner.resolve(*name);
            sema.data_def(data_name)
        }
        HirTyKind::Sum { left, right } => {
            let synth_name = format!("__sum__{}_{}", left.raw(), right.raw());
            sema.data_def(synth_name.as_str())
        }
        _ => None,
    };
    let Some(data) = data else {
        return Vec::new();
    };
    let tag_name = interner.resolve(tag.name);
    let Some(tag_index) = data.variant_index(tag_name) else {
        return Vec::new();
    };
    let Some(variant_count) = u16::try_from(data.variant_count()).ok() else {
        return Vec::new();
    };

    lower_product_patterns(
        sema,
        sema.module().store.pat_ids.get(args),
        interner,
        |items| IrCasePattern::Variant {
            data_key: data.key().clone(),
            variant_count,
            tag_index,
            args: items,
        },
    )
}

fn lower_lit_pattern(sema: &SemaModule, expr: HirExprId) -> Option<IrCasePattern> {
    let HirExprKind::Lit { lit } = sema.module().store.exprs.get(expr).kind else {
        return None;
    };
    let lit = &sema.module().store.lits.get(lit).kind;
    Some(IrCasePattern::Lit(match lit {
        HirLitKind::Int { raw } => IrLit::Int { raw: raw.clone() },
        HirLitKind::Float { raw } => IrLit::Float { raw: raw.clone() },
        HirLitKind::String { value } => IrLit::String {
            value: value.clone(),
        },
        HirLitKind::Rune { value } => IrLit::Rune { value: *value },
    }))
}
