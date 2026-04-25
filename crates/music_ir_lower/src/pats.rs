use super::{
    BoundNameSet, HirExprId, HirExprKind, HirLitKind, HirMatchArm, HirPatId, HirPatKind,
    HirRecordPatField, HirTyKind, Ident, Interner, IrCasePattern, IrCaseRecordField, IrExprKind,
    IrLit, IrLoweredMatchArm, LowerCtx, SemaDataVariantDef, SemaModule, SliceRange,
    decl_binding_id, lower_expr, lowering_invariant_violation, record_layout_for_ty,
};
use music_hir::HirVariantPatArg;

pub(crate) fn collect_pattern_bindings(pattern: &IrCasePattern, out: &mut BoundNameSet) {
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

pub(crate) fn lower_match_expr(
    ctx: &mut LowerCtx<'_>,
    scrutinee: HirExprId,
    arms: &SliceRange<HirMatchArm>,
) -> IrExprKind {
    let sema = ctx.sema;
    let mut lowered = Vec::<IrLoweredMatchArm>::new();
    for arm in sema.module().store.match_arms.get(arms.clone()) {
        lowered.extend(lower_match_arm(ctx, arm));
    }
    if lowered.is_empty() {
        lowering_invariant_violation("case without emit-compatible arms");
    }
    IrExprKind::Match {
        scrutinee: Box::new(lower_expr(ctx, scrutinee)),
        arms: lowered.into_boxed_slice(),
    }
}

pub(crate) fn lower_match_arm(ctx: &mut LowerCtx<'_>, arm: &HirMatchArm) -> Vec<IrLoweredMatchArm> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    let patterns = lower_case_patterns(sema, arm.pat, interner);
    patterns
        .into_iter()
        .map(|pattern| IrLoweredMatchArm {
            pattern,
            guard: arm.guard.map(|guard| lower_expr(ctx, guard)),
            expr: lower_expr(ctx, arm.expr),
        })
        .collect()
}

pub(crate) fn lower_case_patterns(
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
            lower_variant_patterns(sema, pat, *tag, args.clone(), interner)
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

pub(crate) fn lower_record_case_patterns(
    sema: &SemaModule,
    pat: HirPatId,
    fields: &SliceRange<HirRecordPatField>,
    interner: &Interner,
) -> Vec<IrCasePattern> {
    let Some((indices, _layout, _field_count)) = record_layout_for_ty(
        sema,
        sema.try_pat_ty(pat).unwrap_or_else(|| {
            lowering_invariant_violation("pattern type missing for record case")
        }),
        interner,
    ) else {
        return Vec::new();
    };
    let mut acc = vec![Vec::<IrCaseRecordField>::new()];
    for field in sema.module().store.record_pat_fields.get(fields.clone()) {
        let Some(index) = indices.get(interner.resolve(field.name.name)).copied() else {
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

pub(crate) fn lower_product_patterns<F>(
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

pub(crate) fn lower_variant_patterns(
    sema: &SemaModule,
    pat: HirPatId,
    tag: Ident,
    args: SliceRange<HirVariantPatArg>,
    interner: &Interner,
) -> Vec<IrCasePattern> {
    let ty = sema
        .try_pat_ty(pat)
        .unwrap_or_else(|| lowering_invariant_violation("pattern type missing for variant case"));
    let data_def = match &sema.ty(ty).kind {
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
    let Some(data_def) = data_def else {
        return Vec::new();
    };
    let tag_name = interner.resolve(tag.name);
    let Some(tag_index) = data_def.variant_index(tag_name) else {
        return Vec::new();
    };
    let Some(variant_count) = u16::try_from(data_def.variant_count()).ok() else {
        return Vec::new();
    };
    let Some(variant) = data_def.variant(tag_name) else {
        return Vec::new();
    };
    let pat_args = ordered_variant_pat_args(sema, variant, args, interner);

    lower_product_patterns(sema, &pat_args, interner, |items| IrCasePattern::Variant {
        data_key: data_def.key().clone(),
        variant_count,
        tag_index,
        tag_value: variant.tag(),
        args: items,
    })
}

pub(crate) fn ordered_variant_pat_args(
    sema: &SemaModule,
    variant: &SemaDataVariantDef,
    args: SliceRange<HirVariantPatArg>,
    interner: &Interner,
) -> Vec<HirPatId> {
    let arg_nodes = sema.module().store.variant_pat_args.get(args);
    if !variant.field_names().iter().any(Option::is_some) {
        return arg_nodes.iter().map(|arg| arg.pat).collect();
    }
    let mut ordered = vec![None; variant.field_names().len()];
    for arg in arg_nodes {
        let Some(name) = variant_pat_arg_name(sema, variant, arg, interner) else {
            lowering_invariant_violation("named variant pattern missing field name after sema");
        };
        let Some(index) = variant
            .field_names()
            .iter()
            .position(|field| field.as_deref() == Some(name.as_ref()))
        else {
            lowering_invariant_violation("unknown named variant pattern field after sema");
        };
        ordered[index] = Some(arg.pat);
    }
    ordered
        .into_iter()
        .map(|pat| {
            pat.unwrap_or_else(|| {
                lowering_invariant_violation("missing named variant pattern field after sema")
            })
        })
        .collect()
}

pub(crate) fn variant_pat_arg_name(
    sema: &SemaModule,
    variant: &SemaDataVariantDef,
    arg: &HirVariantPatArg,
    interner: &Interner,
) -> Option<Box<str>> {
    if let Some(name) = arg.name {
        return Some(interner.resolve(name.name).into());
    }
    let HirPatKind::Bind { name } = sema.module().store.pats.get(arg.pat).kind else {
        return None;
    };
    let binder_name = interner.resolve(name.name);
    variant
        .field_names()
        .iter()
        .flatten()
        .any(|field_name| field_name.as_ref() == binder_name)
        .then(|| binder_name.into())
}

pub(crate) fn lower_lit_pattern(sema: &SemaModule, expr: HirExprId) -> Option<IrCasePattern> {
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
