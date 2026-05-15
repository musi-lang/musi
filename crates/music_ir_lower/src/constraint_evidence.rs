use music_hir::{HirExprId, HirTyKind};
use music_names::NameBindingId;
use music_sema::{ConstraintEvidence, ConstraintKey, SemaModule};

use music_ir::{IrArg, IrExpr, IrExprKind, IrNameRef, IrOrigin, IrParam};

use super::types::render_ty_name;
use super::{ConstraintEvidenceBindingMap, LowerCtx, lowering_invariant_violation};

pub(crate) fn hidden_constraint_evidence_name(owner: &str, index: usize) -> Box<str> {
    format!("__evidence::{owner}::{index}").into_boxed_str()
}

pub(crate) fn hidden_constraint_evidence_params_for_keys(
    owner: &str,
    keys: &[ConstraintKey],
) -> (Vec<IrParam>, ConstraintEvidenceBindingMap) {
    let mut params = Vec::new();
    let mut bindings = ConstraintEvidenceBindingMap::new();
    for (index, key) in keys.iter().cloned().enumerate() {
        let name = hidden_constraint_evidence_name(owner, index);
        let _ = bindings.insert(key, name.clone());
        params.push(IrParam::synthetic(name));
    }
    (params, bindings)
}

pub(crate) fn hidden_constraint_evidence_params_for_binding(
    sema: &SemaModule,
    owner: &str,
    binding: Option<NameBindingId>,
) -> (Vec<IrParam>, ConstraintEvidenceBindingMap) {
    let keys = binding
        .and_then(|binding| sema.binding_constraint_keys(binding))
        .unwrap_or(&[]);
    hidden_constraint_evidence_params_for_keys(owner, keys)
}

pub(crate) fn push_constraint_evidence_bindings(
    ctx: &mut LowerCtx<'_>,
    bindings: ConstraintEvidenceBindingMap,
) {
    ctx.constraint_evidence_bindings.push(bindings);
}

pub(crate) fn pop_constraint_evidence_bindings(ctx: &mut LowerCtx<'_>) {
    let _ = ctx.constraint_evidence_bindings.pop();
}

pub(crate) fn lower_constraint_evidence_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    constraint_evidence: &ConstraintEvidence,
) -> IrExpr {
    match constraint_evidence {
        ConstraintEvidence::Param { key } => {
            let Some(name) = resolve_constraint_evidence_binding_name(ctx, key) else {
                lowering_invariant_violation("missing evidence binding for constraint");
            };
            IrExpr::new(
                origin,
                IrExprKind::Name {
                    binding: None,
                    name,
                    import_record_target: None,
                },
            )
        }
        ConstraintEvidence::Provider { module, name, args } => IrExpr::new(
            origin,
            IrExprKind::Call {
                callee: Box::new(IrExpr::new(
                    origin,
                    IrExprKind::Name {
                        binding: None,
                        name: name.clone(),
                        import_record_target: Some(module.clone()),
                    },
                )),
                args: args
                    .iter()
                    .map(|arg| IrArg::new(false, lower_constraint_evidence_expr(ctx, origin, arg)))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
        ),
    }
}

pub(crate) fn resolve_constraint_evidence_binding_name(
    ctx: &LowerCtx<'_>,
    key: &ConstraintKey,
) -> Option<Box<str>> {
    ctx.constraint_evidence_bindings
        .iter()
        .rev()
        .find_map(|bindings| {
            bindings.get(key).cloned().or_else(|| {
                bindings.iter().find_map(|(candidate, name)| {
                    constraint_keys_equiv(ctx, key, candidate).then(|| name.clone())
                })
            })
        })
}

pub(crate) fn constraint_keys_equiv(
    ctx: &LowerCtx<'_>,
    left: &ConstraintKey,
    right: &ConstraintKey,
) -> bool {
    left.kind == right.kind
        && left.shape_key == right.shape_key
        && render_ty_name(ctx.sema, left.subject, ctx.interner)
            == render_ty_name(ctx.sema, right.subject, ctx.interner)
        && render_ty_name(ctx.sema, left.value, ctx.interner)
            == render_ty_name(ctx.sema, right.value, ctx.interner)
}

pub(crate) fn bind_expr_constraint_evidence(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    lowered: IrExpr,
) -> IrExpr {
    let Some(evidence) = ctx.sema.expr_constraint_evidence(expr_id) else {
        return lowered;
    };
    if evidence.is_empty() {
        return lowered;
    }
    let IrExprKind::Name {
        binding,
        name,
        import_record_target,
    } = lowered.kind
    else {
        return lowered;
    };
    let is_callable = ctx
        .sema
        .try_expr_ty(expr_id)
        .is_some_and(|ty| matches!(ctx.sema.ty(ty).kind, HirTyKind::Arrow { .. }));
    if !is_callable {
        return IrExpr::new(
            origin,
            IrExprKind::Name {
                binding,
                name,
                import_record_target,
            },
        );
    }
    if import_record_target.is_none()
        && binding.is_some_and(|binding| !ctx.module_level_bindings.contains(&binding))
    {
        return IrExpr::new(
            origin,
            IrExprKind::Name {
                binding,
                name,
                import_record_target,
            },
        );
    }
    IrExpr::new(
        origin,
        IrExprKind::ClosureNew {
            callee: IrNameRef {
                binding,
                name,
                import_record_target,
            },
            captures: evidence
                .iter()
                .map(|item| lower_constraint_evidence_expr(ctx, origin, item))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        },
    )
}
