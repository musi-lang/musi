use music_hir::{HirExprId, HirTyKind};
use music_names::NameBindingId;
use music_sema::{ConstraintEvidence, ConstraintKey, SemaModule};

use crate::api::{IrArg, IrExpr, IrExprKind, IrNameRef, IrOrigin, IrParam};

use super::types::render_ty_name;
use super::{EvidenceBindingMap, LowerCtx, invalid_lowering_path};

fn hidden_evidence_name(owner: &str, index: usize) -> Box<str> {
    format!("__ev::{owner}::{index}").into_boxed_str()
}

pub(super) fn hidden_evidence_params_for_keys(
    owner: &str,
    keys: &[ConstraintKey],
) -> (Vec<IrParam>, EvidenceBindingMap) {
    let mut params = Vec::new();
    let mut bindings = EvidenceBindingMap::new();
    for (index, key) in keys.iter().cloned().enumerate() {
        let name = hidden_evidence_name(owner, index);
        let _ = bindings.insert(key, name.clone());
        params.push(IrParam::synthetic(name));
    }
    (params, bindings)
}

pub(super) fn hidden_evidence_params_for_binding(
    sema: &SemaModule,
    owner: &str,
    binding: Option<NameBindingId>,
) -> (Vec<IrParam>, EvidenceBindingMap) {
    let keys = binding
        .and_then(|binding| sema.binding_evidence_keys(binding))
        .unwrap_or(&[]);
    hidden_evidence_params_for_keys(owner, keys)
}

pub(super) fn push_evidence_bindings(ctx: &mut LowerCtx<'_>, bindings: EvidenceBindingMap) {
    ctx.evidence_bindings.push(bindings);
}

pub(super) fn pop_evidence_bindings(ctx: &mut LowerCtx<'_>) {
    let _ = ctx.evidence_bindings.pop();
}

pub(super) fn lower_evidence_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    evidence: &ConstraintEvidence,
) -> IrExpr {
    match evidence {
        ConstraintEvidence::Param { key } => {
            let Some(name) = resolve_evidence_binding_name(ctx, key) else {
                invalid_lowering_path("missing evidence binding for constraint");
            };
            IrExpr::new(
                origin,
                IrExprKind::Name {
                    binding: None,
                    name,
                    module_target: None,
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
                        module_target: Some(module.clone()),
                    },
                )),
                args: args
                    .iter()
                    .map(|arg| IrArg::new(false, lower_evidence_expr(ctx, origin, arg)))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
        ),
    }
}

fn resolve_evidence_binding_name(ctx: &LowerCtx<'_>, key: &ConstraintKey) -> Option<Box<str>> {
    ctx.evidence_bindings.iter().rev().find_map(|bindings| {
        bindings.get(key).cloned().or_else(|| {
            bindings.iter().find_map(|(candidate, name)| {
                evidence_keys_equiv(ctx, key, candidate).then(|| name.clone())
            })
        })
    })
}

fn evidence_keys_equiv(ctx: &LowerCtx<'_>, left: &ConstraintKey, right: &ConstraintKey) -> bool {
    left.kind == right.kind
        && left.class_key == right.class_key
        && render_ty_name(ctx.sema, left.subject, ctx.interner)
            == render_ty_name(ctx.sema, right.subject, ctx.interner)
        && render_ty_name(ctx.sema, left.value, ctx.interner)
            == render_ty_name(ctx.sema, right.value, ctx.interner)
}

pub(super) fn bind_expr_evidence(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    lowered: IrExpr,
) -> IrExpr {
    let Some(evidence) = ctx.sema.expr_evidence(expr_id) else {
        return lowered;
    };
    if evidence.is_empty() {
        return lowered;
    }
    let IrExprKind::Name {
        binding,
        name,
        module_target,
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
                module_target,
            },
        );
    }
    if module_target.is_none()
        && binding.is_some_and(|binding| !ctx.module_level_bindings.contains(&binding))
    {
        return IrExpr::new(
            origin,
            IrExprKind::Name {
                binding,
                name,
                module_target,
            },
        );
    }
    IrExpr::new(
        origin,
        IrExprKind::ClosureNew {
            callee: IrNameRef {
                binding,
                name,
                module_target,
            },
            captures: evidence
                .iter()
                .map(|item| lower_evidence_expr(ctx, origin, item))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        },
    )
}
