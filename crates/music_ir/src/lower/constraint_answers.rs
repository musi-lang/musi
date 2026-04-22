use music_hir::{HirExprId, HirTyKind};
use music_names::NameBindingId;
use music_sema::{ConstraintAnswer, ConstraintKey, SemaModule};

use crate::api::{IrArg, IrExpr, IrExprKind, IrNameRef, IrOrigin, IrParam};

use super::types::render_ty_name;
use super::{ConstraintAnswerBindingMap, LowerCtx, lowering_invariant_violation};

fn hidden_constraint_answer_name(owner: &str, index: usize) -> Box<str> {
    format!("__answer::{owner}::{index}").into_boxed_str()
}

pub(super) fn hidden_constraint_answer_params_for_keys(
    owner: &str,
    keys: &[ConstraintKey],
) -> (Vec<IrParam>, ConstraintAnswerBindingMap) {
    let mut params = Vec::new();
    let mut bindings = ConstraintAnswerBindingMap::new();
    for (index, key) in keys.iter().cloned().enumerate() {
        let name = hidden_constraint_answer_name(owner, index);
        let _ = bindings.insert(key, name.clone());
        params.push(IrParam::synthetic(name));
    }
    (params, bindings)
}

pub(super) fn hidden_constraint_answer_params_for_binding(
    sema: &SemaModule,
    owner: &str,
    binding: Option<NameBindingId>,
) -> (Vec<IrParam>, ConstraintAnswerBindingMap) {
    let keys = binding
        .and_then(|binding| sema.binding_constraint_keys(binding))
        .unwrap_or(&[]);
    hidden_constraint_answer_params_for_keys(owner, keys)
}

pub(super) fn push_constraint_answer_bindings(
    ctx: &mut LowerCtx<'_>,
    bindings: ConstraintAnswerBindingMap,
) {
    ctx.constraint_answer_bindings.push(bindings);
}

pub(super) fn pop_constraint_answer_bindings(ctx: &mut LowerCtx<'_>) {
    let _ = ctx.constraint_answer_bindings.pop();
}

pub(super) fn lower_constraint_answer_expr(
    ctx: &mut LowerCtx<'_>,
    origin: IrOrigin,
    constraint_answer: &ConstraintAnswer,
) -> IrExpr {
    match constraint_answer {
        ConstraintAnswer::Param { key } => {
            let Some(name) = resolve_constraint_answer_binding_name(ctx, key) else {
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
        ConstraintAnswer::Provider { module, name, args } => IrExpr::new(
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
                    .map(|arg| IrArg::new(false, lower_constraint_answer_expr(ctx, origin, arg)))
                    .collect::<Vec<_>>()
                    .into_boxed_slice(),
            },
        ),
    }
}

fn resolve_constraint_answer_binding_name(
    ctx: &LowerCtx<'_>,
    key: &ConstraintKey,
) -> Option<Box<str>> {
    ctx.constraint_answer_bindings
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

fn constraint_keys_equiv(ctx: &LowerCtx<'_>, left: &ConstraintKey, right: &ConstraintKey) -> bool {
    left.kind == right.kind
        && left.shape_key == right.shape_key
        && render_ty_name(ctx.sema, left.subject, ctx.interner)
            == render_ty_name(ctx.sema, right.subject, ctx.interner)
        && render_ty_name(ctx.sema, left.value, ctx.interner)
            == render_ty_name(ctx.sema, right.value, ctx.interner)
}

pub(super) fn bind_expr_constraint_answers(
    ctx: &mut LowerCtx<'_>,
    expr_id: HirExprId,
    origin: IrOrigin,
    lowered: IrExpr,
) -> IrExpr {
    let Some(evidence) = ctx.sema.expr_constraint_answers(expr_id) else {
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
                .map(|item| lower_constraint_answer_expr(ctx, origin, item))
                .collect::<Vec<_>>()
                .into_boxed_slice(),
        },
    )
}
