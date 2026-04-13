use music_hir::{HirExprId, HirExprKind, HirTyKind};

use crate::api::{IrAssignTarget, IrExprKind};

use super::{LowerCtx, lower_expr, lower_index_expr, record_layout_for_ty, use_binding_id};

pub(super) fn lower_assign_expr(
    ctx: &mut LowerCtx<'_>,
    left: HirExprId,
    right: HirExprId,
) -> Result<IrExprKind, Box<str>> {
    let target = lower_assign_target(ctx, left)?;
    Ok(IrExprKind::Assign {
        target: Box::new(target),
        value: Box::new(lower_expr(ctx, right)),
    })
}

fn lower_assign_target(
    ctx: &mut LowerCtx<'_>,
    expr: HirExprId,
) -> Result<IrAssignTarget, Box<str>> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    match &sema.module().store.exprs.get(expr).kind {
        HirExprKind::Name { name } => Ok(IrAssignTarget::Binding {
            binding: use_binding_id(sema, *name),
            name: interner.resolve(name.name).into(),
            module_target: sema.expr_module_target(expr).cloned(),
        }),
        HirExprKind::Index { base, args } => {
            let IrExprKind::Index { base, indices } = lower_index_expr(ctx, *base, *args) else {
                return Err("unsupported assignment target".into());
            };
            Ok(IrAssignTarget::Index { base, indices })
        }
        HirExprKind::Field { base, name, .. } => {
            let Some(base_ty) = sema.try_expr_ty(*base) else {
                return Err("assignment field base type missing".into());
            };
            let record_ty = match sema.ty(base_ty).kind {
                HirTyKind::Mut { inner } => inner,
                _ => base_ty,
            };
            if matches!(sema.ty(record_ty).kind, HirTyKind::Record { .. }) {
                let Some((indices, _layout, _count)) =
                    record_layout_for_ty(sema, record_ty, interner)
                else {
                    return Err("assignment field base type missing".into());
                };
                let Some(index) = indices.get(interner.resolve(name.name)).copied() else {
                    return Err("unsupported assignment target".into());
                };
                return Ok(IrAssignTarget::RecordField {
                    base: Box::new(lower_expr(ctx, *base)),
                    index,
                });
            }

            let Some(module_target) = sema
                .expr_module_target(expr)
                .cloned()
                .or_else(|| sema.expr_module_target(*base).cloned())
            else {
                return Err("unsupported assignment target".into());
            };
            Ok(IrAssignTarget::Binding {
                binding: None,
                name: interner.resolve(name.name).into(),
                module_target: Some(module_target),
            })
        }
        _ => Err("unsupported assignment target".into()),
    }
}
