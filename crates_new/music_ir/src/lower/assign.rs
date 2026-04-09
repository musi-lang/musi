use music_hir::{HirExprId, HirExprKind, HirTyKind};

use crate::api::{IrAssignTarget, IrExprKind};

use super::{LowerCtx, lower_expr, lower_index_expr, record_layout_for_ty, use_binding_id};

pub(super) fn lower_assign_expr(
    ctx: &mut LowerCtx<'_>,
    left: HirExprId,
    right: HirExprId,
) -> IrExprKind {
    let Some(target) = lower_assign_target(ctx, left) else {
        return IrExprKind::Unsupported {
            description: "unsupported assignment target".into(),
        };
    };
    IrExprKind::Assign {
        target: Box::new(target),
        value: Box::new(lower_expr(ctx, right)),
    }
}

fn lower_assign_target(ctx: &mut LowerCtx<'_>, expr: HirExprId) -> Option<IrAssignTarget> {
    let sema = ctx.sema;
    let interner = ctx.interner;
    match &sema.module().store.exprs.get(expr).kind {
        HirExprKind::Name { name } => Some(IrAssignTarget::Binding {
            binding: use_binding_id(sema, *name),
            name: interner.resolve(name.name).into(),
            module_target: sema.expr_module_target(expr).cloned(),
        }),
        HirExprKind::Index { base, args } => {
            let IrExprKind::Index { base, index } = lower_index_expr(ctx, *base, *args) else {
                return None;
            };
            Some(IrAssignTarget::Index { base, index })
        }
        HirExprKind::Field { base, name, .. } => {
            let base_ty = sema.expr_ty(*base);
            let record_ty = match sema.ty(base_ty).kind {
                HirTyKind::Mut { inner } => inner,
                _ => base_ty,
            };
            if matches!(sema.ty(record_ty).kind, HirTyKind::Record { .. }) {
                let (indices, _layout, _count) = record_layout_for_ty(sema, record_ty, interner)?;
                let index = indices.get(&name.name).copied()?;
                return Some(IrAssignTarget::RecordField {
                    base: Box::new(lower_expr(ctx, *base)),
                    index,
                });
            }

            let module_target = sema
                .expr_module_target(expr)
                .cloned()
                .or_else(|| sema.expr_module_target(*base).cloned())?;
            Some(IrAssignTarget::Binding {
                binding: None,
                name: interner.resolve(name.name).into(),
                module_target: Some(module_target),
            })
        }
        _ => None,
    }
}
