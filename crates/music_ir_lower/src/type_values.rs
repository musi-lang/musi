use super::{
    HirBinaryOp, HirExprId, HirExprKind, HirLitKind, HirPrefixOp, HirTyKind, Interner, SemaModule,
    is_builtin_type_name_symbol, lowering_invariant_violation, use_binding_id,
};

pub(crate) fn is_type_value_expr(
    sema: &SemaModule,
    expr_id: HirExprId,
    interner: &Interner,
) -> bool {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Error => true,
        HirExprKind::Name { name } => {
            if matches!(
                sema.ty(sema
                    .try_expr_ty(expr_id)
                    .unwrap_or_else(|| lowering_invariant_violation(
                        "expr type missing for name ref"
                    )))
                .kind,
                HirTyKind::Type
            ) {
                return true;
            }
            if sema.expr_import_record_target(expr_id).is_some() {
                return false;
            }
            let symbol_text = interner.resolve(name.name);
            match use_binding_id(sema, *name) {
                None => true,
                Some(_) if is_builtin_type_name_symbol(symbol_text) => true,
                Some(_) => false,
            }
        }
        HirExprKind::Tuple { items } => sema
            .module()
            .store
            .expr_ids
            .get(*items)
            .iter()
            .copied()
            .all(|item| is_type_value_expr(sema, item, interner)),
        HirExprKind::ArrayTy { item, .. } => is_type_value_expr(sema, *item, interner),
        HirExprKind::Pi { binder_ty, ret, .. } => {
            is_type_value_expr(sema, *binder_ty, interner)
                && is_type_value_expr(sema, *ret, interner)
        }
        HirExprKind::Apply { callee, args } => {
            is_type_value_expr(sema, *callee, interner)
                && sema
                    .module()
                    .store
                    .expr_ids
                    .get(*args)
                    .iter()
                    .copied()
                    .all(|arg| type_apply_arg_expr(sema, arg, interner))
        }
        HirExprKind::Binary { op, left, right } => {
            matches!(
                op,
                HirBinaryOp::Add | HirBinaryOp::Arrow | HirBinaryOp::EffectArrow
            ) && is_type_value_expr(sema, *left, interner)
                && is_type_value_expr(sema, *right, interner)
        }
        HirExprKind::Prefix {
            op: HirPrefixOp::Mut | HirPrefixOp::Comptime | HirPrefixOp::Any | HirPrefixOp::Some,
            expr,
        } => is_type_value_expr(sema, *expr, interner),
        HirExprKind::Record { items } => sema
            .module()
            .store
            .record_items
            .get(items.clone())
            .iter()
            .all(|item| is_type_value_expr(sema, item.value, interner)),
        HirExprKind::Field { .. } => matches!(
            sema.ty(sema
                .try_expr_ty(expr_id)
                .unwrap_or_else(|| lowering_invariant_violation(
                    "expr type missing for field ref"
                )))
            .kind,
            HirTyKind::Type
        ),
        _ => false,
    }
}

pub(crate) fn type_apply_arg_expr(
    sema: &SemaModule,
    expr_id: HirExprId,
    interner: &Interner,
) -> bool {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Lit { lit } => matches!(
            sema.module().store.lits.get(*lit).kind,
            HirLitKind::Int { .. }
        ),
        _ => is_type_value_expr(sema, expr_id, interner),
    }
}
