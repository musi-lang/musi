use std::collections::HashSet;

use music_hir::{HirExprId, HirExprKind, HirPatKind};
use music_names::NameBindingId;
use music_sema::SemaModule;

pub(super) fn collect_module_level_bindings(sema: &SemaModule) -> HashSet<NameBindingId> {
    let mut bindings = HashSet::new();
    collect_module_level_bindings_from_expr(sema, sema.module().root, &mut bindings);
    bindings
}

fn collect_module_level_bindings_from_expr(
    sema: &SemaModule,
    expr_id: HirExprId,
    out: &mut HashSet<NameBindingId>,
) {
    match &sema.module().store.exprs.get(expr_id).kind {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            for expr in sema.module().store.expr_ids.get(*exprs).iter().copied() {
                collect_module_level_bindings_from_expr(sema, expr, out);
            }
        }
        HirExprKind::Export { expr, .. } | HirExprKind::Attributed { expr, .. } => {
            collect_module_level_bindings_from_expr(sema, *expr, out);
        }
        HirExprKind::Let { pat, .. } => {
            if let HirPatKind::Bind { name } = sema.module().store.pats.get(*pat).kind {
                if let Some(binding) = super::decl_binding_id(sema, name) {
                    let _ = out.insert(binding);
                }
            }
        }
        HirExprKind::Foreign { decls, .. } => {
            for decl in sema.module().store.foreign_decls.get(decls.clone()) {
                if let Some(binding) = super::decl_binding_id(sema, decl.name) {
                    if sema.is_gated_binding(binding) {
                        continue;
                    }
                    let _ = out.insert(binding);
                }
            }
        }
        _ => {}
    }
}

