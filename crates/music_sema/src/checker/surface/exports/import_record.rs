use music_hir::{HirExprId, HirExprKind, HirPatId, HirPatKind};
use music_module::ModuleKey;
use music_names::{NameBindingId, NameSite};

use crate::checker::{ModuleState, TypingState};

pub(super) fn export_import_record_target(
    module: &ModuleState,
    typing: &TypingState,
    binding: NameBindingId,
) -> Option<ModuleKey> {
    typing
        .binding_import_record_targets()
        .get(&binding)
        .cloned()
        .or_else(|| {
            binding_value_expr(module, module.resolved.module.root, binding)
                .and_then(|expr| expr_import_record_target(module, typing, expr))
        })
}

fn binding_value_expr(
    module: &ModuleState,
    expr_id: HirExprId,
    binding: NameBindingId,
) -> Option<HirExprId> {
    match module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Sequence { exprs } => module
            .resolved
            .module
            .store
            .expr_ids
            .get(exprs)
            .iter()
            .copied()
            .find_map(|expr| binding_value_expr(module, expr, binding)),
        HirExprKind::Let { pat, value, .. } => pat_binds(module, pat, binding)
            .then_some(value)
            .or_else(|| binding_value_expr(module, value, binding)),
        _ => None,
    }
}

fn pat_binds(module: &ModuleState, pat_id: HirPatId, binding: NameBindingId) -> bool {
    match module.resolved.module.store.pats.get(pat_id).kind.clone() {
        HirPatKind::Bind { name } => {
            let site = NameSite::new(module.resolved.module.source_id, name.span);
            module
                .binding_id_at_site(site)
                .is_some_and(|found| found == binding)
        }
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => module
            .resolved
            .module
            .store
            .pat_ids
            .get(items)
            .iter()
            .copied()
            .any(|item| pat_binds(module, item, binding)),
        HirPatKind::Variant { args, .. } => module
            .resolved
            .module
            .store
            .variant_pat_args
            .get(args)
            .iter()
            .any(|item| pat_binds(module, item.pat, binding)),
        HirPatKind::Record { fields } => module
            .resolved
            .module
            .store
            .record_pat_fields
            .get(fields)
            .iter()
            .any(|field| {
                field.value.map_or_else(
                    || {
                        let site = NameSite::new(module.resolved.module.source_id, field.name.span);
                        module
                            .binding_id_at_site(site)
                            .is_some_and(|found| found == binding)
                    },
                    |value| pat_binds(module, value, binding),
                )
            }),
        HirPatKind::Or { left, right } => {
            pat_binds(module, left, binding) || pat_binds(module, right, binding)
        }
        HirPatKind::As { pat, name } => {
            pat_binds(module, pat, binding) || {
                let site = NameSite::new(module.resolved.module.source_id, name.span);
                module
                    .binding_id_at_site(site)
                    .is_some_and(|found| found == binding)
            }
        }
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Lit { .. } => false,
    }
}

fn expr_import_record_target(
    module: &ModuleState,
    typing: &TypingState,
    expr_id: HirExprId,
) -> Option<ModuleKey> {
    match module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Import { .. } => {
            let span = module.resolved.module.store.exprs.get(expr_id).origin.span;
            module
                .resolved
                .imports
                .iter()
                .find_map(|import| (import.span == span).then_some(import.to.clone()))
        }
        HirExprKind::Name { name } => {
            let site = NameSite::new(module.resolved.module.source_id, name.span);
            let binding = module.resolved.names.refs.get(&site).copied()?;
            typing
                .binding_import_record_targets()
                .get(&binding)
                .cloned()
        }
        _ => None,
    }
}
