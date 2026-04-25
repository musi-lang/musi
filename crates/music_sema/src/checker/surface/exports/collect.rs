use music_arena::SliceRange;
use music_hir::{
    HirArg, HirArrayItem, HirAttr, HirExprId, HirExprKind, HirFieldDef, HirHandleClause,
    HirMatchArm, HirMemberDef, HirParam, HirPatId, HirPatKind, HirRecordItem, HirTemplatePart,
    HirVariantDef,
};
use music_names::{Ident, Interner, NameSite};

use super::model::{ExportBinding, ExportGiven, ModuleExports};
use crate::checker::ModuleState;

pub(in crate::checker::surface) fn collect_module_exports(
    module: &ModuleState,
    interner: &Interner,
) -> ModuleExports {
    let mut exports = ModuleExports::default();
    let mut attr_stack = Vec::<HirAttr>::new();
    collect_expr(
        module,
        interner,
        module.resolved.module.root,
        &mut exports,
        &mut attr_stack,
    );
    exports
}

fn collect_expr(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    let expr = module.resolved.module.store.exprs.get(expr_id);
    let start = attr_stack.len();
    if !expr.mods.attrs.is_empty() {
        attr_stack.extend_from_slice(
            module
                .resolved
                .module
                .store
                .attrs
                .get(expr.mods.attrs.clone()),
        );
    }
    if let Some(export_mod) = &expr.mods.export {
        collect_direct_exports(
            module,
            interner,
            expr_id,
            export_mod.opaque,
            exports,
            attr_stack,
        );
    }

    collect_exports_from_kind(module, interner, &expr.kind, exports, attr_stack);
    attr_stack.truncate(start);
}

fn collect_exports_from_kind(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    if collect_aggregate_exports(module, interner, kind, exports, attr_stack)
        || collect_call_like_exports(module, interner, kind, exports, attr_stack)
        || collect_decl_or_control_exports(module, interner, kind, exports, attr_stack)
    {}
}

fn collect_aggregate_exports(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) -> bool {
    match kind {
        HirExprKind::Sequence { exprs } | HirExprKind::Tuple { items: exprs } => {
            collect_expr_id_range(module, interner, *exprs, exports, attr_stack);
        }
        HirExprKind::Array { items } => {
            collect_array_exprs(module, interner, items, exports, attr_stack);
        }
        HirExprKind::Record { items } => {
            collect_recordish_exprs(module, interner, items, None, exports, attr_stack);
        }
        HirExprKind::RecordUpdate { base, items } => {
            collect_recordish_exprs(module, interner, items, Some(*base), exports, attr_stack);
        }
        HirExprKind::Template { parts } => {
            collect_template_exprs(module, interner, parts, exports, attr_stack);
        }
        _ => return false,
    }
    true
}

fn collect_call_like_exports(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) -> bool {
    match kind {
        HirExprKind::Pi { binder_ty, ret, .. } => {
            collect_pair_exprs(module, interner, *binder_ty, *ret, exports, attr_stack);
        }
        HirExprKind::Lambda { body, .. }
        | HirExprKind::Import { arg: body }
        | HirExprKind::Request { expr: body }
        | HirExprKind::Unsafe { body } => {
            collect_expr(module, interner, *body, exports, attr_stack);
        }
        HirExprKind::Pin { value, body, .. } => {
            collect_expr(module, interner, *value, exports, attr_stack);
            collect_expr(module, interner, *body, exports, attr_stack);
        }
        HirExprKind::Call { callee, args } => {
            collect_expr(module, interner, *callee, exports, attr_stack);
            collect_arg_exprs(module, interner, args, exports, attr_stack);
        }
        HirExprKind::Apply { callee, args } | HirExprKind::Index { base: callee, args } => {
            collect_expr(module, interner, *callee, exports, attr_stack);
            collect_expr_id_range(module, interner, *args, exports, attr_stack);
        }
        HirExprKind::Field { base, .. }
        | HirExprKind::TypeTest { base, .. }
        | HirExprKind::TypeCast { base, .. }
        | HirExprKind::Prefix { expr: base, .. }
        | HirExprKind::PartialRange { expr: base, .. } => {
            collect_expr(module, interner, *base, exports, attr_stack);
        }
        HirExprKind::Binary { left, right, .. } => {
            collect_pair_exprs(module, interner, *left, *right, exports, attr_stack);
        }
        _ => return false,
    }
    true
}

fn collect_decl_or_control_exports(
    module: &ModuleState,
    interner: &Interner,
    kind: &HirExprKind,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) -> bool {
    match kind {
        HirExprKind::Let { value, .. } => {
            collect_expr(module, interner, *value, exports, attr_stack);
        }
        HirExprKind::Match { scrutinee, arms } => {
            collect_match_exports(module, interner, *scrutinee, arms, exports, attr_stack);
        }
        HirExprKind::Data { variants, fields } => {
            collect_data_exports(module, interner, variants, fields, exports, attr_stack);
        }
        HirExprKind::Effect { members } | HirExprKind::Shape { members, .. } => {
            collect_member_exports(module, interner, members, exports, attr_stack);
        }
        HirExprKind::Given {
            capability,
            members,
            ..
        } => {
            collect_expr(module, interner, *capability, exports, attr_stack);
            collect_member_exports(module, interner, members, exports, attr_stack);
        }
        HirExprKind::AnswerLit { clauses, .. } => {
            collect_handle_clause_exports(module, interner, clauses, exports, attr_stack);
        }
        HirExprKind::Handle { expr, handler } => {
            collect_expr(module, interner, *expr, exports, attr_stack);
            collect_expr(module, interner, *handler, exports, attr_stack);
        }
        HirExprKind::Resume { expr } => {
            collect_optional_expr(module, interner, *expr, exports, attr_stack);
        }
        HirExprKind::Quote { .. }
        | HirExprKind::Splice { .. }
        | HirExprKind::Error
        | HirExprKind::Name { .. }
        | HirExprKind::Lit { .. }
        | HirExprKind::ArrayTy { .. }
        | HirExprKind::AnswerTy { .. }
        | HirExprKind::Variant { .. } => {}
        _ => return false,
    }
    true
}

fn collect_exprs<I>(
    module: &ModuleState,
    interner: &Interner,
    exprs: I,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) where
    I: IntoIterator<Item = HirExprId>,
{
    for expr_id in exprs {
        collect_expr(module, interner, expr_id, exports, attr_stack);
    }
}

fn collect_optional_expr(
    module: &ModuleState,
    interner: &Interner,
    expr_id: Option<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(module, interner, expr_id, exports, attr_stack);
}

fn collect_expr_id_range(
    module: &ModuleState,
    interner: &Interner,
    exprs: SliceRange<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .expr_ids
            .get(exprs)
            .iter()
            .copied(),
        exports,
        attr_stack,
    );
}

fn collect_pair_exprs(
    module: &ModuleState,
    interner: &Interner,
    left: HirExprId,
    right: HirExprId,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(module, interner, [left, right], exports, attr_stack);
}

fn collect_array_exprs(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirArrayItem>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .array_items
            .get(items.clone())
            .iter()
            .map(|item| item.expr),
        exports,
        attr_stack,
    );
}

fn collect_arg_exprs(
    module: &ModuleState,
    interner: &Interner,
    args: &SliceRange<HirArg>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .args
            .get(args.clone())
            .iter()
            .map(|arg| arg.expr),
        exports,
        attr_stack,
    );
}

fn collect_template_exprs(
    module: &ModuleState,
    interner: &Interner,
    parts: &SliceRange<HirTemplatePart>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_exprs(
        module,
        interner,
        module
            .resolved
            .module
            .store
            .template_parts
            .get(parts.clone())
            .iter()
            .filter_map(|part| match part {
                HirTemplatePart::Expr { expr } => Some(*expr),
                HirTemplatePart::Text { .. } => None,
            }),
        exports,
        attr_stack,
    );
}

fn collect_record_item_exports(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirRecordItem>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for item in module.resolved.module.store.record_items.get(items.clone()) {
        collect_expr(module, interner, item.value, exports, attr_stack);
    }
}

fn collect_recordish_exprs(
    module: &ModuleState,
    interner: &Interner,
    items: &SliceRange<HirRecordItem>,
    base: Option<HirExprId>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_record_item_exports(module, interner, items, exports, attr_stack);
    collect_optional_expr(module, interner, base, exports, attr_stack);
}

fn collect_match_exports(
    module: &ModuleState,
    interner: &Interner,
    scrutinee: HirExprId,
    arms: &SliceRange<HirMatchArm>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    collect_expr(module, interner, scrutinee, exports, attr_stack);
    for arm in module.resolved.module.store.match_arms.get(arms.clone()) {
        collect_optional_expr(module, interner, arm.guard, exports, attr_stack);
        collect_expr(module, interner, arm.expr, exports, attr_stack);
    }
}

fn collect_data_exports(
    module: &ModuleState,
    interner: &Interner,
    variants: &SliceRange<HirVariantDef>,
    fields: &SliceRange<HirFieldDef>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for variant in module.resolved.module.store.variants.get(variants.clone()) {
        for field in module
            .resolved
            .module
            .store
            .variant_fields
            .get(variant.fields.clone())
        {
            collect_expr(module, interner, field.ty, exports, attr_stack);
        }
        collect_optional_expr(module, interner, variant.value, exports, attr_stack);
    }
    for field in module.resolved.module.store.fields.get(fields.clone()) {
        collect_expr(module, interner, field.ty, exports, attr_stack);
        collect_optional_expr(module, interner, field.value, exports, attr_stack);
    }
}

fn collect_param_exports(
    module: &ModuleState,
    interner: &Interner,
    params: &SliceRange<HirParam>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for param in module.resolved.module.store.params.get(params.clone()) {
        collect_optional_expr(module, interner, param.ty, exports, attr_stack);
        collect_optional_expr(module, interner, param.default, exports, attr_stack);
    }
}

fn collect_member_exports(
    module: &ModuleState,
    interner: &Interner,
    members: &SliceRange<HirMemberDef>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for member in module.resolved.module.store.members.get(members.clone()) {
        collect_param_exports(module, interner, &member.params, exports, attr_stack);
        collect_optional_expr(module, interner, member.sig, exports, attr_stack);
        collect_optional_expr(module, interner, member.value, exports, attr_stack);
    }
}

fn collect_handle_clause_exports(
    module: &ModuleState,
    interner: &Interner,
    clauses: &SliceRange<HirHandleClause>,
    exports: &mut ModuleExports,
    attr_stack: &mut Vec<HirAttr>,
) {
    for clause in module
        .resolved
        .module
        .store
        .handle_clauses
        .get(clauses.clone())
    {
        collect_expr(module, interner, clause.body, exports, attr_stack);
    }
}

fn collect_direct_exports(
    module: &ModuleState,
    interner: &Interner,
    expr_id: HirExprId,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    match &module.resolved.module.store.exprs.get(expr_id).kind {
        HirExprKind::Let { pat, value, .. } => {
            if matches!(
                module.resolved.module.store.exprs.get(*value).kind,
                HirExprKind::Given { .. }
            ) {
                collect_exported_given(module, *value, opaque, exports, attr_stack);
                return;
            }
            collect_export_bindings_from_pat(module, interner, *pat, opaque, exports, attr_stack);
        }
        HirExprKind::Given { .. } => {
            collect_exported_given(module, expr_id, opaque, exports, attr_stack);
        }
        _ => {}
    }
}

fn collect_exported_given(
    module: &ModuleState,
    expr_id: HirExprId,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    let span = module.resolved.module.store.exprs.get(expr_id).origin.span;
    if opaque {
        return;
    }
    if exports.givens.iter().any(|export| export.span == span) {
        return;
    }
    exports.givens.push(ExportGiven {
        span,
        attrs: attr_stack.to_vec().into_boxed_slice(),
    });
}

fn collect_export_bindings_from_pat(
    module: &ModuleState,
    interner: &Interner,
    pat_id: HirPatId,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    match module.resolved.module.store.pats.get(pat_id).kind.clone() {
        HirPatKind::Error | HirPatKind::Wildcard | HirPatKind::Lit { .. } => {}
        HirPatKind::Bind { name } => {
            push_export_binding(module, interner, name, opaque, exports, attr_stack);
        }
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => {
            collect_pat_id_range(module, interner, items, opaque, exports, attr_stack);
        }
        HirPatKind::Variant { args, .. } => {
            for item in module.resolved.module.store.variant_pat_args.get(args) {
                collect_export_bindings_from_pat(
                    module, interner, item.pat, opaque, exports, attr_stack,
                );
            }
        }
        HirPatKind::Record { fields } => {
            for field in module.resolved.module.store.record_pat_fields.get(fields) {
                if let Some(value) = field.value {
                    collect_export_bindings_from_pat(
                        module, interner, value, opaque, exports, attr_stack,
                    );
                } else {
                    push_export_binding(module, interner, field.name, opaque, exports, attr_stack);
                }
            }
        }
        HirPatKind::Or { left, right } => {
            collect_export_bindings_from_pat(module, interner, left, opaque, exports, attr_stack);
            collect_export_bindings_from_pat(module, interner, right, opaque, exports, attr_stack);
        }
        HirPatKind::As { pat, name } => {
            collect_export_bindings_from_pat(module, interner, pat, opaque, exports, attr_stack);
            push_export_binding(module, interner, name, opaque, exports, attr_stack);
        }
    }
}

fn collect_pat_id_range(
    module: &ModuleState,
    interner: &Interner,
    items: SliceRange<HirPatId>,
    opaque: bool,
    exports: &mut ModuleExports,
    attr_stack: &[HirAttr],
) {
    for item in module
        .resolved
        .module
        .store
        .pat_ids
        .get(items)
        .iter()
        .copied()
    {
        collect_export_bindings_from_pat(module, interner, item, opaque, exports, attr_stack);
    }
}

fn push_export_binding(
    module: &ModuleState,
    interner: &Interner,
    name: Ident,
    opaque: bool,
    exports: &mut ModuleExports,
    attrs: &[HirAttr],
) {
    let site = NameSite::new(module.resolved.module.source_id, name.span);
    let Some(binding) = module.binding_id_at_site(site) else {
        return;
    };
    if exports
        .bindings
        .iter()
        .any(|export| export.binding == binding)
    {
        return;
    }
    exports.bindings.push(ExportBinding {
        binding,
        name: interner.resolve(name.name).into(),
        opaque,
        attrs: attrs.to_vec().into_boxed_slice(),
    });
}
