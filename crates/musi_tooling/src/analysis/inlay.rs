use std::collections::HashMap;
use std::path::Path;

use music_base::{Source, Span};
use music_hir::{HirArg, HirExprId, HirExprKind, HirPatId, HirPatKind, HirReceiverDecl};
use music_module::ModuleKey;
use music_names::{NameBindingKind, NameResolution, Symbol};
use music_sema::{ExprMemberKind, SemaModule};
use music_session::Session;

use super::docs::source_span_text;
use super::type_render::render_hir_ty;
use super::{ToolInlayHint, ToolInlayHintKind, ToolPosition};
use crate::analysis_support::analysis_session;

#[must_use]
pub fn inlay_hints_for_project_file(path: &Path) -> Vec<ToolInlayHint> {
    inlay_hints_for_project_file_with_overlay(path, None)
}

#[must_use]
pub fn inlay_hints_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> Vec<ToolInlayHint> {
    let Some((session, module_key)) = analysis_session(path, overlay_text) else {
        return Vec::new();
    };
    let Some(parsed) = session.parsed_module_cached(&module_key).ok().flatten() else {
        return Vec::new();
    };
    let Some(source) = session.source(parsed.source_id) else {
        return Vec::new();
    };
    let Some(resolved) = session.resolved_module_cached(&module_key).ok().flatten() else {
        return Vec::new();
    };
    let Some(sema) = session.sema_module_cached(&module_key).ok().flatten() else {
        return Vec::new();
    };
    let context = AnalysisContext {
        session: &session,
        source,
        sema,
        resolved: &resolved.names,
    };
    let mut hints = variable_type_hints(&context);
    hints.extend(parameter_name_hints(&context));
    hints.sort_by_key(|hint| (hint.position.line, hint.position.col, hint.label.clone()));
    hints
}

struct AnalysisContext<'a> {
    session: &'a Session,
    source: &'a Source,
    sema: &'a SemaModule,
    resolved: &'a NameResolution,
}

fn variable_type_hints(context: &AnalysisContext<'_>) -> Vec<ToolInlayHint> {
    let sema = context.sema;
    context
        .resolved
        .bindings
        .iter()
        .filter_map(|(binding_id, binding)| {
            if !matches!(
                binding.kind,
                NameBindingKind::Let
                    | NameBindingKind::PatternBind
                    | NameBindingKind::AttachedMethod
            ) {
                return None;
            }
            let hint_offset = binding_type_hint_offset(context, sema, binding.site.span)?;
            let (line, col) = context.source.line_col(hint_offset);
            let ty = sema
                .binding_type(binding_id)
                .map(|ty| render_hir_ty(sema, context.session, ty))?;
            if ty.is_empty() || ty == "Unknown" {
                return None;
            }
            Some(ToolInlayHint::new(
                ToolPosition::new(line, col),
                format!(": {ty}"),
                ToolInlayHintKind::Type,
            ))
        })
        .collect()
}

fn binding_type_hint_offset(
    context: &AnalysisContext<'_>,
    sema: &SemaModule,
    binding_span: Span,
) -> Option<u32> {
    sema.module().store.exprs.iter().find_map(|(_, expr)| {
        let HirExprKind::Let {
            pat,
            sig,
            has_param_clause,
            receiver,
            value,
            ..
        } = &expr.kind
        else {
            return None;
        };
        if sig.is_some() || !let_binds_span(sema, *pat, receiver.as_ref(), binding_span) {
            return None;
        }
        if !has_param_clause {
            return Some(binding_span.end);
        }
        let head_end = receiver.as_ref().map_or_else(
            || sema.module().store.pats.get(*pat).origin.span.end,
            |receiver| receiver.method.span.end,
        );
        let value_start = sema.module().store.exprs.get(*value).origin.span.start;
        find_param_clause_end(context.source, head_end, value_start).or(Some(binding_span.end))
    })
}

fn let_binds_span(
    sema: &SemaModule,
    pat: HirPatId,
    receiver: Option<&HirReceiverDecl>,
    binding_span: Span,
) -> bool {
    pat_contains_span(sema, pat, binding_span)
        || receiver_method_matches_span(receiver, binding_span)
}

fn receiver_method_matches_span(receiver: Option<&HirReceiverDecl>, binding_span: Span) -> bool {
    receiver.is_some_and(|receiver| receiver.method.span == binding_span)
}

fn find_param_clause_end(source: &Source, search_start: u32, search_end: u32) -> Option<u32> {
    if search_end <= search_start {
        return None;
    }
    let start = usize::try_from(search_start).ok()?;
    let end = usize::try_from(search_end).ok()?;
    let text = source.text().get(start..end)?;
    let open = text.find('(')?;
    let mut depth = 0usize;
    let suffix = text.get(open..)?;
    for (offset, ch) in suffix.char_indices() {
        if ch == '(' {
            depth = depth.saturating_add(1);
        } else if ch == ')' {
            if depth == 0 {
                return None;
            }
            depth = depth.saturating_sub(1);
            if depth == 0 {
                let end_offset = open.saturating_add(offset).saturating_add(ch.len_utf8());
                let absolute = start.saturating_add(end_offset);
                return u32::try_from(absolute).ok();
            }
        }
    }
    None
}

fn pat_contains_span(sema: &SemaModule, pat: HirPatId, span: Span) -> bool {
    let pat = sema.module().store.pats.get(pat);
    if pat.origin.span.contains(span.start) || pat.origin.span.contains(span.end) {
        return true;
    }
    match &pat.kind {
        HirPatKind::Tuple { items } | HirPatKind::Array { items } => sema
            .module()
            .store
            .pat_ids
            .get(*items)
            .iter()
            .any(|item| pat_contains_span(sema, *item, span)),
        HirPatKind::Record { fields } => sema
            .module()
            .store
            .record_pat_fields
            .get(fields.clone())
            .iter()
            .filter_map(|field| field.value)
            .any(|item| pat_contains_span(sema, item, span)),
        HirPatKind::Variant { args, .. } => sema
            .module()
            .store
            .variant_pat_args
            .get(args.clone())
            .iter()
            .any(|arg| pat_contains_span(sema, arg.pat, span)),
        HirPatKind::Or { left, right } => {
            pat_contains_span(sema, *left, span) || pat_contains_span(sema, *right, span)
        }
        HirPatKind::As { pat, .. } => pat_contains_span(sema, *pat, span),
        HirPatKind::Error
        | HirPatKind::Wildcard
        | HirPatKind::Bind { .. }
        | HirPatKind::Lit { .. } => false,
    }
}

fn parameter_name_hints(context: &AnalysisContext<'_>) -> Vec<ToolInlayHint> {
    let sema = context.sema;
    let param_names = same_module_param_names(context.session, sema);
    let mut hints = Vec::new();
    for (_, expr) in &sema.module().store.exprs {
        let HirExprKind::Call { callee, args } = &expr.kind else {
            continue;
        };
        let Some(names) = param_names_for_callee(context, sema, &param_names, *callee) else {
            continue;
        };
        let args = sema.module().store.args.get(args.clone());
        for (index, arg) in args.iter().enumerate() {
            if arg.name.is_some() || arg.spread {
                continue;
            }
            let Some(name) = names.get(index) else {
                continue;
            };
            push_parameter_hint(context, sema, &mut hints, arg, name);
        }
    }
    hints
}

fn same_module_param_names(session: &Session, sema: &SemaModule) -> HashMap<Symbol, Vec<String>> {
    let mut names = HashMap::new();
    for (_, expr) in &sema.module().store.exprs {
        let HirExprKind::Let {
            pat,
            params,
            has_param_clause: true,
            ..
        } = &expr.kind
        else {
            continue;
        };
        let Some(binding) = simple_pat_binding(sema, *pat) else {
            continue;
        };
        let params = sema
            .module()
            .store
            .params
            .get(params.clone())
            .iter()
            .map(|param| session.resolve_symbol(param.name.name).to_owned())
            .collect::<Vec<_>>();
        let _ = names.insert(binding, params);
    }
    names
}

fn simple_pat_binding(sema: &SemaModule, pat: HirPatId) -> Option<Symbol> {
    match sema.module().store.pats.get(pat).kind {
        HirPatKind::Bind { name } => Some(name.name),
        _ => None,
    }
}

fn callee_name(sema: &SemaModule, expr: HirExprId) -> Option<Symbol> {
    match sema.module().store.exprs.get(expr).kind {
        HirExprKind::Name { name } => Some(name.name),
        HirExprKind::Apply { callee, .. } => callee_name(sema, callee),
        _ => None,
    }
}

fn param_names_for_callee(
    context: &AnalysisContext<'_>,
    sema: &SemaModule,
    same_module: &HashMap<Symbol, Vec<String>>,
    callee: HirExprId,
) -> Option<Vec<String>> {
    if let Some(name) = callee_name(sema, callee)
        && let Some(names) = same_module.get(&name)
    {
        return Some(names.clone());
    }
    imported_export_param_names(context, sema, callee)
}

fn imported_export_param_names(
    context: &AnalysisContext<'_>,
    sema: &SemaModule,
    callee: HirExprId,
) -> Option<Vec<String>> {
    if let HirExprKind::Apply { callee, .. } = sema.module().store.exprs.get(callee).kind {
        return imported_export_param_names(context, sema, callee);
    }
    let fact = sema.expr_member_fact(callee)?;
    if !matches!(fact.kind, ExprMemberKind::ImportRecordExport) {
        return None;
    }
    let target = fact
        .import_record_target
        .as_ref()
        .or_else(|| import_record_base_target(sema, callee))?;
    let export_name = context.session.resolve_symbol(fact.name);
    let imported = context.session.sema_module_cached(target).ok().flatten()?;
    let export = imported
        .surface()
        .exported_values()
        .iter()
        .find(|export| export.name.as_ref() == export_name)?;
    Some(export.param_names.iter().map(ToString::to_string).collect())
}

fn import_record_base_target(sema: &SemaModule, callee: HirExprId) -> Option<&ModuleKey> {
    let HirExprKind::Field { base, .. } = sema.module().store.exprs.get(callee).kind else {
        return None;
    };
    sema.expr_import_record_target(base)
}

fn push_parameter_hint(
    context: &AnalysisContext<'_>,
    sema: &SemaModule,
    hints: &mut Vec<ToolInlayHint>,
    arg: &HirArg,
    name_text: &str,
) {
    let expr = sema.module().store.exprs.get(arg.expr);
    let argument_text = source_span_text(context.source, expr.origin.span)
        .unwrap_or_default()
        .trim();
    if argument_text == name_text {
        return;
    }
    let is_literal_argument = matches!(
        expr.kind,
        HirExprKind::Lit { .. } | HirExprKind::Template { .. }
    );
    let (line, col) = context.source.line_col(expr.origin.span.start);
    let mut hint = ToolInlayHint::new(
        ToolPosition::new(line, col),
        format!("{name_text}:"),
        ToolInlayHintKind::Parameter,
    );
    hint.tooltip = Some(format!("parameter `{name_text}`"));
    hint.is_literal_argument = is_literal_argument;
    hints.push(hint);
}
