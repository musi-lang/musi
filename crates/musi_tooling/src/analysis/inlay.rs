use std::collections::HashMap;
use std::path::Path;

use music_base::{Source, Span};
use music_hir::{HirArg, HirExprId, HirExprKind, HirPatId, HirPatKind};
use music_names::{NameBindingKind, NameResolution, Symbol};
use music_sema::SemaModule;
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
                NameBindingKind::Let | NameBindingKind::PatternBind
            ) {
                return None;
            }
            if !binding_needs_type_hint(sema, binding.site.span) {
                return None;
            }
            let ty = sema
                .binding_type(binding_id)
                .map(|ty| render_hir_ty(sema, context.session, ty))?;
            if ty.is_empty() || ty == "Unknown" {
                return None;
            }
            let (line, col) = context.source.line_col(binding.site.span.end);
            Some(ToolInlayHint::new(
                ToolPosition::new(line, col),
                format!(": {ty}"),
                ToolInlayHintKind::Type,
            ))
        })
        .collect()
}

fn binding_needs_type_hint(sema: &SemaModule, binding_span: Span) -> bool {
    sema.module().store.exprs.iter().any(|(_, expr)| {
        let HirExprKind::Let { pat, sig, .. } = expr.kind else {
            return false;
        };
        sig.is_none() && pat_contains_span(sema, pat, binding_span)
    })
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
    let param_names = same_module_param_names(sema);
    let mut hints = Vec::new();
    for (_, expr) in &sema.module().store.exprs {
        let HirExprKind::Call { callee, args } = &expr.kind else {
            continue;
        };
        let Some(callee_name) = callee_name(sema, *callee) else {
            continue;
        };
        let Some(names) = param_names.get(&callee_name) else {
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
            push_parameter_hint(context, sema, &mut hints, arg, *name);
        }
    }
    hints
}

fn same_module_param_names(sema: &SemaModule) -> HashMap<Symbol, Vec<Symbol>> {
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
            .map(|param| param.name.name)
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

fn push_parameter_hint(
    context: &AnalysisContext<'_>,
    sema: &SemaModule,
    hints: &mut Vec<ToolInlayHint>,
    arg: &HirArg,
    name: Symbol,
) {
    let expr = sema.module().store.exprs.get(arg.expr);
    let argument_text = source_span_text(context.source, expr.origin.span)
        .unwrap_or_default()
        .trim();
    let name_text = context.session.resolve_symbol(name);
    if argument_text == name_text {
        return;
    }
    let (line, col) = context.source.line_col(expr.origin.span.start);
    let mut hint = ToolInlayHint::new(
        ToolPosition::new(line, col),
        format!("{name_text}:"),
        ToolInlayHintKind::Parameter,
    );
    hint.tooltip = Some(format!("parameter `{name_text}`"));
    hints.push(hint);
}
