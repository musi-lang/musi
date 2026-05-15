use std::path::Path;

use music_base::{Source, SourceId, Span};
use music_hir::{HirExprId, HirExprKind, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_sema::{ExprMemberKind, SemaModule};
use music_session::Session;

use crate::analysis::type_render::render_hir_ty;
use crate::analysis_support::analysis_session;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolSignatureHelp {
    pub signatures: Vec<ToolSignatureInformation>,
    pub active_signature: usize,
    pub active_parameter: usize,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolSignatureInformation {
    pub label: String,
    pub parameters: Vec<ToolParameterInformation>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolParameterInformation {
    pub label: String,
}

#[must_use]
pub fn signature_help_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<ToolSignatureHelp> {
    let (session, module_key) = analysis_session(path, overlay_text)?;
    let parsed = session.parsed_module_cached(&module_key).ok().flatten()?;
    let source = session.source(parsed.source_id)?;
    let offset = source.offset(line, character)?;
    let sema = session.sema_module_cached(&module_key).ok().flatten()?;
    let call = innermost_call_at_offset(sema, parsed.source_id, offset)?;
    signature_help_for_call(&session, source, sema, call, offset)
}

fn signature_help_for_call(
    session: &Session,
    source: &Source,
    sema: &SemaModule,
    call: HirExprId,
    offset: u32,
) -> Option<ToolSignatureHelp> {
    let HirExprKind::Call { callee, .. } = sema.module().store.exprs.get(call).kind else {
        return None;
    };
    let callee_expr = sema.module().store.exprs.get(callee);
    let callable_ty = sema.try_expr_ty(callee)?;
    let param_names = imported_export_param_names(session, sema, callee);
    let signature = signature_information_for_ty(
        session,
        sema,
        callable_ty,
        source_text_for_span(source, callee_expr.origin.span).unwrap_or("call"),
        param_names.as_deref(),
    )?;
    let active_parameter = active_parameter_index(
        source,
        callee_expr.origin.span.end,
        offset,
        signature.parameters.len(),
    );
    Some(ToolSignatureHelp {
        signatures: vec![signature],
        active_signature: 0,
        active_parameter,
    })
}

fn innermost_call_at_offset(
    sema: &SemaModule,
    source_id: SourceId,
    offset: u32,
) -> Option<HirExprId> {
    sema.module()
        .store
        .exprs
        .iter()
        .filter_map(|(expr_id, expr)| {
            if !matches!(expr.kind, HirExprKind::Call { .. }) {
                return None;
            }
            if expr.origin.source_id != source_id || !expr.origin.span.contains(offset) {
                return None;
            }
            Some((expr_id, expr.origin.span))
        })
        .min_by_key(|(_, span)| span.end.saturating_sub(span.start))
        .map(|(expr_id, _)| expr_id)
}

fn signature_information_for_ty(
    session: &Session,
    sema: &SemaModule,
    ty: HirTyId,
    name: &str,
    param_names: Option<&[String]>,
) -> Option<ToolSignatureInformation> {
    let mut params = Vec::new();
    let mut current = ty;
    loop {
        match sema.ty(current).kind.clone() {
            HirTyKind::Pi {
                binder,
                binder_ty,
                body,
                ..
            } => {
                let label = format!(
                    "{} : {}",
                    session.resolve_symbol(binder),
                    render_hir_ty(sema, session, binder_ty)
                );
                params.push(ToolParameterInformation { label });
                current = body;
            }
            HirTyKind::Arrow {
                params: ty_params,
                ret,
                ..
            } => {
                params.extend(
                    sema.module()
                        .store
                        .ty_ids
                        .get(ty_params)
                        .iter()
                        .enumerate()
                        .map(|(index, param)| {
                            let rendered = render_hir_ty(sema, session, *param);
                            let label = param_names.and_then(|names| names.get(index)).map_or_else(
                                || rendered.clone(),
                                |name| format!("{name} : {rendered}"),
                            );
                            ToolParameterInformation { label }
                        }),
                );
                let label = signature_label(name, &params, &render_hir_ty(sema, session, ret));
                return Some(ToolSignatureInformation {
                    label,
                    parameters: params,
                });
            }
            _ if !params.is_empty() => {
                let label = signature_label(name, &params, &render_hir_ty(sema, session, current));
                return Some(ToolSignatureInformation {
                    label,
                    parameters: params,
                });
            }
            _ => return None,
        }
    }
}

fn imported_export_param_names(
    session: &Session,
    sema: &SemaModule,
    callee: HirExprId,
) -> Option<Vec<String>> {
    if let HirExprKind::Apply { callee, .. } = sema.module().store.exprs.get(callee).kind {
        return imported_export_param_names(session, sema, callee);
    }
    let fact = sema.expr_member_fact(callee)?;
    if !matches!(fact.kind, ExprMemberKind::ImportRecordExport) {
        return None;
    }
    let target = fact
        .import_record_target
        .as_ref()
        .or_else(|| import_record_base_target(sema, callee))?;
    let export_name = session.resolve_symbol(fact.name);
    let imported = session.sema_module_cached(target).ok().flatten()?;
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

fn signature_label(name: &str, params: &[ToolParameterInformation], ret: &str) -> String {
    let args = params
        .iter()
        .map(|param| param.label.as_str())
        .collect::<Vec<_>>()
        .join(", ");
    format!("{name}({args}) -> {ret}")
}

fn active_parameter_index(
    source: &Source,
    callee_end: u32,
    offset: u32,
    parameter_count: usize,
) -> usize {
    if parameter_count == 0 {
        return 0;
    }
    let start = usize::try_from(callee_end).unwrap_or(0);
    let end = usize::try_from(offset).unwrap_or(start);
    let Some(text) = source.text().get(start..end) else {
        return 0;
    };
    text.chars()
        .fold((0usize, 0i32), |(count, depth), ch| match ch {
            '(' | '[' | '{' => (count, depth.saturating_add(1)),
            ')' | ']' | '}' => (count, depth.saturating_sub(1)),
            ',' if depth <= 1 => (count.saturating_add(1), depth),
            _ => (count, depth),
        })
        .0
        .min(parameter_count.saturating_sub(1))
}

fn source_text_for_span(source: &Source, span: Span) -> Option<&str> {
    source
        .text()
        .get(usize::try_from(span.start).ok()?..usize::try_from(span.end).ok()?)
}
