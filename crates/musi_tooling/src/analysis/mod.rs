use std::path::Path;

use music_arena::SliceRange;
use music_base::{Source, Span};
use music_hir::{HirDim, HirExprKind, HirTyField, HirTyId, HirTyKind, simple_hir_ty_display_name};
use music_module::ModuleKey;
use music_names::{NameBinding, NameBindingId, NameBindingKind};
use music_sema::{ExprMemberFact, ExprMemberKind, SemaModule};
use music_session::Session;

use crate::{
    analysis_support::analysis_session,
    semantic::{ToolSemanticTokenKind, semantic_syntax_tokens_for_source},
};

mod diagnostics;
mod docs;
mod inlay;
mod model;
pub mod type_render;

use docs::{leading_doc_text, module_doc_hover, module_doc_text};
use type_render::render_hir_ty;

pub use diagnostics::{collect_project_diagnostics, collect_project_diagnostics_with_overlay};
pub use inlay::{inlay_hints_for_project_file, inlay_hints_for_project_file_with_overlay};
pub use model::{
    ToolHover, ToolInlayHint, ToolInlayHintKind, ToolMemberShape, ToolPosition, ToolRange,
    ToolSymbolKind,
};

#[must_use]
pub fn tool_range(source: &Source, span: Span) -> ToolRange {
    let (start_line, start_col) = source.line_col(span.start);
    let (end_line, end_col) = source.line_col(span.end);
    ToolRange::new(start_line, start_col, end_line, end_col)
}

#[must_use]
pub fn hover_for_project_file(path: &Path, line: usize, character: usize) -> Option<ToolHover> {
    hover_for_project_file_with_overlay(path, None, line, character)
}

#[must_use]
pub fn module_docs_for_project_file(path: &Path) -> Option<String> {
    module_docs_for_project_file_with_overlay(path, None)
}

#[must_use]
pub fn module_docs_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> Option<String> {
    let (session, module_key) = analysis_session(path, overlay_text)?;
    let parsed = session.parsed_module_cached(&module_key).ok().flatten()?;
    let source = session.source(parsed.source_id)?;
    module_doc_text(source)
}

pub fn leading_binding_doc_text(source: &Source, span: Span) -> Option<String> {
    leading_doc_text(source, span)
}

#[must_use]
pub fn hover_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<ToolHover> {
    let (session, module_key) = analysis_session(path, overlay_text)?;
    let parsed = session.parsed_module_cached(&module_key).ok().flatten()?;
    let source = session.source(parsed.source_id)?;
    let offset = source.offset(line, character)?;
    if let Some((span, contents)) = module_doc_hover(source, offset) {
        return Some(ToolHover::new(span, tool_range(source, span), contents));
    }
    let resolved = session.resolved_module_cached(&module_key).ok().flatten()?;
    let sema = session.sema_module_cached(&module_key).ok().flatten();
    if let Some(sema) = sema
        && let Some(hover) = member_hover_at_offset(&session, source, sema, offset)
    {
        return Some(hover);
    }

    let by_ref = resolved
        .names
        .refs
        .iter()
        .find(|(site, _)| site.source_id == parsed.source_id && site.span.contains(offset))
        .map(|(site, binding)| (*site, *binding));
    let (site, binding_id) = match by_ref {
        Some(pair) => pair,
        None => resolved
            .names
            .bindings
            .iter()
            .find(|(_, binding)| {
                binding.site.source_id == parsed.source_id && binding.site.span.contains(offset)
            })
            .map(|(binding_id, binding)| (binding.site, binding_id))?,
    };
    let binding = resolved.names.bindings.get(binding_id);
    let kind_override = syntax_hover_kind_at_offset(source, offset);
    Some(ToolHover::new(
        site.span,
        tool_range(source, site.span),
        hover_contents(&session, binding_id, binding, sema, kind_override),
    ))
}

fn member_hover_at_offset(
    session: &Session,
    source: &Source,
    sema: &SemaModule,
    offset: u32,
) -> Option<ToolHover> {
    sema.module()
        .store
        .exprs
        .iter()
        .find_map(|(expr_id, expr)| {
            let HirExprKind::Field { base, name, .. } = expr.kind else {
                return None;
            };
            if !name.span.contains(offset) {
                return None;
            }
            let fact = sema.expr_member_fact(expr_id)?;
            let import_record_target = fact
                .import_record_target
                .as_ref()
                .or_else(|| sema.expr_import_record_target(base));
            Some(ToolHover::new(
                name.span,
                tool_range(source, name.span),
                member_hover_contents(session, sema, fact, import_record_target),
            ))
        })
}

fn hover_contents(
    session: &Session,
    binding_id: NameBindingId,
    binding: &NameBinding,
    sema: Option<&SemaModule>,
    kind_override: Option<ToolSymbolKind>,
) -> String {
    let name = session.resolve_symbol(binding.name);
    let kind = kind_override.unwrap_or_else(|| binding_symbol_kind(binding_id, binding, sema));
    let kind_label = kind.label();
    let mut lines = Vec::new();
    if let Some(sema) = sema.and_then(|module| {
        module
            .binding_type(binding_id)
            .map(|ty| render_hir_ty(module, session, ty))
    }) {
        lines.push(format!("```musi\n({kind_label}) {name} : {sema}\n```"));
    } else {
        lines.push(format!("```musi\n({kind_label}) {name}\n```"));
    }
    if let Some(docs) = session
        .source(binding.site.source_id)
        .and_then(|source| leading_doc_text(source, binding.site.span))
    {
        lines.push(String::new());
        lines.push(docs);
    }
    lines.join("\n")
}

fn syntax_hover_kind_at_offset(source: &Source, offset: u32) -> Option<ToolSymbolKind> {
    semantic_syntax_tokens_for_source(source)
        .into_iter()
        .find(|token| {
            matches!(
                token.kind,
                ToolSemanticTokenKind::Type | ToolSemanticTokenKind::TypeParameter
            ) && source
                .offset(token.range.start_line, token.range.start_col)
                .zip(source.offset(token.range.end_line, token.range.end_col))
                .is_some_and(|(start, end)| start <= offset && offset < end)
        })
        .map(|token| match token.kind {
            ToolSemanticTokenKind::TypeParameter => ToolSymbolKind::TypeParameter,
            _ => ToolSymbolKind::Type,
        })
}

fn member_hover_contents(
    session: &Session,
    sema: &SemaModule,
    fact: &ExprMemberFact,
    import_record_target: Option<&ModuleKey>,
) -> String {
    let name = session.resolve_symbol(fact.name);
    let kind = member_symbol_kind(sema, fact);
    let kind_label = kind.label();
    let ty = imported_member_ty_label(session, sema, fact, import_record_target)
        .unwrap_or_else(|| render_hir_ty(sema, session, fact.ty));
    let mut lines = vec![format!("```musi\n({kind_label}) {name} : {ty}\n```")];
    if let Some(binding_id) = fact.binding {
        let binding = sema.resolved().names.bindings.get(binding_id);
        if let Some(docs) = session
            .source(binding.site.source_id)
            .and_then(|source| leading_doc_text(source, binding.site.span))
        {
            lines.push(String::new());
            lines.push(docs);
        }
    } else if let Some(docs) = imported_member_doc_text(session, fact, import_record_target) {
        lines.push(String::new());
        lines.push(docs);
    }
    lines.join("\n")
}

fn imported_member_doc_text(
    session: &Session,
    fact: &ExprMemberFact,
    import_record_target: Option<&ModuleKey>,
) -> Option<String> {
    if !matches!(fact.kind, ExprMemberKind::ImportRecordExport) {
        return None;
    }
    let target = import_record_target?;
    let imported = session.sema_module_cached(target).ok().flatten()?;
    let export_name = session.resolve_symbol(fact.name);
    let export_exists = imported
        .surface()
        .exported_values()
        .iter()
        .any(|export| export.name.as_ref() == export_name);
    if !export_exists {
        return None;
    }
    let binding = imported
        .resolved()
        .names
        .bindings
        .iter()
        .map(|(_, binding)| binding)
        .find(|binding| {
            session.resolve_symbol(binding.name) == export_name
                && matches!(
                    binding.kind,
                    NameBindingKind::Let | NameBindingKind::AttachedMethod
                )
        })?;
    session
        .source(binding.site.source_id)
        .and_then(|source| leading_doc_text(source, binding.site.span))
        .filter(|docs| !docs.is_empty())
}

fn imported_member_ty_label(
    session: &Session,
    sema: &SemaModule,
    fact: &ExprMemberFact,
    import_record_target: Option<&ModuleKey>,
) -> Option<String> {
    if !matches!(fact.kind, ExprMemberKind::ImportRecordExport) {
        return None;
    }
    let export_name = session.resolve_symbol(fact.name);
    let target = import_record_target?;
    let imported = session.sema_module_cached(target).ok().flatten()?;
    let export = imported
        .surface()
        .exported_values()
        .iter()
        .find(|export| export.name.as_ref() == export_name)?;
    if export.param_names.is_empty() {
        return None;
    }
    callable_ty_with_param_names(session, sema, fact.ty, &export.param_names)
}

fn callable_ty_with_param_names(
    session: &Session,
    sema: &SemaModule,
    ty: HirTyId,
    param_names: &[Box<str>],
) -> Option<String> {
    let HirTyKind::Arrow { params, ret, .. } = sema.ty(ty).kind.clone() else {
        return None;
    };
    let args = sema
        .module()
        .store
        .ty_ids
        .get(params)
        .iter()
        .enumerate()
        .map(|(index, param)| {
            let rendered = render_hir_ty(sema, session, *param);
            param_names
                .get(index)
                .map_or_else(|| rendered.clone(), |name| format!("{name} : {rendered}"))
        })
        .collect::<Vec<_>>()
        .join(", ");
    Some(format!("({args}) -> {}", render_hir_ty(sema, session, ret)))
}

fn member_symbol_kind(sema: &SemaModule, fact: &ExprMemberFact) -> ToolSymbolKind {
    match member_class(sema, fact) {
        ToolMemberShape::Function => ToolSymbolKind::Function,
        ToolMemberShape::Procedure => ToolSymbolKind::Procedure,
        ToolMemberShape::Property => ToolSymbolKind::Property,
        ToolMemberShape::Type => ToolSymbolKind::Type,
    }
}

pub fn member_class(sema: &SemaModule, fact: &ExprMemberFact) -> ToolMemberShape {
    match fact.kind {
        ExprMemberKind::RecordField => ToolMemberShape::Property,
        ExprMemberKind::DotCallable
        | ExprMemberKind::AttachedMethod
        | ExprMemberKind::AttachedMethodNamespace => {
            if is_callable_ty(sema, fact.ty) {
                ToolMemberShape::Procedure
            } else {
                ToolMemberShape::Property
            }
        }
        ExprMemberKind::ShapeMember => ToolMemberShape::Procedure,
        ExprMemberKind::ImportRecordExport | ExprMemberKind::FfiPointerExport => {
            exported_member_class(sema, fact.ty)
        }
    }
}

fn exported_member_class(sema: &SemaModule, ty: HirTyId) -> ToolMemberShape {
    match sema.ty(ty).kind {
        HirTyKind::Arrow { .. } | HirTyKind::Pi { .. } => ToolMemberShape::Function,
        HirTyKind::Type => ToolMemberShape::Type,
        _ => ToolMemberShape::Property,
    }
}

fn is_callable_ty(sema: &SemaModule, ty: HirTyId) -> bool {
    matches!(
        sema.ty(ty).kind,
        HirTyKind::Arrow { .. } | HirTyKind::Pi { .. }
    )
}

pub fn binding_symbol_kind(
    binding_id: NameBindingId,
    binding: &NameBinding,
    sema: Option<&SemaModule>,
) -> ToolSymbolKind {
    match binding.kind {
        NameBindingKind::Param => ToolSymbolKind::Parameter,
        NameBindingKind::PiBinder | NameBindingKind::TypeParam => ToolSymbolKind::TypeParameter,
        NameBindingKind::Prelude
        | NameBindingKind::Import
        | NameBindingKind::Let
        | NameBindingKind::Pin
        | NameBindingKind::AttachedMethod
        | NameBindingKind::PatternBind => sema
            .and_then(|module| {
                module
                    .binding_type(binding_id)
                    .map(|ty| &module.ty(ty).kind)
            })
            .map_or(ToolSymbolKind::Variable, |ty| match ty {
                HirTyKind::Arrow { .. } | HirTyKind::Pi { .. } => ToolSymbolKind::Function,
                HirTyKind::Type => ToolSymbolKind::Type,
                _ => ToolSymbolKind::Variable,
            }),
    }
}
