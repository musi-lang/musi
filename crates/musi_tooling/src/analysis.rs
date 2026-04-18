use std::collections::HashMap;
use std::path::Path;

use musi_project::{ProjectOptions, load_project_ancestor};
use music_arena::SliceRange;
use music_base::{Source, Span};
use music_hir::{
    HirArg, HirDim, HirExprId, HirExprKind, HirPatId, HirPatKind, HirTyField, HirTyId, HirTyKind,
    simple_hir_ty_display_name,
};
use music_names::{NameBinding, NameBindingId, NameBindingKind, NameResolution, Symbol};
use music_sema::{ExprMemberFact, ExprMemberKind, SemaModule};
use music_session::Session;

use crate::{
    CliDiagnostic,
    analysis_support::{
        analysis_session, collect_direct_file_diagnostics, collect_loaded_project_diagnostics,
    },
};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolHover {
    pub span: Span,
    pub range: ToolRange,
    pub contents: String,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolSymbolKind {
    Function,
    Procedure,
    Variable,
    Parameter,
    TypeParameter,
    Type,
    Namespace,
    Alias,
    Property,
    EnumMember,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolMemberClass {
    Function,
    Procedure,
    Property,
    Type,
    Namespace,
}

impl ToolSymbolKind {
    #[must_use]
    pub const fn label(self) -> &'static str {
        match self {
            Self::Function => "function",
            Self::Procedure => "procedure",
            Self::Variable => "variable",
            Self::Parameter => "parameter",
            Self::TypeParameter => "type parameter",
            Self::Type => "type",
            Self::Namespace => "namespace",
            Self::Alias => "alias",
            Self::Property => "property",
            Self::EnumMember => "enum member",
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolInlayHintKind {
    Type,
    Parameter,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ToolPosition {
    pub line: usize,
    pub col: usize,
}

impl ToolPosition {
    #[must_use]
    pub const fn new(line: usize, col: usize) -> Self {
        Self { line, col }
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolInlayHint {
    pub position: ToolPosition,
    pub label: String,
    pub kind: ToolInlayHintKind,
    pub tooltip: Option<String>,
}

impl ToolInlayHint {
    #[must_use]
    pub fn new(position: ToolPosition, label: impl Into<String>, kind: ToolInlayHintKind) -> Self {
        Self {
            position,
            label: label.into(),
            kind,
            tooltip: None,
        }
    }
}

impl ToolHover {
    #[must_use]
    pub fn new(span: Span, range: ToolRange, contents: impl Into<String>) -> Self {
        Self {
            span,
            range,
            contents: contents.into(),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct ToolRange {
    pub start_line: usize,
    pub start_col: usize,
    pub end_line: usize,
    pub end_col: usize,
}

impl ToolRange {
    #[must_use]
    pub const fn new(start_line: usize, start_col: usize, end_line: usize, end_col: usize) -> Self {
        Self {
            start_line,
            start_col,
            end_line,
            end_col,
        }
    }
}

#[must_use]
pub fn collect_project_diagnostics(path: &Path) -> Vec<CliDiagnostic> {
    collect_project_diagnostics_with_overlay(path, None)
}

#[must_use]
pub fn collect_project_diagnostics_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> Vec<CliDiagnostic> {
    if let Ok(project) = load_project_ancestor(path, ProjectOptions::default())
        && let Some(module_key) = project.module_key_for_path(path)
    {
        return collect_loaded_project_diagnostics(&project, &module_key, overlay_text);
    }
    collect_direct_file_diagnostics(path, overlay_text)
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
    Some(ToolHover::new(
        site.span,
        tool_range(source, site.span),
        hover_contents(&session, binding_id, binding, sema),
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
            let HirExprKind::Field { name, .. } = expr.kind else {
                return None;
            };
            if !name.span.contains(offset) {
                return None;
            }
            let fact = sema.expr_member_fact(expr_id)?;
            Some(ToolHover::new(
                name.span,
                tool_range(source, name.span),
                member_hover_contents(session, sema, fact),
            ))
        })
}

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

#[must_use]
pub fn tool_range(source: &Source, span: Span) -> ToolRange {
    let (start_line, start_col) = source.line_col(span.start);
    let (end_line, end_col) = source.line_col(span.end);
    ToolRange::new(start_line, start_col, end_line, end_col)
}

struct AnalysisContext<'a> {
    session: &'a Session,
    source: &'a Source,
    sema: &'a SemaModule,
    resolved: &'a NameResolution,
}

fn hover_contents(
    session: &Session,
    binding_id: NameBindingId,
    binding: &NameBinding,
    sema: Option<&SemaModule>,
) -> String {
    let name = session.resolve_symbol(binding.name);
    let kind = binding_symbol_kind(binding_id, binding, sema);
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

fn member_hover_contents(session: &Session, sema: &SemaModule, fact: &ExprMemberFact) -> String {
    let name = session.resolve_symbol(fact.name);
    let kind = member_symbol_kind(sema, fact);
    let kind_label = kind.label();
    let ty = render_hir_ty(sema, session, fact.ty);
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
    }
    lines.join("\n")
}

fn member_symbol_kind(sema: &SemaModule, fact: &ExprMemberFact) -> ToolSymbolKind {
    match member_class(sema, fact) {
        ToolMemberClass::Function => ToolSymbolKind::Function,
        ToolMemberClass::Procedure => ToolSymbolKind::Procedure,
        ToolMemberClass::Property => ToolSymbolKind::Property,
        ToolMemberClass::Type => ToolSymbolKind::Type,
        ToolMemberClass::Namespace => ToolSymbolKind::Namespace,
    }
}

pub fn member_class(sema: &SemaModule, fact: &ExprMemberFact) -> ToolMemberClass {
    match fact.kind {
        ExprMemberKind::RecordField => ToolMemberClass::Property,
        ExprMemberKind::DotCallable
        | ExprMemberKind::AttachedMethod
        | ExprMemberKind::AttachedMethodNamespace => {
            if is_callable_ty(sema, fact.ty) {
                ToolMemberClass::Procedure
            } else {
                ToolMemberClass::Property
            }
        }
        ExprMemberKind::EffectOperation | ExprMemberKind::ClassMember => ToolMemberClass::Procedure,
        ExprMemberKind::ModuleExport | ExprMemberKind::FfiPointerExport => {
            exported_member_class(sema, fact.ty)
        }
    }
}

fn exported_member_class(sema: &SemaModule, ty: HirTyId) -> ToolMemberClass {
    match sema.ty(ty).kind {
        HirTyKind::Arrow { .. } | HirTyKind::Pi { .. } => ToolMemberClass::Function,
        HirTyKind::Module => ToolMemberClass::Namespace,
        HirTyKind::Type => ToolMemberClass::Type,
        _ => ToolMemberClass::Property,
    }
}

fn is_callable_ty(sema: &SemaModule, ty: HirTyId) -> bool {
    matches!(
        sema.ty(ty).kind,
        HirTyKind::Arrow { .. } | HirTyKind::Pi { .. }
    )
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

fn source_span_text(source: &Source, span: Span) -> Option<&str> {
    let start = usize::try_from(span.start).ok()?;
    let end = usize::try_from(span.end).ok()?;
    source.text().get(start..end)
}

fn starts_with_item_doc_block(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'-', ..])
}

fn starts_with_module_doc_block(text: &str) -> bool {
    let bytes = text.as_bytes();
    matches!(bytes, [b'/', b'-', b'!', ..])
}

fn leading_doc_text(source: &Source, span: Span) -> Option<String> {
    let (line, _) = source.line_col(span.start);
    let previous_line = line.checked_sub(1)?;
    let previous_text = source.line_text(previous_line)?.trim_start();
    if previous_text.starts_with("---") {
        return leading_line_doc_text(source, previous_line);
    }
    if previous_text.starts_with("--!") {
        return None;
    }
    if previous_text.ends_with("-/") {
        return leading_block_doc_text(source, previous_line);
    }
    None
}

fn leading_line_doc_text(source: &Source, mut line: usize) -> Option<String> {
    let mut docs = Vec::new();
    loop {
        let text = source.line_text(line)?.trim_start();
        let Some(doc_text) = text.strip_prefix("---") else {
            break;
        };
        docs.push(doc_text.trim_start().to_owned());
        if line == 1 {
            break;
        }
        line -= 1;
    }
    docs.reverse();
    (!docs.is_empty()).then(|| docs.join("\n"))
}

fn leading_block_doc_text(source: &Source, mut line: usize) -> Option<String> {
    let mut lines = Vec::new();
    loop {
        let text = source.line_text(line)?.trim_start();
        lines.push(text.to_owned());
        if starts_with_item_doc_block(text) {
            lines.reverse();
            return Some(clean_block_doc_text(&lines.join("\n"), 3));
        }
        if starts_with_module_doc_block(text) {
            return None;
        }
        if line == 1 {
            return None;
        }
        line -= 1;
    }
}

fn module_doc_text(source: &Source) -> Option<String> {
    let mut line = 1;
    let mut docs = Vec::new();
    while let Some(text) = source.line_text(line) {
        let trimmed = text.trim_start();
        if trimmed.is_empty() {
            line += 1;
            continue;
        }
        if let Some(doc_text) = trimmed.strip_prefix("--!") {
            docs.push(doc_text.trim_start().to_owned());
            line += 1;
            continue;
        }
        if starts_with_module_doc_block(trimmed) {
            let (doc, next_line) = module_block_doc_text(source, line)?;
            docs.push(doc);
            line = next_line;
            continue;
        }
        break;
    }
    (!docs.is_empty()).then(|| docs.join("\n"))
}

fn module_block_doc_text(source: &Source, start_line: usize) -> Option<(String, usize)> {
    let mut line = start_line;
    let mut lines = Vec::new();
    while let Some(text) = source.line_text(line) {
        lines.push(text.trim_start().to_owned());
        if text.contains("-/") {
            return Some((clean_block_doc_text(&lines.join("\n"), 3), line + 1));
        }
        line += 1;
    }
    None
}

fn clean_block_doc_text(text: &str, opener_len: usize) -> String {
    let without_opener = text.get(opener_len..).unwrap_or(text);
    without_opener
        .strip_suffix("-/")
        .unwrap_or(without_opener)
        .trim()
        .to_owned()
}

fn binding_symbol_kind(
    binding_id: NameBindingId,
    binding: &NameBinding,
    sema: Option<&SemaModule>,
) -> ToolSymbolKind {
    match binding.kind {
        NameBindingKind::Param
        | NameBindingKind::HandleClauseParam
        | NameBindingKind::HandleClauseResult => ToolSymbolKind::Parameter,
        NameBindingKind::PiBinder | NameBindingKind::TypeParam => ToolSymbolKind::TypeParameter,
        NameBindingKind::Prelude
        | NameBindingKind::Let
        | NameBindingKind::AttachedMethod
        | NameBindingKind::PatternBind => sema
            .and_then(|module| {
                module
                    .binding_type(binding_id)
                    .map(|ty| &module.ty(ty).kind)
            })
            .map_or(ToolSymbolKind::Variable, |ty| match ty {
                HirTyKind::Arrow { .. } | HirTyKind::Pi { .. } => ToolSymbolKind::Function,
                HirTyKind::Module => {
                    if binding.kind == NameBindingKind::Let {
                        ToolSymbolKind::Alias
                    } else {
                        ToolSymbolKind::Namespace
                    }
                }
                HirTyKind::Type => ToolSymbolKind::Type,
                _ => ToolSymbolKind::Variable,
            }),
    }
}

const fn _binding_kind_label(kind: NameBindingKind) -> &'static str {
    match kind {
        NameBindingKind::Prelude => "prelude",
        NameBindingKind::Let => "let",
        NameBindingKind::AttachedMethod => "attached method",
        NameBindingKind::Param => "parameter",
        NameBindingKind::PiBinder => "pi binder",
        NameBindingKind::TypeParam => "type parameter",
        NameBindingKind::PatternBind => "pattern binding",
        NameBindingKind::HandleClauseResult => "handler result",
        NameBindingKind::HandleClauseParam => "handler parameter",
    }
}

#[must_use]
fn render_hir_ty(sema: &SemaModule, session: &Session, ty: HirTyId) -> String {
    let kind = &sema.ty(ty).kind;
    if let Some(atomic) = render_atomic_hir_ty(kind) {
        return atomic;
    }
    match kind {
        HirTyKind::Named { name, args } => render_named_hir_ty(
            session.resolve_symbol(*name),
            sema.module().store.ty_ids.get(*args),
            |ty| render_hir_ty(sema, session, ty),
        ),
        HirTyKind::Pi {
            binder,
            binder_ty,
            body,
            is_effectful,
        } => {
            let arrow = if *is_effectful { " ~> " } else { " -> " };
            format!(
                "forall ({} : {}){arrow}{}",
                session.resolve_symbol(*binder),
                render_hir_ty(sema, session, *binder_ty),
                render_hir_ty(sema, session, *body)
            )
        }
        HirTyKind::Arrow {
            params,
            ret,
            is_effectful,
        } => render_arrow_hir_ty(
            sema.module().store.ty_ids.get(*params),
            *ret,
            *is_effectful,
            |ty| render_hir_ty(sema, session, ty),
        ),
        HirTyKind::Sum { left, right } => render_sum_hir_ty(sema, session, *left, *right),
        HirTyKind::Tuple { items } => {
            let values = sema
                .module()
                .store
                .ty_ids
                .get(*items)
                .iter()
                .map(|item| render_hir_ty(sema, session, *item))
                .collect::<Vec<_>>()
                .join(", ");
            format!("({values})")
        }
        HirTyKind::Array { dims, item } => render_array_hir_ty(sema, session, dims, *item),
        HirTyKind::Seq { item } => format!("[]{}", render_hir_ty(sema, session, *item)),
        HirTyKind::Range { bound } => render_applied_hir_ty("Range", sema, session, *bound),
        HirTyKind::ClosedRange { bound } => {
            render_applied_hir_ty("ClosedRange", sema, session, *bound)
        }
        HirTyKind::PartialRangeFrom { bound } => {
            render_applied_hir_ty("PartialRangeFrom", sema, session, *bound)
        }
        HirTyKind::PartialRangeUpTo { bound } => {
            render_applied_hir_ty("PartialRangeUpTo", sema, session, *bound)
        }
        HirTyKind::PartialRangeThru { bound } => {
            render_applied_hir_ty("PartialRangeThru", sema, session, *bound)
        }
        HirTyKind::Handler {
            effect,
            input,
            output,
        } => render_handler_hir_ty(sema, session, *effect, *input, *output),
        HirTyKind::Mut { inner } => render_prefixed_hir_ty("mut", sema, session, *inner),
        HirTyKind::AnyClass { class } => render_prefixed_hir_ty("any", sema, session, *class),
        HirTyKind::SomeClass { class } => render_prefixed_hir_ty("some", sema, session, *class),
        HirTyKind::Record { fields } => render_record_hir_ty(sema, session, fields),
        HirTyKind::Error
        | HirTyKind::Unknown
        | HirTyKind::Type
        | HirTyKind::Syntax
        | HirTyKind::Any
        | HirTyKind::Empty
        | HirTyKind::Unit
        | HirTyKind::Bool
        | HirTyKind::Nat
        | HirTyKind::Int
        | HirTyKind::Int8
        | HirTyKind::Int16
        | HirTyKind::Int32
        | HirTyKind::Int64
        | HirTyKind::Nat8
        | HirTyKind::Nat16
        | HirTyKind::Nat32
        | HirTyKind::Nat64
        | HirTyKind::Float
        | HirTyKind::Float32
        | HirTyKind::Float64
        | HirTyKind::String
        | HirTyKind::Rune
        | HirTyKind::CString
        | HirTyKind::CPtr
        | HirTyKind::Module
        | HirTyKind::NatLit(_) => render_atomic_hir_ty(kind).unwrap_or_default(),
    }
}

fn render_prefixed_hir_ty(
    prefix: &str,
    sema: &SemaModule,
    session: &Session,
    inner: HirTyId,
) -> String {
    format!("{prefix} {}", render_hir_ty(sema, session, inner))
}

fn render_atomic_hir_ty(kind: &HirTyKind) -> Option<String> {
    if let HirTyKind::NatLit(value) = kind {
        return Some(value.to_string());
    }
    simple_hir_ty_display_name(kind).map(str::to_owned)
}

fn render_sum_hir_ty(
    sema: &SemaModule,
    session: &Session,
    left: HirTyId,
    right: HirTyId,
) -> String {
    format!(
        "{} + {}",
        render_hir_ty(sema, session, left),
        render_hir_ty(sema, session, right)
    )
}

fn render_array_hir_ty(
    sema: &SemaModule,
    session: &Session,
    dims: &SliceRange<HirDim>,
    item: HirTyId,
) -> String {
    let mut parts = vec![render_hir_ty(sema, session, item)];
    for dim in sema.module().store.dims.get(dims.clone()) {
        parts.push(render_dim(session, dim));
    }
    format!("[{}]", parts.join("; "))
}

fn render_applied_hir_ty(
    name: &str,
    sema: &SemaModule,
    session: &Session,
    bound: HirTyId,
) -> String {
    format!("{name}[{}]", render_hir_ty(sema, session, bound))
}

fn render_handler_hir_ty(
    sema: &SemaModule,
    session: &Session,
    effect: HirTyId,
    input: HirTyId,
    output: HirTyId,
) -> String {
    format!(
        "using {} ({} -> {})",
        render_hir_ty(sema, session, effect),
        render_hir_ty(sema, session, input),
        render_hir_ty(sema, session, output)
    )
}

fn render_record_hir_ty(
    sema: &SemaModule,
    session: &Session,
    fields: &SliceRange<HirTyField>,
) -> String {
    let rendered = sema
        .module()
        .store
        .ty_fields
        .get(fields.clone())
        .iter()
        .map(|field| render_ty_field(sema, session, field))
        .collect::<Vec<_>>()
        .join(", ");
    format!("{{ {rendered} }}")
}

fn render_named_hir_ty(
    name: &str,
    args: &[HirTyId],
    mut render: impl FnMut(HirTyId) -> String,
) -> String {
    let mut rendered = name.to_owned();
    if !args.is_empty() {
        let contents = args
            .iter()
            .map(|item| render(*item))
            .collect::<Vec<_>>()
            .join(", ");
        rendered.push('[');
        rendered.push_str(&contents);
        rendered.push(']');
    }
    rendered
}

fn render_arrow_hir_ty(
    params: &[HirTyId],
    ret: HirTyId,
    is_effectful: bool,
    mut render: impl FnMut(HirTyId) -> String,
) -> String {
    let left_items = params.iter().map(|item| render(*item)).collect::<Vec<_>>();
    let left = if left_items.len() == 1 {
        left_items[0].clone()
    } else {
        format!("({})", left_items.join(", "))
    };
    let arrow = if is_effectful { " ~> " } else { " -> " };
    format!("{left}{arrow}{}", render(ret))
}

#[must_use]
fn render_dim(session: &Session, dim: &HirDim) -> String {
    match dim {
        HirDim::Unknown => "_".into(),
        HirDim::Name(ident) => session.resolve_symbol(ident.name).to_owned(),
        HirDim::Int(value) => value.to_string(),
    }
}

#[must_use]
fn render_ty_field(sema: &SemaModule, session: &Session, field: &HirTyField) -> String {
    format!(
        "{} : {}",
        session.resolve_symbol(field.name),
        render_hir_ty(sema, session, field.ty)
    )
}
