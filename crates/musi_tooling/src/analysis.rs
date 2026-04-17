use std::path::Path;

use musi_project::{Project, ProjectOptions, ProjectResult, load_project_ancestor};
use music_arena::SliceRange;
use music_base::Span;
use music_hir::{HirDim, HirTyField, HirTyId, HirTyKind, simple_hir_ty_display_name};
use music_module::ModuleKey;
use music_names::{NameBinding, NameBindingId, NameBindingKind};
use music_sema::SemaModule;
use music_session::Session;

use crate::{CliDiagnostic, project_error_report, session_error_report};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolHover {
    pub span: Span,
    pub contents: String,
}

impl ToolHover {
    #[must_use]
    pub fn new(span: Span, contents: impl Into<String>) -> Self {
        Self {
            span,
            contents: contents.into(),
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
    let project = match load_project_ancestor(path, ProjectOptions::default()) {
        Ok(project) => project,
        Err(error) => {
            return project_error_report("musi_lsp", "diagnostics", None, None, &error).diagnostics;
        }
    };
    let Some(module_key) = project.module_key_for_path(path) else {
        return Vec::new();
    };
    let mut session = match build_overlay_session(&project, &module_key, overlay_text) {
        Ok(session) => session,
        Err(error) => {
            return project_error_report("musi_lsp", "diagnostics", None, None, &error).diagnostics;
        }
    };
    match session.check_module(&module_key) {
        Ok(_) => Vec::new(),
        Err(error) => {
            session_error_report("musi_lsp", "diagnostics", None, None, &session, &error)
                .diagnostics
        }
    }
}

#[must_use]
pub fn hover_for_project_file(path: &Path, line: usize, character: usize) -> Option<ToolHover> {
    hover_for_project_file_with_overlay(path, None, line, character)
}

#[must_use]
pub fn hover_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<ToolHover> {
    let project = load_project_ancestor(path, ProjectOptions::default()).ok()?;
    let module_key = project.module_key_for_path(path)?;
    let mut session = build_overlay_session(&project, &module_key, overlay_text).ok()?;
    drop(session.check_module(&module_key));
    let parsed = session.parsed_module_cached(&module_key).ok().flatten()?;
    let source = session.source(parsed.source_id)?;
    let offset = source.offset(line, character)?;
    let resolved = session.resolved_module_cached(&module_key).ok().flatten()?;
    let sema = session.sema_module_cached(&module_key).ok().flatten();

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
        hover_contents(&session, binding_id, binding, sema),
    ))
}

fn build_overlay_session(
    project: &Project,
    module_key: &ModuleKey,
    overlay_text: Option<&str>,
) -> ProjectResult<Session> {
    let mut session = project.build_session()?;
    if let Some(text) = overlay_text {
        session.set_module_text(module_key, text.to_owned())?;
    }
    Ok(session)
}

fn hover_contents(
    session: &Session,
    binding_id: NameBindingId,
    binding: &NameBinding,
    sema: Option<&SemaModule>,
) -> String {
    let name = session.resolve_symbol(binding.name);
    let kind = binding_kind_label(binding.kind);
    let mut lines = Vec::new();
    if let Some(sema) = sema.and_then(|module| {
        module
            .binding_type(binding_id)
            .map(|ty| render_hir_ty(module, session, ty))
    }) {
        lines.push(format!("```musi\n{name} : {sema}\n```"));
    } else {
        lines.push(format!("```musi\n{name}\n```"));
    }
    lines.push(format!("kind: {kind}"));
    lines.join("\n")
}

const fn binding_kind_label(kind: NameBindingKind) -> &'static str {
    match kind {
        NameBindingKind::Prelude => "prelude",
        NameBindingKind::Let => "let",
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
        HirTyKind::Mut { inner } => format!("mut {}", render_hir_ty(sema, session, *inner)),
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
