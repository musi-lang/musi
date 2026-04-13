use std::path::Path;

use musi_project::{Project, ProjectOptions, ProjectResult, load_project_ancestor};
use music_base::Span;
use music_hir::{HirDim, HirTyField, HirTyId, HirTyKind};
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
#[allow(clippy::too_many_lines)]
fn render_hir_ty(sema: &SemaModule, session: &Session, ty: HirTyId) -> String {
    match &sema.ty(ty).kind {
        HirTyKind::Error => "<error>".into(),
        HirTyKind::Unknown => "Unknown".into(),
        HirTyKind::Type => "Type".into(),
        HirTyKind::Syntax => "Syntax".into(),
        HirTyKind::Any => "Any".into(),
        HirTyKind::Empty => "Empty".into(),
        HirTyKind::Unit => "Unit".into(),
        HirTyKind::Bool => "Bool".into(),
        HirTyKind::Nat => "Nat".into(),
        HirTyKind::Int => "Int".into(),
        HirTyKind::Float => "Float".into(),
        HirTyKind::String => "String".into(),
        HirTyKind::CString => "CString".into(),
        HirTyKind::CPtr => "CPtr".into(),
        HirTyKind::Module => "Module".into(),
        HirTyKind::NatLit(value) => value.to_string(),
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
        HirTyKind::Sum { left, right } => {
            format!(
                "{} + {}",
                render_hir_ty(sema, session, *left),
                render_hir_ty(sema, session, *right)
            )
        }
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
        HirTyKind::Array { dims, item } => {
            let mut parts = vec![render_hir_ty(sema, session, *item)];
            for dim in sema.module().store.dims.get(dims.clone()) {
                parts.push(render_dim(session, dim));
            }
            format!("[{}]", parts.join("; "))
        }
        HirTyKind::Seq { item } => format!("[]{}", render_hir_ty(sema, session, *item)),
        HirTyKind::Range { bound } => format!("Range[{}]", render_hir_ty(sema, session, *bound)),
        HirTyKind::ClosedRange { bound } => {
            format!("ClosedRange[{}]", render_hir_ty(sema, session, *bound))
        }
        HirTyKind::PartialRangeFrom { bound } => {
            format!("PartialRangeFrom[{}]", render_hir_ty(sema, session, *bound))
        }
        HirTyKind::PartialRangeUpTo { bound } => {
            format!("PartialRangeUpTo[{}]", render_hir_ty(sema, session, *bound))
        }
        HirTyKind::PartialRangeThru { bound } => {
            format!("PartialRangeThru[{}]", render_hir_ty(sema, session, *bound))
        }
        HirTyKind::Handler {
            effect,
            input,
            output,
        } => format!(
            "using {} ({} -> {})",
            render_hir_ty(sema, session, *effect),
            render_hir_ty(sema, session, *input),
            render_hir_ty(sema, session, *output)
        ),
        HirTyKind::Mut { inner } => format!("mut {}", render_hir_ty(sema, session, *inner)),
        HirTyKind::Record { fields } => {
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
    }
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
