use std::fs;
use std::path::{Path, PathBuf};

use musi_project::{Project, ProjectOptions, load_project_ancestor};
use music_base::{Source, SourceMap, Span};
use music_module::{ImportSiteKind, ModuleSpecifier, collect_import_sites};
use music_syntax::{Lexer, parse};

use crate::analysis::{ToolRange, tool_range};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolDocumentLink {
    pub range: ToolRange,
    pub specifier: String,
    pub resolved: String,
    pub target: PathBuf,
    pub tooltip: Option<String>,
}

#[must_use]
pub fn document_links_for_project_file(path: &Path) -> Vec<ToolDocumentLink> {
    document_links_for_project_file_with_overlay(path, None)
}

#[must_use]
pub fn document_links_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> Vec<ToolDocumentLink> {
    let Ok(project) = load_project_ancestor(path, ProjectOptions::default()) else {
        return Vec::new();
    };
    let Some(from_key) = project.module_key_for_path(path) else {
        return Vec::new();
    };
    let source_text = overlay_text
        .map(str::to_owned)
        .or_else(|| fs::read_to_string(path).ok())
        .unwrap_or_default();
    let mut sources = SourceMap::new();
    let Ok(source_id) = sources.add(path.to_path_buf(), source_text) else {
        return Vec::new();
    };
    let Some(source) = sources.get(source_id) else {
        return Vec::new();
    };
    let parsed = parse(Lexer::new(source.text()).lex());
    collect_import_sites(source_id, parsed.tree())
        .into_iter()
        .filter_map(|site| {
            let ImportSiteKind::Static { spec } = site.kind else {
                return None;
            };
            let resolved = project.import_map().resolve(&from_key, &spec)?;
            let target = target_path_for_resolved_spec(&project, &resolved)?;
            Some(ToolDocumentLink {
                range: tool_range(source, import_link_span(source, site.span, spec.as_str())),
                specifier: spec.as_str().to_owned(),
                resolved: resolved.as_str().to_owned(),
                target,
                tooltip: Some(format!("Open `{}`", spec.as_str())),
            })
        })
        .collect()
}

fn target_path_for_resolved_spec(project: &Project, resolved: &ModuleSpecifier) -> Option<PathBuf> {
    project
        .workspace()
        .packages
        .values()
        .flat_map(|package| package.module_keys.iter())
        .find_map(|(module_key, module_path)| {
            (module_key.as_str() == resolved.as_str()).then(|| module_path.clone())
        })
}

fn import_link_span(source: &Source, span: Span, spec: &str) -> Span {
    let text = source.text();
    let Ok(start) = usize::try_from(span.start) else {
        return span;
    };
    let Ok(end) = usize::try_from(span.end) else {
        return span;
    };
    let Some(slice) = text.get(start..end) else {
        return span;
    };
    let quoted = format!("\"{spec}\"");
    if let Some(relative) = slice.find(&quoted) {
        let link_start = start.saturating_add(relative);
        let link_end = link_start.saturating_add(quoted.len());
        return Span::new(
            u32::try_from(link_start).unwrap_or(span.start),
            u32::try_from(link_end).unwrap_or(span.end),
        );
    }
    let templated = format!("`{spec}`");
    if let Some(relative) = slice.find(&templated) {
        let link_start = start.saturating_add(relative);
        let link_end = link_start.saturating_add(templated.len());
        return Span::new(
            u32::try_from(link_start).unwrap_or(span.start),
            u32::try_from(link_end).unwrap_or(span.end),
        );
    }
    span
}
