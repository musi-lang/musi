use std::collections::HashMap;
use std::path::{Path, PathBuf};

use musi_project::{ProjectOptions, load_project_ancestor};
use music_base::{Source, SourceId};
use music_hir::HirExprKind;
use music_module::ModuleKey;
use music_names::{NameBinding, NameBindingId, NameBindingKind, NameResolution, NameSite};
use music_sema::SemaModule;
use music_session::Session;

use crate::analysis::{ToolRange, ToolSymbolKind, binding_symbol_kind, tool_range};
use crate::analysis_support::analysis_session;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolLocation {
    pub path: PathBuf,
    pub range: ToolRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolTextEdit {
    pub range: ToolRange,
    pub new_text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolWorkspaceEdit {
    pub changes: HashMap<PathBuf, Vec<ToolTextEdit>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolDocumentSymbol {
    pub name: String,
    pub kind: ToolSymbolKind,
    pub range: ToolRange,
    pub selection_range: ToolRange,
    pub children: Vec<Self>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolWorkspaceSymbol {
    pub name: String,
    pub kind: ToolSymbolKind,
    pub location: ToolLocation,
}

#[must_use]
pub fn definition_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<ToolLocation> {
    let context = SymbolAnalysis::new(path, overlay_text)?;
    let binding_id = context.binding_at(line, character)?;
    context.binding_location(binding_id)
}

#[must_use]
pub fn references_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
    include_declaration: bool,
) -> Vec<ToolLocation> {
    let Some(context) = SymbolAnalysis::new(path, overlay_text) else {
        return Vec::new();
    };
    let Some(binding_id) = context.binding_at(line, character) else {
        return Vec::new();
    };
    context.references(binding_id, include_declaration)
}

#[must_use]
pub fn document_symbols_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> Vec<ToolDocumentSymbol> {
    let Some(context) = SymbolAnalysis::new(path, overlay_text) else {
        return Vec::new();
    };
    context.document_symbols()
}

#[must_use]
pub fn workspace_symbols_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    query: &str,
) -> Vec<ToolWorkspaceSymbol> {
    let Some(context) = SymbolAnalysis::new(path, overlay_text) else {
        return Vec::new();
    };
    context.workspace_symbols(query)
}

#[must_use]
pub fn prepare_rename_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<(ToolRange, String)> {
    let context = SymbolAnalysis::new(path, overlay_text)?;
    let binding_id = context.binding_at(line, character)?;
    let binding = context.resolved()?.bindings.get(binding_id);
    if !context.can_rename_binding(binding) {
        return None;
    }
    Some((
        tool_range(context.source_for_site(binding.site)?, binding.site.span),
        context.session.resolve_symbol(binding.name).to_owned(),
    ))
}

#[must_use]
pub fn rename_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
    new_name: &str,
) -> Option<ToolWorkspaceEdit> {
    if !is_valid_rename_name(new_name) {
        return None;
    }
    let context = SymbolAnalysis::new(path, overlay_text)?;
    let binding_id = context.binding_at(line, character)?;
    let binding = context.resolved()?.bindings.get(binding_id);
    if !context.can_rename_binding(binding) {
        return None;
    }
    let mut changes = HashMap::<PathBuf, Vec<ToolTextEdit>>::new();
    let mut push_edit = |location: ToolLocation| {
        changes
            .entry(location.path)
            .or_default()
            .push(ToolTextEdit {
                range: location.range,
                new_text: new_name.to_owned(),
            });
    };
    push_edit(context.binding_location(binding_id)?);
    for location in context.references(binding_id, false) {
        push_edit(location);
    }
    for edits in changes.values_mut() {
        edits.sort_by_key(|edit| {
            (
                edit.range.start_line,
                edit.range.start_col,
                edit.range.end_line,
                edit.range.end_col,
            )
        });
        edits.dedup_by_key(|edit| {
            (
                edit.range.start_line,
                edit.range.start_col,
                edit.range.end_line,
                edit.range.end_col,
            )
        });
    }
    Some(ToolWorkspaceEdit { changes })
}

struct SymbolAnalysis {
    session: Session,
    module_key: ModuleKey,
    source_id: SourceId,
    path: PathBuf,
    path_map: HashMap<String, PathBuf>,
}

impl SymbolAnalysis {
    fn new(path: &Path, overlay_text: Option<&str>) -> Option<Self> {
        let (session, module_key) = analysis_session(path, overlay_text)?;
        let parsed = session.parsed_module_cached(&module_key).ok().flatten()?;
        let source_id = parsed.source_id;
        Some(Self {
            session,
            module_key,
            source_id,
            path: path.to_path_buf(),
            path_map: module_path_map(path),
        })
    }

    fn source(&self) -> Option<&Source> {
        self.session.source(self.source_id)
    }

    fn resolved(&self) -> Option<&NameResolution> {
        Some(
            &self
                .session
                .resolved_module_cached(&self.module_key)
                .ok()
                .flatten()?
                .names,
        )
    }

    fn sema(&self) -> Option<&SemaModule> {
        self.session
            .sema_module_cached(&self.module_key)
            .ok()
            .flatten()
    }

    fn binding_at(&self, line: usize, character: usize) -> Option<NameBindingId> {
        let source = self.source()?;
        let offset = source.offset(line, character)?;
        if let Some(sema) = self.sema()
            && let Some(binding_id) = member_binding_at_offset(sema, offset)
        {
            return Some(binding_id);
        }
        let resolved = self.resolved()?;
        resolved
            .refs
            .iter()
            .find(|(site, _)| site.source_id == self.source_id && site.span.contains(offset))
            .map(|(_, binding_id)| *binding_id)
            .or_else(|| {
                resolved
                    .bindings
                    .iter()
                    .find(|(_, binding)| {
                        binding.site.source_id == self.source_id
                            && binding.site.span.contains(offset)
                    })
                    .map(|(binding_id, _)| binding_id)
            })
    }

    fn binding_location(&self, binding_id: NameBindingId) -> Option<ToolLocation> {
        let binding = self.resolved()?.bindings.get(binding_id);
        self.site_location(binding.site)
    }

    fn references(
        &self,
        binding_id: NameBindingId,
        include_declaration: bool,
    ) -> Vec<ToolLocation> {
        let Some(resolved) = self.resolved() else {
            return Vec::new();
        };
        let mut locations = Vec::new();
        if include_declaration && let Some(location) = self.binding_location(binding_id) {
            locations.push(location);
        }
        locations.extend(
            resolved
                .refs
                .iter()
                .filter(|(_, target)| **target == binding_id)
                .filter_map(|(site, _)| self.site_location(*site)),
        );
        locations.sort_by_key(|location| {
            (
                location.path.clone(),
                location.range.start_line,
                location.range.start_col,
            )
        });
        locations
    }

    fn document_symbols(&self) -> Vec<ToolDocumentSymbol> {
        let Some(resolved) = self.resolved() else {
            return Vec::new();
        };
        let sema = self.sema();
        let mut symbols = resolved
            .bindings
            .iter()
            .filter(|(_, binding)| binding.site.source_id == self.source_id)
            .filter(|(_, binding)| {
                !matches!(
                    binding.kind,
                    NameBindingKind::Prelude | NameBindingKind::Import
                )
            })
            .map(|(binding_id, binding)| {
                let range = self
                    .source_for_site(binding.site)
                    .map_or(ToolRange::new(1, 1, 1, 1), |source| {
                        tool_range(source, binding.site.span)
                    });
                ToolDocumentSymbol {
                    name: self.session.resolve_symbol(binding.name).to_owned(),
                    kind: binding_symbol_kind(binding_id, binding, sema),
                    range,
                    selection_range: range,
                    children: Vec::new(),
                }
            })
            .collect::<Vec<_>>();
        symbols.sort_by_key(|symbol| {
            (
                symbol.range.start_line,
                symbol.range.start_col,
                symbol.name.clone(),
            )
        });
        symbols
    }

    fn workspace_symbols(&self, query: &str) -> Vec<ToolWorkspaceSymbol> {
        let query = query.to_ascii_lowercase();
        let Some(resolved) = self.resolved() else {
            return Vec::new();
        };
        let sema = self.sema();
        let mut symbols = resolved
            .bindings
            .iter()
            .filter(|(_, binding)| !matches!(binding.kind, NameBindingKind::Prelude))
            .filter_map(|(binding_id, binding)| {
                let name = self.session.resolve_symbol(binding.name).to_owned();
                if !query.is_empty() && !name.to_ascii_lowercase().contains(&query) {
                    return None;
                }
                Some(ToolWorkspaceSymbol {
                    name,
                    kind: binding_symbol_kind(binding_id, binding, sema),
                    location: self.binding_location(binding_id)?,
                })
            })
            .collect::<Vec<_>>();
        symbols.sort_by_key(|symbol| {
            (
                symbol.name.clone(),
                symbol.location.path.clone(),
                symbol.location.range.start_line,
                symbol.location.range.start_col,
            )
        });
        symbols
    }

    fn can_rename_binding(&self, binding: &NameBinding) -> bool {
        !matches!(
            binding.kind,
            NameBindingKind::Prelude | NameBindingKind::Import
        ) && self.site_location(binding.site).is_some()
    }

    fn source_for_site(&self, site: NameSite) -> Option<&Source> {
        self.session.source(site.source_id)
    }

    fn site_location(&self, site: NameSite) -> Option<ToolLocation> {
        let source = self.source_for_site(site)?;
        Some(ToolLocation {
            path: self.path_for_source(source)?,
            range: tool_range(source, site.span),
        })
    }

    fn path_for_source(&self, source: &Source) -> Option<PathBuf> {
        if source.id() == self.source_id {
            return Some(self.path.clone());
        }
        if source.path().exists() {
            return Some(source.path().to_path_buf());
        }
        let key = source.path().to_string_lossy();
        self.path_map
            .get(key.as_ref())
            .or_else(|| {
                key.strip_suffix("#expanded")
                    .and_then(|key| self.path_map.get(key))
            })
            .cloned()
    }
}

fn member_binding_at_offset(sema: &SemaModule, offset: u32) -> Option<NameBindingId> {
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
            sema.expr_member_fact(expr_id)?.binding
        })
}

fn module_path_map(path: &Path) -> HashMap<String, PathBuf> {
    load_project_ancestor(path, ProjectOptions::default())
        .ok()
        .map(|project| {
            project
                .workspace()
                .packages
                .values()
                .flat_map(|package| package.module_keys.iter())
                .map(|(key, path)| (key.as_str().to_owned(), path.clone()))
                .collect()
        })
        .unwrap_or_default()
}

fn is_valid_rename_name(name: &str) -> bool {
    let mut chars = name.chars();
    chars
        .next()
        .is_some_and(|ch| ch == '_' || ch.is_ascii_alphabetic())
        && chars.all(|ch| ch == '_' || ch.is_ascii_alphanumeric())
}
