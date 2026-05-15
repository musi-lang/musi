use std::cmp::Reverse;
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

use musi_project::{PackageSource, Project, ProjectOptions, load_project, load_project_ancestor};
use music_base::{Source, SourceId, Span};
use music_hir::{HirExpr, HirExprId, HirExprKind, HirPatKind, HirTyId, HirTyKind};
use music_module::ModuleKey;
use music_names::{NameBinding, NameBindingId, NameBindingKind, NameResolution, NameSite, Symbol};
use music_sema::{ExprMemberKind, SemaModule};
use music_session::Session;

use super::types::{
    ToolCallHierarchyItem, ToolDocumentHighlight, ToolDocumentHighlightKind, ToolDocumentSymbol,
    ToolLocation, ToolMoniker, ToolMonikerKind, ToolOutgoingCall, ToolReferenceLens, ToolTextEdit,
    ToolWorkspaceEdit, ToolWorkspaceSymbol,
};
use crate::analysis::{ToolRange, ToolSymbolKind, binding_symbol_kind, tool_range};
use crate::analysis_support::analysis_session;

#[must_use]
pub fn definition_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<ToolLocation> {
    let context = SymbolAnalysis::new(path, overlay_text)?;
    if let Some(binding_id) = context.binding_at(line, character) {
        return context.binding_location(binding_id);
    }
    context
        .import_record_member_definition_at(line, character)
        .or_else(|| context.variant_definition_at(line, character))
}

#[must_use]
pub fn type_definition_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<ToolLocation> {
    let context = SymbolAnalysis::new(path, overlay_text)?;
    context.type_definition_at(line, character)
}

#[must_use]
pub fn implementation_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Vec<ToolLocation> {
    let Some(context) = SymbolAnalysis::new(path, overlay_text) else {
        return Vec::new();
    };
    let Some(binding_id) = context.binding_at(line, character) else {
        return Vec::new();
    };
    context.implementations_for_binding(binding_id)
}

#[must_use]
pub fn moniker_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<ToolMoniker> {
    let context = SymbolAnalysis::new(path, overlay_text)?;
    let binding_id = context.binding_at(line, character)?;
    let binding = context.resolved()?.bindings.get(binding_id);
    Some(ToolMoniker {
        location: context.binding_location(binding_id)?,
        kind: match binding.kind {
            NameBindingKind::Import => ToolMonikerKind::Import,
            _ => ToolMonikerKind::Local,
        },
    })
}

#[must_use]
pub fn references_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
    include_declaration: bool,
) -> Vec<ToolLocation> {
    let Some(mut context) = SymbolAnalysis::new(path, overlay_text) else {
        return Vec::new();
    };
    if let Some(binding_id) = context.binding_at(line, character) {
        return context.references(binding_id, include_declaration);
    }
    context
        .import_record_member_references_at(line, character, include_declaration)
        .or_else(|| context.variant_references_at(line, character, include_declaration))
        .unwrap_or_default()
}

#[must_use]
pub fn document_highlights_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Vec<ToolDocumentHighlight> {
    let Some(context) = SymbolAnalysis::new(path, overlay_text) else {
        return Vec::new();
    };
    let Some(binding_id) = context.binding_at(line, character) else {
        return Vec::new();
    };
    context.document_highlights(binding_id)
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
pub fn reference_lenses_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
) -> Vec<ToolReferenceLens> {
    let Some(mut context) = SymbolAnalysis::new(path, overlay_text) else {
        return Vec::new();
    };
    context.reference_lenses()
}

#[must_use]
pub fn outgoing_calls_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Vec<ToolOutgoingCall> {
    let Some(context) = SymbolAnalysis::new(path, overlay_text) else {
        return Vec::new();
    };
    context.outgoing_calls(line, character)
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
pub fn workspace_symbols_for_project_root(root: &Path, query: &str) -> Vec<ToolWorkspaceSymbol> {
    let Ok(project) = load_project(root, ProjectOptions::default()) else {
        return Vec::new();
    };
    let mut symbols = project
        .workspace()
        .packages
        .values()
        .filter(|package| matches!(package.source, PackageSource::Workspace))
        .flat_map(|package| package.module_keys.values())
        .flat_map(|path| workspace_symbols_for_project_file_with_overlay(path, None, query))
        .collect::<Vec<_>>();
    symbols.extend(workspace_module_symbols(&project, query));
    symbols.sort_by_key(|symbol| {
        (
            symbol.name.clone(),
            symbol.location.path.clone(),
            symbol.location.range.start_line,
            symbol.location.range.start_col,
        )
    });
    symbols.dedup_by_key(|symbol| {
        (
            symbol.name.clone(),
            symbol.location.path.clone(),
            symbol.location.range.start_line,
            symbol.location.range.start_col,
        )
    });
    symbols
}

fn workspace_module_symbols(project: &Project, query: &str) -> Vec<ToolWorkspaceSymbol> {
    let query = query.to_ascii_lowercase();
    project
        .workspace()
        .packages
        .values()
        .filter(|package| matches!(package.source, PackageSource::Workspace))
        .flat_map(|package| {
            package.module_keys.values().filter_map(|path| {
                let name = module_symbol_name(package.root_dir.as_path(), path)?;
                if !query.is_empty() && !name.to_ascii_lowercase().contains(&query) {
                    return None;
                }
                Some(ToolWorkspaceSymbol {
                    name,
                    kind: ToolSymbolKind::Module,
                    location: ToolLocation {
                        path: path.clone(),
                        range: ToolRange::new(1, 1, 1, 1),
                    },
                })
            })
        })
        .collect()
}

fn module_symbol_name(package_root: &Path, module_path: &Path) -> Option<String> {
    let mut relative = module_path.strip_prefix(package_root).ok()?.to_path_buf();
    if relative
        .extension()
        .is_some_and(|extension| extension.eq_ignore_ascii_case("ms"))
    {
        let _ = relative.set_extension("");
    }
    Some(relative.to_string_lossy().replace('\\', "/"))
}

const fn callee_name_site(source_id: SourceId, expr: &HirExpr) -> Option<NameSite> {
    match expr.kind {
        HirExprKind::Name { name } => Some(NameSite::new(source_id, name.span)),
        _ => None,
    }
}

fn enclosing_let_range(sema: &SemaModule, binding_span: Span) -> Option<Span> {
    sema.module()
        .store
        .exprs
        .iter()
        .filter_map(|(_, expr)| {
            matches!(expr.kind, HirExprKind::Let { .. })
                .then_some(expr.origin.span)
                .filter(|span| span_contains_span(*span, binding_span))
        })
        .min_by_key(|span| span.end.saturating_sub(span.start))
}

const fn tool_range_contains_range(container: &ToolRange, range: &ToolRange) -> bool {
    (range.start_line > container.start_line
        || range.start_line == container.start_line && range.start_col >= container.start_col)
        && (range.end_line < container.end_line
            || range.end_line == container.end_line && range.end_col <= container.end_col)
}

const fn tool_range_strictly_contains_range(container: &ToolRange, range: &ToolRange) -> bool {
    tool_range_contains_range(container, range)
        && (container.start_line < range.start_line
            || container.start_line == range.start_line && container.start_col < range.start_col
            || range.end_line < container.end_line
            || range.end_line == container.end_line && range.end_col < container.end_col)
}

const fn span_contains_span(container: Span, span: Span) -> bool {
    container.start <= span.start && span.end <= container.end
}

fn nest_document_symbols(symbols: Vec<ToolDocumentSymbol>) -> Vec<ToolDocumentSymbol> {
    let mut roots = Vec::new();
    for symbol in symbols {
        push_nested_document_symbol(&mut roots, symbol);
    }
    roots
}

fn push_nested_document_symbol(symbols: &mut Vec<ToolDocumentSymbol>, symbol: ToolDocumentSymbol) {
    if let Some(parent) = symbols.iter_mut().rev().find(|candidate| {
        tool_range_strictly_contains_range(&candidate.range, &symbol.selection_range)
    }) {
        push_nested_document_symbol(&mut parent.children, symbol);
    } else {
        symbols.push(symbol);
    }
}

#[must_use]
pub fn prepare_rename_for_project_file_with_overlay(
    path: &Path,
    overlay_text: Option<&str>,
    line: usize,
    character: usize,
) -> Option<(ToolRange, String)> {
    let context = SymbolAnalysis::new(path, overlay_text)?;
    let (binding_id, site) = context.binding_site_at(line, character)?;
    let binding = context.resolved()?.bindings.get(binding_id);
    if !context.can_rename_binding(binding) {
        return None;
    }
    Some((
        tool_range(context.source_for_site(site)?, site.span),
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
    let mut context = SymbolAnalysis::new(path, overlay_text)?;
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

pub(super) struct SymbolAnalysis {
    session: Session,
    module_key: ModuleKey,
    source_id: SourceId,
    path: PathBuf,
    overlay_text: Option<String>,
    path_map: HashMap<String, PathBuf>,
    workspace_modules: Vec<(ModuleKey, PathBuf)>,
    binding_references: HashMap<NameBindingId, Vec<ToolLocation>>,
    workspace_import_record_member_references: HashMap<Symbol, Vec<ToolLocation>>,
    reference_lenses: Option<Vec<ToolReferenceLens>>,
}

#[derive(Debug, Clone)]
struct ImportRecordMemberTarget {
    name: Symbol,
    target: ModuleKey,
}

#[derive(Debug, Clone)]
struct VariantTarget {
    data_name: Symbol,
    variant_name: Symbol,
}

impl SymbolAnalysis {
    pub(super) fn new(path: &Path, overlay_text: Option<&str>) -> Option<Self> {
        let (session, module_key) = analysis_session(path, overlay_text)?;
        let parsed = session.parsed_module_cached(&module_key).ok().flatten()?;
        let source_id = parsed.source_id;
        Some(Self {
            session,
            module_key,
            source_id,
            path: path.to_path_buf(),
            overlay_text: overlay_text.map(str::to_owned),
            path_map: module_path_map(path),
            workspace_modules: workspace_modules(path),
            binding_references: HashMap::new(),
            workspace_import_record_member_references: HashMap::new(),
            reference_lenses: None,
        })
    }

    #[cfg(test)]
    pub(super) fn cached_reference_data_len(&self) -> (usize, usize, bool) {
        (
            self.binding_references.len(),
            self.workspace_import_record_member_references.len(),
            self.reference_lenses.is_some(),
        )
    }

    pub(super) fn overlay_matches(&self, overlay_text: Option<&str>) -> bool {
        self.overlay_text.as_deref() == overlay_text
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

    pub(super) fn binding_at(&self, line: usize, character: usize) -> Option<NameBindingId> {
        self.binding_site_at(line, character)
            .map(|(binding_id, _)| binding_id)
    }

    fn binding_site_at(&self, line: usize, character: usize) -> Option<(NameBindingId, NameSite)> {
        let source = self.source()?;
        let offset = source.offset(line, character)?;
        if let Some(sema) = self.sema()
            && let Some((binding_id, site)) =
                member_binding_site_at_offset(sema, self.source_id, offset)
        {
            return Some((binding_id, site));
        }
        let resolved = self.resolved()?;
        resolved
            .refs
            .iter()
            .find(|(site, _)| site.source_id == self.source_id && site.span.contains(offset))
            .map(|(site, binding_id)| (*binding_id, *site))
            .or_else(|| {
                resolved
                    .bindings
                    .iter()
                    .find(|(_, binding)| {
                        binding.site.source_id == self.source_id
                            && binding.site.span.contains(offset)
                    })
                    .map(|(binding_id, binding)| (binding_id, binding.site))
            })
    }

    fn import_record_member_definition_at(
        &self,
        line: usize,
        character: usize,
    ) -> Option<ToolLocation> {
        let source = self.source()?;
        let offset = source.offset(line, character)?;
        let target = self.import_record_member_target_at_offset(offset)?;
        self.export_location_for_target_name(&target.target, target.name)
    }

    fn import_record_member_references_at(
        &self,
        line: usize,
        character: usize,
        include_declaration: bool,
    ) -> Option<Vec<ToolLocation>> {
        let source = self.source()?;
        let offset = source.offset(line, character)?;
        let target = self.import_record_member_target_at_offset(offset)?;
        let target_path = self.path_map.get(target.target.as_str())?;
        let mut target_analysis = Self::new(target_path, None)?;
        let export_name = self.session.resolve_symbol(target.name).to_owned();
        let binding_id = target_analysis.export_binding_id(&export_name)?;
        Some(target_analysis.references(binding_id, include_declaration))
    }

    fn variant_definition_at(&self, line: usize, character: usize) -> Option<ToolLocation> {
        let source = self.source()?;
        let offset = source.offset(line, character)?;
        let target = self.variant_target_at_offset(offset)?;
        self.find_variant_definition_location(&target)
    }

    fn variant_references_at(
        &self,
        line: usize,
        character: usize,
        include_declaration: bool,
    ) -> Option<Vec<ToolLocation>> {
        let source = self.source()?;
        let offset = source.offset(line, character)?;
        let target = self.variant_target_at_offset(offset)?;
        let data_name = self.session.resolve_symbol(target.data_name).to_owned();
        let variant_name = self.session.resolve_symbol(target.variant_name).to_owned();
        let mut locations =
            self.variant_reference_locations_in_module_by_names(&data_name, &variant_name);
        for (module_key, module_path) in &self.workspace_modules {
            if module_key == &self.module_key {
                continue;
            }
            let Some(analysis) = Self::new(module_path, None) else {
                continue;
            };
            locations.extend(
                analysis.variant_reference_locations_in_module_by_names(&data_name, &variant_name),
            );
        }
        if include_declaration
            && let Some(definition) = self.find_variant_definition_location(&target)
        {
            locations.push(definition);
        }
        locations.sort_by_key(|location| {
            (
                location.path.clone(),
                location.range.start_line,
                location.range.start_col,
                location.range.end_line,
                location.range.end_col,
            )
        });
        locations.dedup_by_key(|location| {
            (
                location.path.clone(),
                location.range.start_line,
                location.range.start_col,
                location.range.end_line,
                location.range.end_col,
            )
        });
        Some(locations)
    }

    fn import_record_member_target_at_offset(
        &self,
        offset: u32,
    ) -> Option<ImportRecordMemberTarget> {
        let sema = self.sema()?;
        sema.module()
            .store
            .exprs
            .iter()
            .filter_map(|(expr_id, expr)| {
                let HirExprKind::Field { base, name, .. } = expr.kind else {
                    return None;
                };
                if expr.origin.source_id != self.source_id || !name.span.contains(offset) {
                    return None;
                }
                let fact = sema.expr_member_fact(expr_id)?;
                if fact.kind != ExprMemberKind::ImportRecordExport {
                    return None;
                }
                let target = fact
                    .import_record_target
                    .clone()
                    .or_else(|| sema.expr_import_record_target(expr_id).cloned())
                    .or_else(|| sema.expr_import_record_target(base).cloned())?;
                Some((
                    name.span.end.saturating_sub(name.span.start),
                    ImportRecordMemberTarget {
                        name: fact.name,
                        target,
                    },
                ))
            })
            .min_by_key(|(span_len, _)| *span_len)
            .map(|(_, target)| target)
    }

    fn export_binding_id(&self, name: &str) -> Option<NameBindingId> {
        let sema = self.sema()?;
        let _export = sema.surface().exported_value(name)?;
        self.resolved()?
            .bindings
            .iter()
            .filter(|(_, binding)| self.session.resolve_symbol(binding.name) == name)
            .filter(|(_, binding)| !matches!(binding.kind, NameBindingKind::Prelude))
            .min_by_key(|(_, binding)| binding.site.span.start)
            .map(|(binding_id, _)| binding_id)
    }

    fn export_location_for_target_name(
        &self,
        target: &ModuleKey,
        name: Symbol,
    ) -> Option<ToolLocation> {
        let target_path = self.path_map.get(target.as_str())?;
        let target_analysis = Self::new(target_path, None)?;
        let export_name = self.session.resolve_symbol(name).to_owned();
        let binding_id = target_analysis.export_binding_id(&export_name)?;
        target_analysis.binding_location(binding_id)
    }

    fn variant_target_at_offset(&self, offset: u32) -> Option<VariantTarget> {
        let sema = self.sema()?;
        let expr_target = sema
            .module()
            .store
            .exprs
            .iter()
            .filter_map(|(expr_id, expr)| {
                let HirExprKind::Variant { tag, .. } = expr.kind else {
                    return None;
                };
                if expr.origin.source_id != self.source_id || !tag.span.contains(offset) {
                    return None;
                }
                let ty = sema.try_expr_ty(expr_id)?;
                let data_name = ty_named_symbol(sema, ty)?;
                Some((
                    tag.span.end.saturating_sub(tag.span.start),
                    VariantTarget {
                        data_name,
                        variant_name: tag.name,
                    },
                ))
            })
            .min_by_key(|(span_len, _)| *span_len);
        let pat_target = sema
            .module()
            .store
            .pats
            .iter()
            .filter_map(|(pat_id, pat)| {
                let HirPatKind::Variant { tag, .. } = pat.kind else {
                    return None;
                };
                if pat.origin.source_id != self.source_id || !tag.span.contains(offset) {
                    return None;
                }
                let ty = sema.try_pat_ty(pat_id)?;
                let data_name = ty_named_symbol(sema, ty)?;
                Some((
                    tag.span.end.saturating_sub(tag.span.start),
                    VariantTarget {
                        data_name,
                        variant_name: tag.name,
                    },
                ))
            })
            .min_by_key(|(span_len, _)| *span_len);
        match (expr_target, pat_target) {
            (Some(expr), Some(pat)) => Some(if expr.0 <= pat.0 { expr.1 } else { pat.1 }),
            (Some(expr), None) => Some(expr.1),
            (None, Some(pat)) => Some(pat.1),
            (None, None) => None,
        }
    }

    fn find_variant_definition_location(&self, target: &VariantTarget) -> Option<ToolLocation> {
        let data_name = self.session.resolve_symbol(target.data_name).to_owned();
        let variant_name = self.session.resolve_symbol(target.variant_name).to_owned();
        if let Some(location) =
            self.variant_definition_location_in_module_by_names(&data_name, &variant_name)
        {
            return Some(location);
        }
        for (module_key, module_path) in &self.workspace_modules {
            if module_key == &self.module_key {
                continue;
            }
            let Some(analysis) = Self::new(module_path, None) else {
                continue;
            };
            if let Some(location) =
                analysis.variant_definition_location_in_module_by_names(&data_name, &variant_name)
            {
                return Some(location);
            }
        }
        None
    }

    fn variant_definition_location_in_module_by_names(
        &self,
        data_name: &str,
        variant_name: &str,
    ) -> Option<ToolLocation> {
        let sema = self.sema()?;
        let mut best_location = None;
        let mut best_span_len = u32::MAX;
        for (_, expr) in &sema.module().store.exprs {
            let HirExprKind::Let { pat, value, .. } = expr.kind else {
                continue;
            };
            let pat = sema.module().store.pats.get(pat);
            let HirPatKind::Bind { name } = pat.kind else {
                continue;
            };
            if self.session.resolve_symbol(name.name) != data_name {
                continue;
            }
            let value_expr = sema.module().store.exprs.get(value);
            let HirExprKind::Data { ref variants, .. } = value_expr.kind else {
                continue;
            };
            for variant in sema.module().store.variants.get(variants.clone()) {
                if self.session.resolve_symbol(variant.name.name) != variant_name {
                    continue;
                }
                let span_len = variant
                    .name
                    .span
                    .end
                    .saturating_sub(variant.name.span.start);
                if span_len < best_span_len {
                    let site = NameSite::new(variant.origin.source_id, variant.name.span);
                    let Some(location) = self.site_location(site) else {
                        continue;
                    };
                    best_span_len = span_len;
                    best_location = Some(location);
                }
            }
        }
        best_location
    }

    fn variant_reference_locations_in_module_by_names(
        &self,
        data_name: &str,
        variant_name: &str,
    ) -> Vec<ToolLocation> {
        let Some(sema) = self.sema() else {
            return Vec::new();
        };
        let mut locations = sema
            .module()
            .store
            .exprs
            .iter()
            .filter_map(|(expr_id, expr)| {
                let HirExprKind::Variant { tag, .. } = expr.kind else {
                    return None;
                };
                let ty = sema.try_expr_ty(expr_id)?;
                let ty_name = self.session.resolve_symbol(ty_named_symbol(sema, ty)?);
                if ty_name != data_name || self.session.resolve_symbol(tag.name) != variant_name {
                    return None;
                }
                self.site_location(NameSite::new(expr.origin.source_id, tag.span))
            })
            .collect::<Vec<_>>();
        locations.extend(sema.module().store.pats.iter().filter_map(|(pat_id, pat)| {
            let HirPatKind::Variant { tag, .. } = pat.kind else {
                return None;
            };
            let ty = sema.try_pat_ty(pat_id)?;
            let ty_name = self.session.resolve_symbol(ty_named_symbol(sema, ty)?);
            if ty_name != data_name || self.session.resolve_symbol(tag.name) != variant_name {
                return None;
            }
            self.site_location(NameSite::new(pat.origin.source_id, tag.span))
        }));
        locations
    }

    fn type_definition_at(&self, line: usize, character: usize) -> Option<ToolLocation> {
        let source = self.source()?;
        let offset = source.offset(line, character)?;
        let sema = self.sema()?;
        let ty = self
            .binding_at(line, character)
            .and_then(|binding_id| sema.binding_type(binding_id))
            .or_else(|| expr_ty_at_offset(sema, self.source_id, offset))?;
        self.type_definition_location(sema, ty)
    }

    fn type_definition_location(&self, sema: &SemaModule, ty: HirTyId) -> Option<ToolLocation> {
        match sema.ty(ty).kind.clone() {
            HirTyKind::Named { name, .. } => self.type_binding_location(sema, name),
            HirTyKind::Mut { inner } => self.type_definition_location(sema, inner),
            _ => None,
        }
    }

    fn type_binding_location(&self, sema: &SemaModule, name: Symbol) -> Option<ToolLocation> {
        let resolved = self.resolved()?;
        resolved
            .bindings
            .iter()
            .find(|(binding_id, binding)| {
                binding.name == name
                    && sema
                        .binding_type(*binding_id)
                        .is_some_and(|ty| matches!(sema.ty(ty).kind, HirTyKind::Type))
            })
            .and_then(|(binding_id, _)| self.binding_location(binding_id))
    }

    fn implementations_for_binding(&self, binding_id: NameBindingId) -> Vec<ToolLocation> {
        let Some(resolved) = self.resolved() else {
            return Vec::new();
        };
        let Some(sema) = self.sema() else {
            return Vec::new();
        };
        let binding = resolved.bindings.get(binding_id);
        let Some(_shape) = sema.shape_facts_by_name(binding.name) else {
            return Vec::new();
        };
        Vec::new()
    }

    fn binding_location(&self, binding_id: NameBindingId) -> Option<ToolLocation> {
        let binding = self.resolved()?.bindings.get(binding_id);
        self.site_location(binding.site)
    }

    fn member_references(&self, binding_id: NameBindingId) -> Vec<ToolLocation> {
        let Some(sema) = self.sema() else {
            return Vec::new();
        };
        sema.module()
            .store
            .exprs
            .iter()
            .filter_map(|(expr_id, expr)| {
                let HirExprKind::Field { name, .. } = expr.kind else {
                    return None;
                };
                let fact = sema.expr_member_fact(expr_id)?;
                if fact.binding != Some(binding_id) {
                    return None;
                }
                self.site_location(NameSite::new(expr.origin.source_id, name.span))
            })
            .collect()
    }

    pub(super) fn references(
        &mut self,
        binding_id: NameBindingId,
        include_declaration: bool,
    ) -> Vec<ToolLocation> {
        let mut locations = self.references_without_declaration(binding_id);
        if include_declaration && let Some(location) = self.binding_location(binding_id) {
            locations.push(location);
            locations.sort_by_key(|location| {
                (
                    location.path.clone(),
                    location.range.start_line,
                    location.range.start_col,
                )
            });
            locations.dedup_by_key(|location| {
                (
                    location.path.clone(),
                    location.range.start_line,
                    location.range.start_col,
                    location.range.end_line,
                    location.range.end_col,
                )
            });
        }
        locations
    }

    fn references_without_declaration(&mut self, binding_id: NameBindingId) -> Vec<ToolLocation> {
        if let Some(cached) = self.binding_references.get(&binding_id) {
            return cached.clone();
        }
        let Some(resolved) = self.resolved() else {
            return Vec::new();
        };
        let binding_name = resolved.bindings.get(binding_id).name;
        let has_workspace_import_record_references =
            self.has_workspace_import_record_references(binding_id);
        let mut locations = Vec::new();
        locations.extend(
            resolved
                .refs
                .iter()
                .filter(|(_, target)| **target == binding_id)
                .filter_map(|(site, _)| self.site_location(*site)),
        );
        locations.extend(self.member_references(binding_id));
        if has_workspace_import_record_references {
            locations.extend(self.workspace_import_record_member_references(binding_name));
        }
        locations.sort_by_key(|location| {
            (
                location.path.clone(),
                location.range.start_line,
                location.range.start_col,
            )
        });
        locations.dedup_by_key(|location| {
            (
                location.path.clone(),
                location.range.start_line,
                location.range.start_col,
                location.range.end_line,
                location.range.end_col,
            )
        });
        let _ = self
            .binding_references
            .insert(binding_id, locations.clone());
        locations
    }

    fn workspace_import_record_member_references(&mut self, name: Symbol) -> Vec<ToolLocation> {
        if let Some(cached) = self.workspace_import_record_member_references.get(&name) {
            return cached.clone();
        }
        let mut modules = self.workspace_modules.clone();
        modules.retain(|(module_key, _)| module_key != &self.module_key);
        let mut locations = Vec::new();
        for (module_key, path) in modules {
            locations.extend(self.import_record_member_references_in_module(
                &module_key,
                &path,
                name,
            ));
        }
        locations.sort_by_key(|location| {
            (
                location.path.clone(),
                location.range.start_line,
                location.range.start_col,
            )
        });
        locations.dedup_by_key(|location| {
            (
                location.path.clone(),
                location.range.start_line,
                location.range.start_col,
                location.range.end_line,
                location.range.end_col,
            )
        });
        let _ = self
            .workspace_import_record_member_references
            .insert(name, locations.clone());
        locations
    }

    fn import_record_member_references_in_module(
        &mut self,
        module_key: &ModuleKey,
        path: &Path,
        name: Symbol,
    ) -> Vec<ToolLocation> {
        let Ok(sema) = self.session.check_module(module_key) else {
            return Vec::new();
        };
        let sites = sema
            .module()
            .store
            .exprs
            .iter()
            .filter_map(|(expr_id, expr)| {
                let HirExprKind::Field {
                    base, name: field, ..
                } = expr.kind
                else {
                    return None;
                };
                let fact = sema.expr_member_fact(expr_id)?;
                if !matches!(fact.kind, ExprMemberKind::ImportRecordExport) || fact.name != name {
                    return None;
                }
                let target = fact
                    .import_record_target
                    .as_ref()
                    .or_else(|| sema.expr_import_record_target(base))?;
                (target == &self.module_key)
                    .then_some(NameSite::new(expr.origin.source_id, field.span))
            })
            .collect::<Vec<_>>();
        let mut locations = Vec::new();
        for site in sites {
            let Some(source) = self.source_for_site(site) else {
                continue;
            };
            let path = self
                .path_for_source(source)
                .unwrap_or_else(|| path.to_path_buf());
            locations.push(ToolLocation {
                path,
                range: tool_range(source, site.span),
            });
        }
        locations
    }

    fn document_highlights(&self, binding_id: NameBindingId) -> Vec<ToolDocumentHighlight> {
        let Some(resolved) = self.resolved() else {
            return Vec::new();
        };
        let mut highlights = Vec::new();
        if let Some(location) = self.binding_location(binding_id) {
            highlights.push(ToolDocumentHighlight {
                location,
                kind: ToolDocumentHighlightKind::Write,
            });
        }
        highlights.extend(
            resolved
                .refs
                .iter()
                .filter(|(_, target)| **target == binding_id)
                .filter_map(|(site, _)| {
                    Some(ToolDocumentHighlight {
                        location: self.site_location(*site)?,
                        kind: ToolDocumentHighlightKind::Read,
                    })
                }),
        );
        highlights.extend(
            self.member_references(binding_id)
                .into_iter()
                .map(|location| ToolDocumentHighlight {
                    location,
                    kind: ToolDocumentHighlightKind::Read,
                }),
        );
        highlights.sort_by_key(|highlight| {
            (
                highlight.location.path.clone(),
                highlight.location.range.start_line,
                highlight.location.range.start_col,
            )
        });
        highlights.dedup_by_key(|highlight| {
            (
                highlight.location.path.clone(),
                highlight.location.range.start_line,
                highlight.location.range.start_col,
                highlight.location.range.end_line,
                highlight.location.range.end_col,
            )
        });
        highlights
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
                let selection_range = self
                    .source_for_site(binding.site)
                    .map_or(ToolRange::new(1, 1, 1, 1), |source| {
                        tool_range(source, binding.site.span)
                    });
                let range = sema
                    .and_then(|sema| enclosing_let_range(sema, binding.site.span))
                    .and_then(|span| {
                        self.source_for_site(binding.site)
                            .map(|source| tool_range(source, span))
                    })
                    .unwrap_or(selection_range);
                ToolDocumentSymbol {
                    name: self.session.resolve_symbol(binding.name).to_owned(),
                    kind: binding_symbol_kind(binding_id, binding, sema),
                    range,
                    selection_range,
                    children: Vec::new(),
                }
            })
            .collect::<Vec<_>>();
        symbols.sort_by_key(|symbol| {
            (
                symbol.range.start_line,
                symbol.range.start_col,
                Reverse(symbol.range.end_line),
                Reverse(symbol.range.end_col),
                symbol.name.clone(),
            )
        });
        nest_document_symbols(symbols)
    }

    pub(super) fn reference_lenses(&mut self) -> Vec<ToolReferenceLens> {
        if let Some(cached) = &self.reference_lenses {
            return cached.clone();
        }
        let Some(resolved) = self.resolved() else {
            return Vec::new();
        };
        let mut reference_counts = HashMap::<NameBindingId, usize>::new();
        for binding_id in resolved.refs.values() {
            *reference_counts.entry(*binding_id).or_default() += 1;
        }
        if let Some(sema) = self.sema() {
            for (expr_id, expr) in &sema.module().store.exprs {
                let HirExprKind::Field { .. } = expr.kind else {
                    continue;
                };
                let Some(binding_id) = sema.expr_member_fact(expr_id).and_then(|fact| fact.binding)
                else {
                    continue;
                };
                *reference_counts.entry(binding_id).or_default() += 1;
            }
        }
        let mut bindings = resolved
            .bindings
            .iter()
            .filter(|(_, binding)| binding.site.source_id == self.source_id)
            .filter(|(_, binding)| {
                !matches!(
                    binding.kind,
                    NameBindingKind::Prelude | NameBindingKind::Import
                )
            })
            .map(|(binding_id, binding)| (binding_id, binding.site, binding.name))
            .collect::<Vec<_>>();
        bindings.sort_by_key(|(_, site, _)| site.span.start);
        let workspace_reference_counts = bindings
            .iter()
            .filter(|(binding_id, _, _)| self.has_workspace_import_record_references(*binding_id))
            .map(|(_, _, name)| *name)
            .collect::<HashSet<_>>()
            .into_iter()
            .map(|name| {
                (
                    name,
                    self.workspace_import_record_member_references(name).len(),
                )
            })
            .collect::<HashMap<_, _>>();
        let lenses: Vec<ToolReferenceLens> = bindings
            .into_iter()
            .filter_map(|(binding_id, site, binding_name)| {
                Some(ToolReferenceLens {
                    range: tool_range(self.source_for_site(site)?, site.span),
                    reference_count: reference_counts
                        .get(&binding_id)
                        .copied()
                        .unwrap_or_default()
                        + workspace_reference_counts
                            .get(&binding_name)
                            .copied()
                            .unwrap_or_default(),
                })
            })
            .collect();
        self.reference_lenses = Some(lenses.clone());
        lenses
    }

    fn has_workspace_import_record_references(&self, binding_id: NameBindingId) -> bool {
        let Some(resolved) = self.resolved() else {
            return false;
        };
        let Some(sema) = self.sema() else {
            return false;
        };
        let binding = resolved.bindings.get(binding_id);
        if !matches!(
            binding.kind,
            NameBindingKind::Let | NameBindingKind::Pin | NameBindingKind::AttachedMethod
        ) {
            return false;
        }
        let name = self.session.resolve_symbol(binding.name);
        sema.surface().exported_value(name).is_some()
    }

    fn outgoing_calls(&self, line: usize, character: usize) -> Vec<ToolOutgoingCall> {
        let Some(source) = self.source() else {
            return Vec::new();
        };
        let Some(sema) = self.sema() else {
            return Vec::new();
        };
        let Some(resolved) = self.resolved() else {
            return Vec::new();
        };
        let Some(binding_id) = self.binding_at(line, character) else {
            return Vec::new();
        };
        let binding = resolved.bindings.get(binding_id);
        let container_range = enclosing_let_range(sema, binding.site.span).map_or_else(
            || tool_range(source, binding.site.span),
            |span| tool_range(source, span),
        );
        let mut calls = Vec::<ToolOutgoingCall>::new();
        for (_, expr) in &sema.module().store.exprs {
            let HirExprKind::Call { callee, .. } = expr.kind else {
                continue;
            };
            let callee_expr = sema.module().store.exprs.get(callee);
            let Some((callee_binding_id, callee_span)) =
                call_target(self.source_id, resolved, sema, callee, callee_expr)
            else {
                continue;
            };
            let range = tool_range(source, callee_span);
            if !tool_range_contains_range(&container_range, &range) {
                continue;
            }
            let binding = resolved.bindings.get(callee_binding_id);
            let Some(location) = self.binding_location(callee_binding_id) else {
                continue;
            };
            let to = ToolCallHierarchyItem {
                name: self.session.resolve_symbol(binding.name).to_owned(),
                kind: binding_symbol_kind(callee_binding_id, binding, Some(sema)),
                location,
            };
            if let Some(call) = calls.iter_mut().find(|call| call.to == to) {
                call.from_ranges.push(range);
            } else {
                calls.push(ToolOutgoingCall {
                    to,
                    from_ranges: vec![range],
                });
            }
        }
        calls.sort_by_key(|call| {
            (
                call.to.name.clone(),
                call.to.location.path.clone(),
                call.to.location.range.start_line,
                call.to.location.range.start_col,
            )
        });
        calls
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

fn call_target(
    source_id: SourceId,
    resolved: &NameResolution,
    sema: &SemaModule,
    callee_id: HirExprId,
    callee: &HirExpr,
) -> Option<(NameBindingId, Span)> {
    if let Some(site) = callee_name_site(source_id, callee) {
        return resolved
            .refs
            .get(&site)
            .copied()
            .map(|binding_id| (binding_id, site.span));
    }
    let HirExprKind::Field { name, .. } = callee.kind else {
        return None;
    };
    if callee.origin.source_id != source_id {
        return None;
    }
    sema.expr_member_fact(callee_id)
        .and_then(|fact| fact.binding)
        .map(|binding_id| (binding_id, name.span))
}

fn expr_ty_at_offset(sema: &SemaModule, source_id: SourceId, offset: u32) -> Option<HirTyId> {
    sema.module()
        .store
        .exprs
        .iter()
        .filter_map(|(expr_id, expr)| {
            if expr.origin.source_id != source_id || !expr.origin.span.contains(offset) {
                return None;
            }
            Some((expr_id, expr.origin.span))
        })
        .min_by_key(|(_, span)| span.end.saturating_sub(span.start))
        .and_then(|(expr_id, _)| sema.try_expr_ty(expr_id))
}

fn ty_named_symbol(sema: &SemaModule, ty: HirTyId) -> Option<Symbol> {
    match sema.ty(ty).kind {
        HirTyKind::Named { name, .. } => Some(name),
        HirTyKind::Mut { inner } => ty_named_symbol(sema, inner),
        _ => None,
    }
}

fn member_binding_site_at_offset(
    sema: &SemaModule,
    source_id: SourceId,
    offset: u32,
) -> Option<(NameBindingId, NameSite)> {
    sema.module()
        .store
        .exprs
        .iter()
        .filter_map(|(expr_id, expr)| {
            let HirExprKind::Field { name, .. } = expr.kind else {
                return None;
            };
            if expr.origin.source_id != source_id || !name.span.contains(offset) {
                return None;
            }
            let binding_id = sema.expr_member_fact(expr_id)?.binding?;
            Some((
                name.span.end.saturating_sub(name.span.start),
                (binding_id, NameSite::new(expr.origin.source_id, name.span)),
            ))
        })
        .min_by_key(|(span_len, _)| *span_len)
        .map(|(_, site)| site)
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

fn workspace_modules(path: &Path) -> Vec<(ModuleKey, PathBuf)> {
    load_project_ancestor(path, ProjectOptions::default())
        .ok()
        .map(|project| {
            project
                .workspace()
                .packages
                .values()
                .filter(|package| matches!(package.source, PackageSource::Workspace))
                .flat_map(|package| {
                    package
                        .module_keys
                        .iter()
                        .map(|(key, path)| (key.clone(), path.clone()))
                })
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
