use std::path::Path;

use async_lsp::lsp_types::{
    CallHierarchyItem, CodeActionKind, CodeLens, DocumentHighlight, DocumentHighlightKind,
    LinkedEditingRanges, Location, Moniker, MonikerKind, Position, Range, UniquenessLevel, Url,
};
use musi_tooling::{
    ToolDocumentSymbol, ToolRange, ToolReferenceLens, document_links_for_project_file_with_overlay,
};
use serde_json::{Value, json};

use super::convert::{to_lsp_range_in_text, to_tool_range, uri_for_path};
use super::workspace::paths_match;

pub(super) fn reference_lens_title(count: usize) -> String {
    if count == 1 {
        "1 reference".to_owned()
    } else {
        format!("{count} references")
    }
}

pub(super) fn push_reference_lens(
    path: &Path,
    lens: &ToolReferenceLens,
    lenses: &mut Vec<CodeLens>,
) {
    if lens.reference_count == 0 {
        return;
    }
    if let Some(data) = reference_lens_data(path, &lens.range) {
        lenses.push(CodeLens {
            range: to_tool_range(&lens.range),
            command: None,
            data: Some(data),
        });
    }
}

pub(super) fn symbol_at_position(
    symbols: &[ToolDocumentSymbol],
    position: Position,
) -> Option<&ToolDocumentSymbol> {
    symbols.iter().find_map(|symbol| {
        let selection_range = to_tool_range(&symbol.selection_range);
        if position_in_lsp_range(position, selection_range) {
            return Some(symbol);
        }
        symbol_at_position(&symbol.children, position)
    })
}

pub(super) fn caller_symbol_for_reference<'a>(
    symbols: &'a [ToolDocumentSymbol],
    range: &ToolRange,
) -> Option<&'a ToolDocumentSymbol> {
    symbols
        .iter()
        .flat_map(flatten_symbols)
        .filter(|symbol| tool_range_contains_range(&symbol.range, range))
        .min_by_key(|symbol| tool_range_size(&symbol.range))
}

fn flatten_symbols(symbol: &ToolDocumentSymbol) -> Vec<&ToolDocumentSymbol> {
    let mut symbols = vec![symbol];
    for child in &symbol.children {
        symbols.extend(flatten_symbols(child));
    }
    symbols
}

pub(super) fn call_hierarchy_item_data_parts(
    item: &CallHierarchyItem,
) -> Option<(Url, usize, usize)> {
    let data = item.data.as_ref()?;
    let uri = data.get("uri")?.as_str()?;
    let line = usize::try_from(data.get("line")?.as_u64()?).ok()?;
    let character = usize::try_from(data.get("character")?.as_u64()?).ok()?;
    Some((Url::parse(uri).ok()?, line, character))
}

pub(super) fn call_hierarchy_items_match(
    left: &CallHierarchyItem,
    right: &CallHierarchyItem,
) -> bool {
    left.uri == right.uri && left.selection_range == right.selection_range
}

pub(super) fn import_definition_at(
    path: &Path,
    text: &str,
    position: Position,
) -> Option<Location> {
    let link = document_links_for_project_file_with_overlay(path, Some(text))
        .into_iter()
        .find(|link| position_in_lsp_range(position, to_lsp_range_in_text(text, &link.range)))?;
    Some(Location {
        uri: uri_for_path(&link.target)?,
        range: Range::new(Position::new(0, 0), Position::new(0, 0)),
    })
}

pub(super) fn import_moniker_at(path: &Path, text: &str, position: Position) -> Option<Moniker> {
    let link = document_links_for_project_file_with_overlay(path, Some(text))
        .into_iter()
        .find(|link| position_in_lsp_range(position, to_lsp_range_in_text(text, &link.range)))?;
    let uri = uri_for_path(&link.target)?;
    Some(Moniker {
        scheme: "musi".to_owned(),
        identifier: format!("{}#1:1", uri.as_str()),
        unique: UniquenessLevel::Project,
        kind: Some(MonikerKind::Import),
    })
}

pub(super) fn import_document_highlights(
    path: &Path,
    text: &str,
    position: Position,
) -> Option<Vec<DocumentHighlight>> {
    let links = document_links_for_project_file_with_overlay(path, Some(text));
    let target = links
        .iter()
        .find(|link| position_in_lsp_range(position, to_lsp_range_in_text(text, &link.range)))?
        .target
        .clone();
    Some(
        links
            .into_iter()
            .filter(|link| paths_match(&link.target, &target))
            .map(|link| DocumentHighlight {
                range: to_lsp_range_in_text(text, &link.range),
                kind: Some(DocumentHighlightKind::TEXT),
            })
            .collect(),
    )
}

pub(super) fn import_linked_editing_ranges(
    path: &Path,
    text: &str,
    position: Position,
) -> Option<LinkedEditingRanges> {
    let ranges = import_document_highlights(path, text, position)?
        .into_iter()
        .map(|highlight| highlight.range)
        .collect::<Vec<_>>();
    (ranges.len() > 1).then_some(LinkedEditingRanges {
        ranges,
        word_pattern: None,
    })
}

pub(super) const fn position_in_lsp_range(position: Position, range: Range) -> bool {
    !position_lt(position, range.start) && position_lt(position, range.end)
}

const fn position_lt(left: Position, right: Position) -> bool {
    left.line < right.line || (left.line == right.line && left.character < right.character)
}

const fn tool_range_contains_range(container: &ToolRange, range: &ToolRange) -> bool {
    (range.start_line > container.start_line
        || range.start_line == container.start_line && range.start_col >= container.start_col)
        && (range.end_line < container.end_line
            || range.end_line == container.end_line && range.end_col <= container.end_col)
}

const fn tool_range_size(range: &ToolRange) -> (usize, usize) {
    (
        range.end_line.saturating_sub(range.start_line),
        range.end_col.saturating_sub(range.start_col),
    )
}

fn reference_lens_data(path: &Path, range: &ToolRange) -> Option<Value> {
    Some(json!({
        "uri": Url::from_file_path(path).ok()?.as_str(),
        "line": range.start_line.saturating_sub(1),
        "character": range.start_col.saturating_sub(1),
    }))
}

pub(super) fn reference_lens_data_parts(data: &Value) -> Option<(Url, usize, usize)> {
    let uri = data.get("uri")?.as_str()?;
    let line = usize::try_from(data.get("line")?.as_u64()?).ok()?;
    let character = usize::try_from(data.get("character")?.as_u64()?).ok()?;
    Some((Url::parse(uri).ok()?, line, character))
}

pub(super) fn code_action_kind_requested(
    only: Option<&[CodeActionKind]>,
    target: &CodeActionKind,
) -> bool {
    only.is_none_or(|kinds| {
        kinds.iter().any(|kind| {
            kind == target
                || target
                    .as_str()
                    .strip_prefix(kind.as_str())
                    .is_some_and(|suffix| suffix.starts_with('.'))
        })
    })
}
