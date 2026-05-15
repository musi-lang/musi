//! Formatting request helpers for the LSP server.

use std::fs;
use std::path::{Path, PathBuf};
use std::time::SystemTime;

use async_lsp::lsp_types::{
    DocumentFormattingParams, DocumentOnTypeFormattingParams, DocumentRangeFormattingParams,
    FormattingOptions, Position, Range, TextEdit, Url, WillSaveTextDocumentParams,
};
use musi_fmt::{FormatOptions, format_source, format_text_for_path};
use musi_project::{ProjectOptions, load_project_ancestor};

use super::MusiLanguageServer;
use super::convert::full_document_range;

#[derive(Debug, Clone)]
pub(super) struct FormatOptionsCacheEntry {
    modified: Option<SystemTime>,
    options: FormatOptions,
}

impl MusiLanguageServer {
    pub(super) fn document_formatting(
        &mut self,
        params: DocumentFormattingParams,
    ) -> Option<Vec<TextEdit>> {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let mut options = self.formatting_options_for_path(&path);
        apply_document_formatting_options(&mut options, &params.options);
        let text = self.open_documents.get(&uri)?;
        let formatted = format_text_for_path(&path, text, &options).ok()?;
        if !formatted.changed {
            return Some(Vec::new());
        }
        Some(vec![TextEdit::new(
            full_document_range(text),
            formatted.text,
        )])
    }

    pub(super) fn will_save_formatting(
        &mut self,
        params: WillSaveTextDocumentParams,
    ) -> Option<Vec<TextEdit>> {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let options = self.formatting_options_for_path(&path);
        let text = self.open_documents.get(&uri)?;
        let formatted = format_text_for_path(&path, text, &options).ok()?;
        if !formatted.changed {
            return Some(Vec::new());
        }
        Some(vec![TextEdit::new(
            full_document_range(text),
            formatted.text,
        )])
    }

    pub(super) fn document_on_type_formatting(
        &mut self,
        params: DocumentOnTypeFormattingParams,
    ) -> Option<Vec<TextEdit>> {
        if !on_type_formatting_trigger(&params.ch) {
            return Some(Vec::new());
        }
        let uri = params.text_document_position.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let mut options = self.formatting_options_for_path(&path);
        apply_document_formatting_options(&mut options, &params.options);
        let text = self.open_documents.get(&uri)?;
        let offset = lsp_position_offset(text, params.text_document_position.position)?;
        let (start, end) = line_offsets_around(text, offset)?;
        let selected = text.get(start..end)?;
        let formatted = format_source(selected, &options).ok()?;
        if !formatted.changed {
            return Some(Vec::new());
        }
        Some(vec![TextEdit::new(
            range_for_offsets(text, start, end)?,
            formatted.text,
        )])
    }

    pub(super) fn document_range_formatting(
        &mut self,
        params: DocumentRangeFormattingParams,
    ) -> Option<Vec<TextEdit>> {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let mut options = self.formatting_options_for_path(&path);
        apply_document_formatting_options(&mut options, &params.options);
        let text = self.open_documents.get(&uri)?;
        let (start, end) = lsp_range_offsets(text, params.range)?;
        let selected = text.get(start..end)?;
        let formatted = if markdown_range_inside_musi_fence_body(text, start, end) {
            format_source(selected, &options).ok()?
        } else {
            format_text_for_path(&path, selected, &options).ok()?
        };
        if !formatted.changed {
            return Some(Vec::new());
        }
        Some(vec![TextEdit::new(params.range, formatted.text)])
    }
    pub(super) fn formatting_options_for_path(&mut self, path: &Path) -> FormatOptions {
        let Some(manifest_path) = ancestor_manifest_path(path) else {
            return FormatOptions::default();
        };
        let modified = manifest_modified(&manifest_path);
        if let Some(entry) = self.format_options_cache.get(&manifest_path)
            && entry.modified == modified
        {
            return entry.options.clone();
        }
        let options = load_project_ancestor(path, ProjectOptions::default())
            .ok()
            .map_or_else(FormatOptions::default, |project| {
                FormatOptions::from_manifest(project.manifest().fmt.as_ref())
            });
        let _ = self.format_options_cache.insert(
            manifest_path,
            FormatOptionsCacheEntry {
                modified,
                options: options.clone(),
            },
        );
        options
    }

    pub(super) fn invalidate_format_options_cache_if_manifest(&mut self, uri: &Url) {
        if let Some(path) = uri
            .to_file_path()
            .ok()
            .filter(|path| path.file_name().is_some_and(|name| name == "musi.json"))
        {
            let _ = self.format_options_cache.remove(&path);
        }
    }
}

fn ancestor_manifest_path(path: &Path) -> Option<PathBuf> {
    let mut current = if path.is_dir() { path } else { path.parent()? };
    loop {
        let manifest = current.join("musi.json");
        if manifest.is_file() {
            return Some(manifest);
        }
        current = current.parent()?;
    }
}

fn manifest_modified(path: &Path) -> Option<SystemTime> {
    fs::metadata(path).ok()?.modified().ok()
}

pub(super) fn apply_document_formatting_options(
    options: &mut FormatOptions,
    formatting_options: &FormattingOptions,
) {
    options.indent_width = usize::try_from(formatting_options.tab_size).unwrap_or(2);
    options.use_tabs = !formatting_options.insert_spaces;
}

pub(super) fn lsp_range_offsets(text: &str, range: Range) -> Option<(usize, usize)> {
    let start = lsp_position_offset(text, range.start)?;
    let end = lsp_position_offset(text, range.end)?;
    (start <= end).then_some((start, end))
}

fn line_offsets_around(text: &str, offset: usize) -> Option<(usize, usize)> {
    if offset > text.len() || !text.is_char_boundary(offset) {
        return None;
    }
    let before = text.get(..offset)?;
    let after = text.get(offset..)?;
    let start = before
        .rfind('\n')
        .map_or(0, |index| index.saturating_add(1));
    let end = after.find('\n').map_or(text.len(), |index| {
        offset.saturating_add(index).saturating_add(1)
    });
    Some((start, end))
}

fn range_for_offsets(text: &str, start: usize, end: usize) -> Option<Range> {
    Some(Range::new(
        lsp_position_for_offset(text, start)?,
        lsp_position_for_offset(text, end)?,
    ))
}

fn lsp_position_for_offset(text: &str, target: usize) -> Option<Position> {
    if target > text.len() || !text.is_char_boundary(target) {
        return None;
    }
    let mut line = 0u32;
    let mut character = 0u32;
    for (offset, ch) in text.char_indices() {
        if offset == target {
            return Some(Position::new(line, character));
        }
        if ch == '\n' {
            line = line.saturating_add(1);
            character = 0;
        } else {
            character = character.saturating_add(u32::try_from(ch.len_utf16()).ok()?);
        }
    }
    (target == text.len()).then_some(Position::new(line, character))
}

pub(super) const fn on_type_formatting_trigger(ch: &str) -> bool {
    matches!(ch.as_bytes(), b";" | b")" | b"]" | b"}")
}

pub(super) fn markdown_range_inside_musi_fence_body(text: &str, start: usize, end: usize) -> bool {
    let mut offset = 0usize;
    let mut open_fence = None::<(MarkdownFence, usize)>;
    while offset < text.len() {
        let line_start = offset;
        let line = next_markdown_line(text, &mut offset);
        if let Some((fence, body_start)) = open_fence {
            if fence.is_closing(line) {
                if fence.is_musi && start >= body_start && end <= line_start {
                    return true;
                }
                open_fence = None;
                continue;
            }
            open_fence = Some((fence, body_start));
            continue;
        }
        if let Some(fence) = MarkdownFence::parse(line) {
            if start < offset {
                return false;
            }
            open_fence = Some((fence, offset));
        }
    }
    open_fence.is_some_and(|(fence, body_start)| {
        fence.is_musi && start >= body_start && end <= text.len()
    })
}

fn lsp_position_offset(text: &str, position: Position) -> Option<usize> {
    let target_line = usize::try_from(position.line).ok()?;
    let target_character = usize::try_from(position.character).ok()?;
    let mut line = 0usize;
    let mut character = 0usize;
    for (offset, ch) in text.char_indices() {
        if line == target_line && character == target_character {
            return Some(offset);
        }
        if ch == '\n' {
            line = line.saturating_add(1);
            character = 0;
        } else {
            character = character.saturating_add(ch.len_utf16());
        }
    }
    (line == target_line && character == target_character).then_some(text.len())
}

fn next_markdown_line<'a>(text: &'a str, offset: &mut usize) -> &'a str {
    let Some(rest) = text.get(*offset..) else {
        return "";
    };
    let end = rest
        .find('\n')
        .map_or(text.len(), |index| *offset + index + 1);
    let start = *offset;
    *offset = end;
    text.get(start..end).unwrap_or_default()
}

#[derive(Debug, Clone, Copy)]
struct MarkdownFence {
    marker: char,
    marker_len: usize,
    is_musi: bool,
}

impl MarkdownFence {
    fn parse(line: &str) -> Option<Self> {
        let trimmed = line.trim_start();
        let marker = trimmed.chars().next()?;
        if marker != '`' && marker != '~' {
            return None;
        }
        let marker_len = trimmed.chars().take_while(|char| *char == marker).count();
        if marker_len < 3 {
            return None;
        }
        let tag = markdown_fence_tag(trimmed.trim_start_matches(marker).trim());
        Some(Self {
            marker,
            marker_len,
            is_musi: ["musi", "ms", "music"]
                .iter()
                .any(|candidate| tag.eq_ignore_ascii_case(candidate)),
        })
    }

    fn is_closing(self, line: &str) -> bool {
        let trimmed = line.trim_start();
        let marker_len = trimmed
            .chars()
            .take_while(|char| *char == self.marker)
            .count();
        marker_len >= self.marker_len
            && trimmed
                .get(marker_len..)
                .is_some_and(|rest| rest.trim().is_empty())
    }
}

fn markdown_fence_tag(info: &str) -> &str {
    if let Some(attributes) = info.strip_prefix('{') {
        return attributes
            .trim_end_matches('}')
            .split(|char: char| char.is_whitespace())
            .find_map(|attribute| attribute.strip_prefix('.'))
            .unwrap_or_default();
    }
    info.split_whitespace().next().unwrap_or_default()
}
