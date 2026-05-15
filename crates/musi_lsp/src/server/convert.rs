use std::ffi::OsStr;
use std::path::{Component, Path, PathBuf};

use async_lsp::lsp_types::{
    CallHierarchyItem, CompletionItem, CompletionItemKind, CompletionTextEdit, Diagnostic,
    DiagnosticRelatedInformation, DiagnosticSeverity, DocumentHighlightKind, DocumentLink,
    DocumentSymbol, Documentation, FoldingRange, FoldingRangeKind, InlayHint, InlayHintKind,
    InlayHintLabel, InlayHintTooltip, Location, NumberOrString, OneOf, ParameterInformation,
    ParameterLabel, Position, Range, SelectionRange, SemanticToken, SemanticTokenModifier,
    SemanticTokenType, SemanticTokensLegend, SignatureHelp, SignatureInformation, SymbolKind,
    TextEdit, Url, WorkspaceLocation, WorkspaceSymbol,
};
use musi_tooling::ToolDocumentHighlightKind::{Read, Text, Write};
use musi_tooling::{
    CliDiagnostic, CliDiagnosticLabel, CliDiagnosticRange, ToolCompletion, ToolCompletionKind,
    ToolDocumentHighlightKind, ToolDocumentLink, ToolDocumentSymbol, ToolFoldingRange,
    ToolFoldingRangeKind, ToolInlayHint, ToolInlayHintKind, ToolLocation, ToolPosition, ToolRange,
    ToolSelectionRange, ToolSemanticModifier, ToolSemanticToken, ToolSemanticTokenKind,
    ToolSignatureHelp, ToolSignatureInformation, ToolSymbolKind, ToolWorkspaceSymbol,
};
use serde_json::{Value, json};

pub(super) fn to_lsp_completion(text: &str, completion: ToolCompletion) -> CompletionItem {
    let insert_text = completion
        .insert_text
        .clone()
        .unwrap_or_else(|| completion.label.clone());
    CompletionItem {
        label: completion.label,
        kind: Some(to_completion_item_kind(completion.kind)),
        detail: None,
        documentation: None,
        sort_text: completion.sort_text,
        filter_text: completion.filter_text,
        text_edit: Some(CompletionTextEdit::Edit(TextEdit::new(
            to_lsp_range_in_text(text, &completion.replace_range),
            insert_text,
        ))),
        data: Some(json!({
            "detail": completion.detail,
            "documentation": completion.documentation,
        })),
        ..CompletionItem::default()
    }
}

pub(super) fn to_tool_position_in_text(text: &str, position: Position) -> Option<ToolPosition> {
    let target_line = usize::try_from(position.line).ok()?;
    let target_character = usize::try_from(position.character).ok()?;
    let line = text_line(text, target_line)?;
    let mut utf16_character = 0usize;
    let mut character_col = 1usize;
    for ch in line.chars() {
        if utf16_character == target_character {
            return Some(ToolPosition::new(
                target_line.saturating_add(1),
                character_col,
            ));
        }
        utf16_character = utf16_character.saturating_add(ch.len_utf16());
        character_col = character_col.saturating_add(1);
        if utf16_character > target_character {
            return None;
        }
    }
    (utf16_character == target_character).then_some(ToolPosition::new(
        target_line.saturating_add(1),
        character_col,
    ))
}

pub(super) fn to_lsp_range_in_text(text: &str, range: &ToolRange) -> Range {
    Range {
        start: to_lsp_position_in_text(text, range.start_line, range.start_col),
        end: to_lsp_position_in_text(text, range.end_line, range.end_col),
    }
}

pub(super) fn to_lsp_position_in_text(text: &str, line: usize, col: usize) -> Position {
    let lsp_line = line.saturating_sub(1);
    let character = text_line(text, lsp_line).map_or_else(
        || col.saturating_sub(1),
        |line_text| {
            line_text
                .chars()
                .take(col.saturating_sub(1))
                .map(char::len_utf16)
                .sum::<usize>()
        },
    );
    Position::new(usize_to_u32(lsp_line), usize_to_u32(character))
}

struct LineIndex<'a> {
    lines: Vec<&'a str>,
}

impl<'a> LineIndex<'a> {
    fn new(text: &'a str) -> Self {
        let mut lines = text
            .split('\n')
            .map(|line| line.strip_suffix('\r').unwrap_or(line))
            .collect::<Vec<_>>();
        if text.ends_with('\n') {
            let _ = lines.pop();
        }
        Self { lines }
    }

    fn range(&self, range: &ToolRange) -> Range {
        Range {
            start: self.position(range.start_line, range.start_col),
            end: self.position(range.end_line, range.end_col),
        }
    }

    fn position(&self, line: usize, col: usize) -> Position {
        let lsp_line = line.saturating_sub(1);
        let character = self.lines.get(lsp_line).map_or_else(
            || col.saturating_sub(1),
            |line_text| {
                line_text
                    .chars()
                    .take(col.saturating_sub(1))
                    .map(char::len_utf16)
                    .sum::<usize>()
            },
        );
        Position::new(usize_to_u32(lsp_line), usize_to_u32(character))
    }
}

fn text_line(text: &str, zero_based_line: usize) -> Option<&str> {
    let mut current = 0usize;
    let mut start = 0usize;
    for (offset, ch) in text.char_indices() {
        if current == zero_based_line && ch == '\n' {
            return text
                .get(start..offset)
                .map(|line| line.strip_suffix('\r').unwrap_or(line));
        }
        if ch == '\n' {
            current = current.saturating_add(1);
            start = offset.saturating_add(1);
        }
    }
    (current == zero_based_line)
        .then(|| {
            text.get(start..)
                .map(|line| line.strip_suffix('\r').unwrap_or(line))
        })
        .flatten()
}

pub(super) fn resolve_lsp_completion(mut completion: CompletionItem) -> CompletionItem {
    if completion.detail.is_some() || completion.documentation.is_some() {
        return completion;
    }
    let Some(Value::Object(data)) = completion.data.as_ref() else {
        return completion;
    };
    if let Some(detail) = data.get("detail").and_then(Value::as_str) {
        completion.detail = Some(detail.to_owned());
    }
    if let Some(documentation) = data.get("documentation").and_then(Value::as_str) {
        completion.documentation = Some(Documentation::String(documentation.to_owned()));
    }
    completion
}

pub(super) const fn to_lsp_document_highlight_kind(
    kind: ToolDocumentHighlightKind,
) -> DocumentHighlightKind {
    match kind {
        Read => DocumentHighlightKind::READ,
        Text => DocumentHighlightKind::TEXT,
        Write => DocumentHighlightKind::WRITE,
    }
}

pub(super) fn to_lsp_folding_range(range: &ToolFoldingRange) -> FoldingRange {
    let lsp_range = to_tool_range(&range.range);
    FoldingRange {
        start_line: lsp_range.start.line,
        start_character: Some(lsp_range.start.character),
        end_line: lsp_range.end.line,
        end_character: Some(lsp_range.end.character),
        kind: range.kind.map(to_folding_range_kind),
        collapsed_text: None,
    }
}

pub(super) fn to_lsp_document_link(text: &str, link: &ToolDocumentLink) -> Option<DocumentLink> {
    let target = uri_for_path(&link.target)?;
    Some(DocumentLink {
        range: to_lsp_range_in_text(text, &link.range),
        target: None,
        tooltip: None,
        data: Some(json!({
            "target": target.as_str(),
            "tooltip": link.tooltip,
        })),
    })
}

pub(super) fn uri_for_path(path: &Path) -> Option<Url> {
    Url::from_file_path(path)
        .ok()
        .or_else(|| Url::parse(&path.to_string_lossy()).ok())
}

pub(super) fn resolve_lsp_document_link(mut link: DocumentLink) -> DocumentLink {
    if link.target.is_some() {
        return link;
    }
    let Some(Value::Object(data)) = link.data.as_ref() else {
        return link;
    };
    if let Some(target) = data
        .get("target")
        .and_then(Value::as_str)
        .and_then(|target| Url::parse(target).ok())
    {
        link.target = Some(target);
    }
    if let Some(tooltip) = data.get("tooltip").and_then(Value::as_str) {
        link.tooltip = Some(tooltip.to_owned());
    }
    link
}

pub(super) fn to_lsp_selection_range(range: ToolSelectionRange) -> SelectionRange {
    SelectionRange {
        range: to_tool_range(&range.range),
        parent: range
            .parent
            .map(|parent| Box::new(to_lsp_selection_range(*parent))),
    }
}

pub(super) fn to_lsp_signature_help(help: ToolSignatureHelp) -> SignatureHelp {
    SignatureHelp {
        signatures: help
            .signatures
            .into_iter()
            .map(to_lsp_signature_information)
            .collect(),
        active_signature: Some(usize_to_u32(help.active_signature)),
        active_parameter: Some(usize_to_u32(help.active_parameter)),
    }
}

fn to_lsp_signature_information(signature: ToolSignatureInformation) -> SignatureInformation {
    SignatureInformation {
        label: signature.label,
        documentation: None,
        parameters: Some(
            signature
                .parameters
                .into_iter()
                .map(|parameter| ParameterInformation {
                    label: ParameterLabel::Simple(parameter.label),
                    documentation: None,
                })
                .collect(),
        ),
        active_parameter: None,
    }
}

pub(super) fn to_lsp_document_symbol(symbol: ToolDocumentSymbol) -> DocumentSymbol {
    #[allow(deprecated)]
    DocumentSymbol {
        name: symbol.name,
        detail: None,
        kind: to_lsp_symbol_kind(symbol.kind),
        tags: None,
        deprecated: None,
        range: to_tool_range(&symbol.range),
        selection_range: to_tool_range(&symbol.selection_range),
        children: (!symbol.children.is_empty()).then(|| {
            symbol
                .children
                .into_iter()
                .map(to_lsp_document_symbol)
                .collect()
        }),
    }
}

pub(super) fn to_lsp_workspace_symbol(symbol: ToolWorkspaceSymbol) -> Option<WorkspaceSymbol> {
    Some(WorkspaceSymbol {
        name: symbol.name,
        kind: to_lsp_symbol_kind(symbol.kind),
        tags: None,
        container_name: None,
        location: OneOf::Right(WorkspaceLocation {
            uri: Url::from_file_path(symbol.location.path).ok()?,
        }),
        data: Some(json!({
            "range": to_tool_range(&symbol.location.range),
        })),
    })
}

pub(super) fn to_lsp_call_hierarchy_item(
    uri: &Url,
    symbol: &ToolDocumentSymbol,
) -> CallHierarchyItem {
    CallHierarchyItem {
        name: symbol.name.clone(),
        kind: to_lsp_symbol_kind(symbol.kind),
        tags: None,
        detail: None,
        uri: uri.clone(),
        range: to_tool_range(&symbol.range),
        selection_range: to_tool_range(&symbol.selection_range),
        data: Some(json!({
            "uri": uri.as_str(),
            "line": symbol.selection_range.start_line.saturating_sub(1),
            "character": symbol.selection_range.start_col.saturating_sub(1),
        })),
    }
}

pub(super) fn resolve_lsp_workspace_symbol(mut symbol: WorkspaceSymbol) -> WorkspaceSymbol {
    let OneOf::Right(location) = &symbol.location else {
        return symbol;
    };
    let Some(range) = symbol
        .data
        .as_ref()
        .and_then(|data| data.get("range"))
        .cloned()
        .and_then(|range| serde_json::from_value::<Range>(range).ok())
    else {
        return symbol;
    };
    symbol.location = OneOf::Left(Location {
        uri: location.uri.clone(),
        range,
    });
    symbol
}

const fn to_completion_item_kind(kind: ToolCompletionKind) -> CompletionItemKind {
    match kind {
        ToolCompletionKind::Keyword => CompletionItemKind::KEYWORD,
        ToolCompletionKind::Function | ToolCompletionKind::Procedure => {
            CompletionItemKind::FUNCTION
        }
        ToolCompletionKind::Variable | ToolCompletionKind::Parameter => {
            CompletionItemKind::VARIABLE
        }
        ToolCompletionKind::TypeParameter => CompletionItemKind::TYPE_PARAMETER,
        ToolCompletionKind::Type => CompletionItemKind::CLASS,
        ToolCompletionKind::Module => CompletionItemKind::MODULE,
        ToolCompletionKind::Property => CompletionItemKind::PROPERTY,
        ToolCompletionKind::EnumMember => CompletionItemKind::ENUM_MEMBER,
    }
}

pub(super) const fn to_lsp_symbol_kind(kind: ToolSymbolKind) -> SymbolKind {
    match kind {
        ToolSymbolKind::Module => SymbolKind::MODULE,
        ToolSymbolKind::Function | ToolSymbolKind::Procedure => SymbolKind::FUNCTION,
        ToolSymbolKind::Variable | ToolSymbolKind::Parameter => SymbolKind::VARIABLE,
        ToolSymbolKind::TypeParameter => SymbolKind::TYPE_PARAMETER,
        ToolSymbolKind::Type => SymbolKind::STRUCT,
        ToolSymbolKind::Namespace => SymbolKind::NAMESPACE,
        ToolSymbolKind::Alias => SymbolKind::CONSTANT,
        ToolSymbolKind::Property => SymbolKind::PROPERTY,
        ToolSymbolKind::EnumMember => SymbolKind::ENUM_MEMBER,
    }
}

const fn to_folding_range_kind(kind: ToolFoldingRangeKind) -> FoldingRangeKind {
    match kind {
        ToolFoldingRangeKind::Comment => FoldingRangeKind::Comment,
        ToolFoldingRangeKind::Imports => FoldingRangeKind::Imports,
        ToolFoldingRangeKind::Region => FoldingRangeKind::Region,
    }
}

pub(super) fn semantic_tokens_legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::NAMESPACE,
            SemanticTokenType::TYPE,
            SemanticTokenType::TYPE_PARAMETER,
            SemanticTokenType::PARAMETER,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::PROPERTY,
            SemanticTokenType::ENUM_MEMBER,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::new("procedure"),
            SemanticTokenType::MACRO,
            SemanticTokenType::KEYWORD,
            SemanticTokenType::MODIFIER,
            SemanticTokenType::COMMENT,
            SemanticTokenType::STRING,
            SemanticTokenType::NUMBER,
            SemanticTokenType::OPERATOR,
            SemanticTokenType::DECORATOR,
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::DEFINITION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::STATIC,
            SemanticTokenModifier::DEPRECATED,
            SemanticTokenModifier::DOCUMENTATION,
            SemanticTokenModifier::DEFAULT_LIBRARY,
            SemanticTokenModifier::MODIFICATION,
            SemanticTokenModifier::new("module"),
        ],
    }
}

pub(super) fn encode_semantic_tokens(
    text: &str,
    tokens: &[ToolSemanticToken],
    range: Option<&Range>,
) -> Vec<SemanticToken> {
    let mut encoded = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;
    let lines = LineIndex::new(text);
    for token in tokens
        .iter()
        .filter(|token| token_intersects_range(&lines, token, range))
    {
        let lsp_range = lines.range(&token.range);
        let line = lsp_range.start.line;
        let start = lsp_range.start.character;
        let end = lsp_range.end.character;
        if end <= start {
            continue;
        }
        let delta_line = line.saturating_sub(prev_line);
        let delta_start = if delta_line == 0 {
            start.saturating_sub(prev_start)
        } else {
            start
        };
        encoded.push(SemanticToken {
            delta_line,
            delta_start,
            length: end.saturating_sub(start),
            token_type: semantic_token_kind_index(token.kind),
            token_modifiers_bitset: semantic_modifier_bitset(&token.modifiers),
        });
        prev_line = line;
        prev_start = start;
    }
    encoded
}

pub(super) fn position_in_range(text: &str, position: ToolPosition, range: Range) -> bool {
    let position = to_lsp_position_in_text(text, position.line, position.col);
    !position_lt(position, range.start) && position_lt(position, range.end)
}

pub(super) fn to_lsp_inlay_hint(text: &str, hint: ToolInlayHint) -> InlayHint {
    InlayHint {
        position: to_lsp_position_in_text(text, hint.position.line, hint.position.col),
        label: InlayHintLabel::String(hint.label),
        kind: Some(match hint.kind {
            ToolInlayHintKind::Type => InlayHintKind::TYPE,
            ToolInlayHintKind::Parameter => InlayHintKind::PARAMETER,
        }),
        text_edits: None,
        tooltip: None,
        padding_left: Some(matches!(hint.kind, ToolInlayHintKind::Type)),
        padding_right: Some(matches!(hint.kind, ToolInlayHintKind::Parameter)),
        data: Some(json!({
            "tooltip": hint.tooltip,
        })),
    }
}

pub(super) fn resolve_lsp_inlay_hint(mut hint: InlayHint) -> InlayHint {
    if hint.tooltip.is_some() {
        return hint;
    }
    let Some(Value::Object(data)) = hint.data.as_ref() else {
        return hint;
    };
    if let Some(tooltip) = data.get("tooltip").and_then(Value::as_str) {
        hint.tooltip = Some(InlayHintTooltip::String(tooltip.to_owned()));
    }
    hint
}

pub(super) fn full_document_range(text: &str) -> Range {
    let mut line = 0usize;
    let mut character = 0usize;
    for ch in text.chars() {
        if ch == '\n' {
            line = line.saturating_add(1);
            character = 0;
        } else {
            character = character.saturating_add(ch.len_utf16());
        }
    }
    Range {
        start: Position::new(0, 0),
        end: Position::new(usize_to_u32(line), usize_to_u32(character)),
    }
}

pub(super) fn diagnostic_matches_path(path: &Path, diagnostic: &CliDiagnostic) -> bool {
    let Some(file) = &diagnostic.file else {
        return false;
    };
    normalized_path(Path::new(file)) == normalized_path(path)
}

pub(super) fn tool_location_matches_path(path: &Path, location: &ToolLocation) -> bool {
    normalized_path(&location.path) == normalized_path(path)
}

pub(super) fn to_lsp_diagnostic(diagnostic: CliDiagnostic) -> Diagnostic {
    let data = diagnostic_data(&diagnostic);
    Diagnostic {
        range: diagnostic
            .range
            .as_ref()
            .map_or_else(default_range, to_cli_range),
        severity: Some(to_severity(diagnostic.severity)),
        code: diagnostic.code.map(NumberOrString::String),
        code_description: None,
        source: Some("musi".to_owned()),
        message: diagnostic.message,
        related_information: related_information(&diagnostic.labels),
        tags: None,
        data,
    }
}

fn diagnostic_data(diagnostic: &CliDiagnostic) -> Option<Value> {
    (diagnostic.hint.is_some() || !diagnostic.notes.is_empty()).then(|| {
        json!({
            "phase": diagnostic.phase,
            "hint": diagnostic.hint,
            "notes": diagnostic.notes,
        })
    })
}

pub(super) fn to_severity(value: &str) -> DiagnosticSeverity {
    match value {
        "warning" => DiagnosticSeverity::WARNING,
        "info" => DiagnosticSeverity::INFORMATION,
        "hint" => DiagnosticSeverity::HINT,
        _ => DiagnosticSeverity::ERROR,
    }
}

pub(super) fn to_cli_range(range: &CliDiagnosticRange) -> Range {
    Range {
        start: Position {
            line: usize_to_u32(range.start_line.saturating_sub(1)),
            character: usize_to_u32(range.start_col.saturating_sub(1)),
        },
        end: Position {
            line: usize_to_u32(range.end_line.saturating_sub(1)),
            character: usize_to_u32(range.end_col.saturating_sub(1)),
        },
    }
}

pub(super) fn to_tool_range(range: &ToolRange) -> Range {
    Range {
        start: Position {
            line: usize_to_u32(range.start_line.saturating_sub(1)),
            character: usize_to_u32(range.start_col.saturating_sub(1)),
        },
        end: Position {
            line: usize_to_u32(range.end_line.saturating_sub(1)),
            character: usize_to_u32(range.end_col.saturating_sub(1)),
        },
    }
}

pub(super) fn default_range() -> Range {
    Range {
        start: Position::new(0, 0),
        end: Position::new(0, 1),
    }
}

pub(super) fn truncate_hover_contents(contents: &str, maximum_length: usize) -> String {
    if contents.chars().count() <= maximum_length {
        return contents.to_owned();
    }
    let mut truncated = contents.chars().take(maximum_length).collect::<String>();
    truncated.push('…');
    truncated
}

fn normalized_path(path: &Path) -> PathBuf {
    path.components()
        .filter_map(|component| match component {
            Component::CurDir => None,
            Component::ParentDir => Some(OsStr::new("..").to_owned()),
            Component::Normal(part) => Some(part.to_owned()),
            Component::RootDir | Component::Prefix(_) => Some(component.as_os_str().to_owned()),
        })
        .collect()
}

fn related_information(labels: &[CliDiagnosticLabel]) -> Option<Vec<DiagnosticRelatedInformation>> {
    let items = labels
        .iter()
        .filter_map(|label| {
            let file = label.file.as_ref()?;
            let uri = Url::from_file_path(file).ok()?;
            let range = label
                .range
                .as_ref()
                .map_or_else(default_range, to_cli_range);
            Some(DiagnosticRelatedInformation {
                location: Location { uri, range },
                message: label.message.clone(),
            })
        })
        .collect::<Vec<_>>();
    (!items.is_empty()).then_some(items)
}

fn token_intersects_range(
    lines: &LineIndex<'_>,
    token: &ToolSemanticToken,
    range: Option<&Range>,
) -> bool {
    let Some(range) = range else {
        return true;
    };
    let token_range = lines.range(&token.range);
    position_lt(token_range.start, range.end) && position_lt(range.start, token_range.end)
}

const fn position_lt(left: Position, right: Position) -> bool {
    left.line < right.line || (left.line == right.line && left.character < right.character)
}

fn semantic_modifier_bitset(modifiers: &[ToolSemanticModifier]) -> u32 {
    modifiers.iter().fold(0, |bits, modifier| {
        bits | (1 << semantic_modifier_index(*modifier))
    })
}

const TOOL_TOKEN_KIND_LEGEND: [ToolSemanticTokenKind; 17] = [
    ToolSemanticTokenKind::Namespace,
    ToolSemanticTokenKind::Type,
    ToolSemanticTokenKind::TypeParameter,
    ToolSemanticTokenKind::Parameter,
    ToolSemanticTokenKind::Variable,
    ToolSemanticTokenKind::Property,
    ToolSemanticTokenKind::EnumMember,
    ToolSemanticTokenKind::Function,
    ToolSemanticTokenKind::Procedure,
    ToolSemanticTokenKind::Macro,
    ToolSemanticTokenKind::Keyword,
    ToolSemanticTokenKind::Modifier,
    ToolSemanticTokenKind::Comment,
    ToolSemanticTokenKind::String,
    ToolSemanticTokenKind::Number,
    ToolSemanticTokenKind::Operator,
    ToolSemanticTokenKind::Decorator,
];

fn semantic_token_kind_index(kind: ToolSemanticTokenKind) -> u32 {
    TOOL_TOKEN_KIND_LEGEND
        .iter()
        .position(|item| *item == kind)
        .and_then(|index| u32::try_from(index).ok())
        .unwrap_or(0)
}

const TOOL_TOKEN_MODIFIER_LEGEND: [ToolSemanticModifier; 9] = [
    ToolSemanticModifier::Declaration,
    ToolSemanticModifier::Definition,
    ToolSemanticModifier::Readonly,
    ToolSemanticModifier::Static,
    ToolSemanticModifier::Deprecated,
    ToolSemanticModifier::Documentation,
    ToolSemanticModifier::DefaultLibrary,
    ToolSemanticModifier::Modification,
    ToolSemanticModifier::Module,
];

fn semantic_modifier_index(modifier: ToolSemanticModifier) -> u32 {
    TOOL_TOKEN_MODIFIER_LEGEND
        .iter()
        .position(|item| *item == modifier)
        .and_then(|index| u32::try_from(index).ok())
        .unwrap_or(0)
}

fn usize_to_u32(value: usize) -> u32 {
    u32::try_from(value).unwrap_or(u32::MAX)
}
