use std::ffi::OsStr;
use std::path::{Component, Path, PathBuf};

use async_lsp::lsp_types::{
    CompletionItem, CompletionItemKind, CompletionTextEdit, Diagnostic,
    DiagnosticRelatedInformation, DiagnosticSeverity, Documentation, InlayHint, InlayHintKind,
    InlayHintLabel, InlayHintTooltip, Location, NumberOrString, Position, Range, SemanticToken,
    SemanticTokenModifier, SemanticTokenType, SemanticTokensLegend, TextEdit, Url,
};
use musi_tooling::{
    CliDiagnostic, CliDiagnosticLabel, CliDiagnosticRange, ToolCompletion, ToolCompletionKind,
    ToolInlayHint, ToolInlayHintKind, ToolPosition, ToolRange, ToolSemanticModifier,
    ToolSemanticToken, ToolSemanticTokenKind,
};

pub(super) fn to_lsp_completion(completion: ToolCompletion) -> CompletionItem {
    let text = completion
        .insert_text
        .clone()
        .unwrap_or_else(|| completion.label.clone());
    CompletionItem {
        label: completion.label,
        kind: Some(to_completion_item_kind(completion.kind)),
        detail: completion.detail,
        documentation: completion.documentation.map(Documentation::String),
        sort_text: completion.sort_text,
        filter_text: completion.filter_text,
        text_edit: Some(CompletionTextEdit::Edit(TextEdit::new(
            to_tool_range(&completion.replace_range),
            text,
        ))),
        ..CompletionItem::default()
    }
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
    tokens: &[ToolSemanticToken],
    range: Option<&Range>,
) -> Vec<SemanticToken> {
    let mut encoded = Vec::new();
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;
    for token in tokens
        .iter()
        .filter(|token| token_intersects_range(token, range))
    {
        let line = usize_to_u32(token.range.start_line.saturating_sub(1));
        let start = usize_to_u32(token.range.start_col.saturating_sub(1));
        let end = usize_to_u32(token.range.end_col.saturating_sub(1));
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

pub(super) fn position_in_range(position: ToolPosition, range: Range) -> bool {
    let position = to_tool_position(position);
    !position_lt(position, range.start) && position_lt(position, range.end)
}

pub(super) fn to_lsp_inlay_hint(hint: ToolInlayHint) -> InlayHint {
    InlayHint {
        position: to_tool_position(hint.position),
        label: InlayHintLabel::String(hint.label),
        kind: Some(match hint.kind {
            ToolInlayHintKind::Type => InlayHintKind::TYPE,
            ToolInlayHintKind::Parameter => InlayHintKind::PARAMETER,
        }),
        text_edits: None,
        tooltip: hint.tooltip.map(InlayHintTooltip::String),
        padding_left: Some(matches!(hint.kind, ToolInlayHintKind::Type)),
        padding_right: Some(matches!(hint.kind, ToolInlayHintKind::Parameter)),
        data: None,
    }
}

pub(super) fn full_document_range(text: &str) -> Range {
    let mut line = 0usize;
    let mut character = 0usize;
    for ch in text.chars() {
        if ch == '\n' {
            line = line.saturating_add(1);
            character = 0;
        } else {
            character = character.saturating_add(1);
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

pub(super) fn to_lsp_diagnostic(diagnostic: CliDiagnostic) -> Diagnostic {
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
        data: None,
    }
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

fn token_intersects_range(token: &ToolSemanticToken, range: Option<&Range>) -> bool {
    let Some(range) = range else {
        return true;
    };
    let start = Position::new(
        usize_to_u32(token.range.start_line.saturating_sub(1)),
        usize_to_u32(token.range.start_col.saturating_sub(1)),
    );
    let end = Position::new(
        usize_to_u32(token.range.end_line.saturating_sub(1)),
        usize_to_u32(token.range.end_col.saturating_sub(1)),
    );
    position_lt(start, range.end) && position_lt(range.start, end)
}

const fn position_lt(left: Position, right: Position) -> bool {
    left.line < right.line || (left.line == right.line && left.character < right.character)
}

fn to_tool_position(position: ToolPosition) -> Position {
    Position {
        line: usize_to_u32(position.line.saturating_sub(1)),
        character: usize_to_u32(position.col.saturating_sub(1)),
    }
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
