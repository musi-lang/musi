use crate::handlers::AnalyzedDocument;
use lsp_types::{SemanticToken, SemanticTokens};
use musi_basic::source::SourceFile;
use musi_sema::SemanticTokenKind;

pub const TOKEN_TYPES: &[lsp_types::SemanticTokenType] = &[
    lsp_types::SemanticTokenType::VARIABLE,
    lsp_types::SemanticTokenType::PARAMETER,
    lsp_types::SemanticTokenType::FUNCTION,
    lsp_types::SemanticTokenType::TYPE,
    lsp_types::SemanticTokenType::PROPERTY,
    lsp_types::SemanticTokenType::ENUM_MEMBER,
];

pub const TOKEN_MODIFIERS: &[lsp_types::SemanticTokenModifier] = &[
    lsp_types::SemanticTokenModifier::DECLARATION,
    lsp_types::SemanticTokenModifier::READONLY,
    lsp_types::SemanticTokenModifier::DEFAULT_LIBRARY,
];

pub fn get_semantic_tokens(doc: &AnalyzedDocument, source: &SourceFile) -> SemanticTokens {
    let raw_tokens =
        musi_sema::collect_tokens(&doc.arena, &doc.prog, &doc.sema_model, &doc.symbols);

    let mut last_line = 0;
    let mut last_start = 0;
    let mut data = vec![];

    for raw in raw_tokens {
        let (line, col) = source.location_at(raw.span.lo);
        let line = (line).saturating_sub(1);
        let col = (col).saturating_sub(1);

        let delta_line = line - last_line;
        let delta_start = if delta_line == 0 {
            col - last_start
        } else {
            col
        };

        let token_type = match raw.kind {
            SemanticTokenKind::Variable => 0,
            SemanticTokenKind::Parameter => 1,
            SemanticTokenKind::Function => 2,
            SemanticTokenKind::Type => 3,
            SemanticTokenKind::Property => 4,
            SemanticTokenKind::EnumMember => 5,
        };

        let modifiers = raw.modifiers;

        data.push(SemanticToken {
            delta_line: delta_line.try_into().expect("delta_line too large"),
            delta_start: delta_start.try_into().expect("delta_start too large"),
            length: (raw.span.hi - raw.span.lo),
            token_type,
            token_modifiers_bitset: modifiers,
        });

        last_line = line;
        last_start = col;
    }

    SemanticTokens {
        result_id: None,
        data,
    }
}
