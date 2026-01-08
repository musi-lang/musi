use crate::handlers::AnalyzedDocument;
use lsp_types::{SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens};
use musi_core::SourceFile;
use musi_sema::SymbolKind;

pub const TOKEN_TYPES: &[SemanticTokenType] = &[
    SemanticTokenType::FUNCTION,
    SemanticTokenType::VARIABLE,
    SemanticTokenType::PARAMETER,
    SemanticTokenType::TYPE,
    SemanticTokenType::PROPERTY,
    SemanticTokenType::ENUM_MEMBER,
];

pub const TOKEN_MODIFIERS: &[SemanticTokenModifier] = &[
    SemanticTokenModifier::DECLARATION,
    SemanticTokenModifier::READONLY,
    SemanticTokenModifier::DEFAULT_LIBRARY,
    SemanticTokenModifier::DEPRECATED,
];

const MOD_DECLARATION: u32 = 1 << 0;
const MOD_READONLY: u32 = 1 << 1;
const MOD_DEFAULT_LIBRARY: u32 = 1 << 2;
const MOD_DEPRECATED: u32 = 1 << 3;

#[must_use]
pub fn get_semantic_tokens(doc: &AnalyzedDocument, source: &SourceFile) -> SemanticTokens {
    let mut tokens = Vec::new();
    collect_definition_tokens(doc, source, &mut tokens);
    collect_reference_tokens(doc, source, &mut tokens);
    tokens.sort_by_key(|t| (t.delta_line, t.delta_start));
    convert_to_deltas(&mut tokens);
    SemanticTokens {
        result_id: None,
        data: tokens,
    }
}

fn collect_definition_tokens(
    doc: &AnalyzedDocument,
    source: &SourceFile,
    tokens: &mut Vec<SemanticToken>,
) {
    for sym in doc.symbols.iter_defs() {
        let span = sym.def_span;
        let (line, col) = source.location_at(span.lo);
        let line = u32::try_from(line.saturating_sub(1)).unwrap_or(0);
        let col = u32::try_from(col.saturating_sub(1)).unwrap_or(0);
        let length = span.hi.saturating_sub(span.lo);
        let token_type = kind_to_type_index(sym.kind);
        let mut modifiers = MOD_DECLARATION;
        if sym.kind == SymbolKind::Builtin {
            modifiers |= MOD_DEFAULT_LIBRARY;
        }
        if doc.symbols.is_unused(sym.name) {
            modifiers |= MOD_DEPRECATED;
        }
        tokens.push(SemanticToken {
            delta_line: line,
            delta_start: col,
            length,
            token_type,
            token_modifiers_bitset: modifiers,
        });
    }
}

fn collect_reference_tokens(
    doc: &AnalyzedDocument,
    source: &SourceFile,
    tokens: &mut Vec<SemanticToken>,
) {
    for (span, name) in doc.symbols.iter_refs() {
        let Some(sym) = doc.symbols.get(*name) else {
            continue;
        };
        let (line, col) = source.location_at(span.lo);
        let line = u32::try_from(line.saturating_sub(1)).unwrap_or(0);
        let col = u32::try_from(col.saturating_sub(1)).unwrap_or(0);
        let length = span.hi.saturating_sub(span.lo);
        let token_type = kind_to_type_index(sym.kind);
        let mut modifiers = 0u32;
        if sym.kind == SymbolKind::Builtin {
            modifiers |= MOD_DEFAULT_LIBRARY;
        }
        if sym.kind == SymbolKind::Local {
            modifiers |= MOD_READONLY;
        }
        tokens.push(SemanticToken {
            delta_line: line,
            delta_start: col,
            length,
            token_type,
            token_modifiers_bitset: modifiers,
        });
    }
}

const fn kind_to_type_index(kind: SymbolKind) -> u32 {
    match kind {
        SymbolKind::Fn | SymbolKind::Builtin => 0,
        SymbolKind::Local => 1,
        SymbolKind::Param => 2,
        SymbolKind::Type => 3,
        SymbolKind::Field => 4,
        SymbolKind::Variant => 5,
    }
}

fn convert_to_deltas(tokens: &mut [SemanticToken]) {
    let mut prev_line = 0u32;
    let mut prev_start = 0u32;
    for token in tokens.iter_mut() {
        let line = token.delta_line;
        let start = token.delta_start;
        token.delta_line = line.saturating_sub(prev_line);
        token.delta_start = if line == prev_line {
            start.saturating_sub(prev_start)
        } else {
            start
        };
        prev_line = line;
        prev_start = start;
    }
}
