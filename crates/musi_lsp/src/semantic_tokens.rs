//! Semantic token highlighting for Musi source files.
//!
//! Walks the token stream with a lightweight state machine to identify
//! declarations without requiring full scope analysis. Semantic tokens
//! override TextMate grammar highlighting, giving precise per-token control.

use musi_lex::{lex, TokenKind};
use musi_shared::{DiagnosticBag, FileId, Interner, Span, SourceDb};
use tower_lsp_server::ls_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions,
    SemanticTokensResult, SemanticTokensServerCapabilities,
};

// -- Legend indices (must stay in sync with `legend()` below) ---------------

pub const TT_TYPE: u32 = 0;
pub const TT_TYPE_PARAM: u32 = 1;
pub const TT_ENUM_MEMBER: u32 = 2;
pub const TT_FUNCTION: u32 = 3;
pub const TT_VARIABLE: u32 = 4;

pub const TM_DECLARATION: u32 = 1 << 0;
pub const TM_READONLY: u32 = 1 << 1;
pub const TM_MUTABLE: u32 = 1 << 2;

/// Build the semantic tokens legend.
///
/// Token type indices correspond to `TT_*` constants above.
/// Token modifier bit positions correspond to `TM_*` constants above.
pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::TYPE,           // 0 = TT_TYPE
            SemanticTokenType::TYPE_PARAMETER, // 1 = TT_TYPE_PARAM
            SemanticTokenType::ENUM_MEMBER,    // 2 = TT_ENUM_MEMBER
            SemanticTokenType::FUNCTION,       // 3 = TT_FUNCTION
            SemanticTokenType::VARIABLE,       // 4 = TT_VARIABLE
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,    // bit 0 = TM_DECLARATION
            SemanticTokenModifier::READONLY,       // bit 1 = TM_READONLY
            SemanticTokenModifier::new("mutable"), // bit 2 = TM_MUTABLE (custom)
        ],
    }
}

/// Build the `semanticTokensProvider` capability entry for `initialize`.
pub fn provider() -> SemanticTokensServerCapabilities {
    SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
        legend: legend(),
        full: Some(SemanticTokensFullOptions::Bool(true)),
        ..SemanticTokensOptions::default()
    })
}

// -- State machine ----------------------------------------------------------

#[derive(Clone, Copy)]
enum ScanState {
    Default,
    AfterFn,           // next Ident = function declaration
    AfterConst,        // next Ident = variable + readonly + declaration
    AfterVar,          // next Ident = variable + mutable + declaration
    AfterNamedTypeDef, // record | opaque | class: next Ident = type + declaration
    AfterChoiceKw,     // choice: next Ident = type + declaration, then WaitBrace
    WaitChoiceBrace,   // after choice name; looking for `{`
    ExpectVariant(u32), // inside choice body at brace depth; next Ident = enumMember
    SkipVariant(u32),  // variant name seen; skip payload until `|` or `}`
    AfterGiven,        // next Ident = type reference (class being instantiated)
}

// -- Raw token (absolute position before delta encoding) --------------------

struct RawToken {
    line: u32,
    start_char: u32,
    length: u32,
    token_type: u32,
    token_modifiers: u32,
}

// -- Public API -------------------------------------------------------------

/// Lex `source` and produce semantic tokens for VS Code.
pub fn compute(source: &str) -> SemanticTokensResult {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();
    let file_id = source_db.add("<document>", source);
    let lexed = lex(source, file_id, &mut interner, &mut diags);

    let mut raw: Vec<RawToken> = Vec::new();
    let mut state = ScanState::Default;

    for tok in &lexed.tokens {
        let kind = tok.kind;

        // Type identifiers (e.g. `'T`) are always typeParameter tokens,
        // regardless of surrounding context — they can never be confused with
        // enum members or variables at the TextMate level.
        if kind == TokenKind::TyIdent {
            push_raw(&mut raw, tok.span, TT_TYPE_PARAM, 0, file_id, &source_db);
            // Don't reset state — a `'T` in `fn['T] name` shouldn't prevent
            // `name` from being classified as a function declaration.
            continue;
        }

        state = match state {
            ScanState::Default => match kind {
                TokenKind::Fn => ScanState::AfterFn,
                TokenKind::Const => ScanState::AfterConst,
                TokenKind::Var => ScanState::AfterVar,
                TokenKind::Record | TokenKind::Opaque | TokenKind::Class => {
                    ScanState::AfterNamedTypeDef
                }
                TokenKind::Choice => ScanState::AfterChoiceKw,
                TokenKind::Given => ScanState::AfterGiven,
                _ => ScanState::Default,
            },

            ScanState::AfterFn => match kind {
                TokenKind::Ident => {
                    push_raw(
                        &mut raw,
                        tok.span,
                        TT_FUNCTION,
                        TM_DECLARATION,
                        file_id,
                        &source_db,
                    );
                    ScanState::Default
                }
                // Skip `[` / `]` wrapping type parameters before the function name.
                TokenKind::LBracket | TokenKind::RBracket => ScanState::AfterFn,
                _ => ScanState::Default,
            },

            ScanState::AfterConst => match kind {
                TokenKind::Ident => {
                    push_raw(
                        &mut raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_READONLY,
                        file_id,
                        &source_db,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterVar => match kind {
                TokenKind::Ident => {
                    push_raw(
                        &mut raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_MUTABLE,
                        file_id,
                        &source_db,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterNamedTypeDef => match kind {
                TokenKind::Ident => {
                    push_raw(
                        &mut raw,
                        tok.span,
                        TT_TYPE,
                        TM_DECLARATION,
                        file_id,
                        &source_db,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterChoiceKw => match kind {
                TokenKind::Ident => {
                    push_raw(
                        &mut raw,
                        tok.span,
                        TT_TYPE,
                        TM_DECLARATION,
                        file_id,
                        &source_db,
                    );
                    ScanState::WaitChoiceBrace
                }
                // Anonymous inline choice: `choice { Foo | Bar }`
                TokenKind::LBrace => ScanState::ExpectVariant(1),
                _ => ScanState::Default,
            },

            ScanState::WaitChoiceBrace => match kind {
                TokenKind::LBrace => ScanState::ExpectVariant(1),
                _ => ScanState::WaitChoiceBrace,
            },

            // Inside a choice body. Brace depth 1 = top level of the body.
            // Only emit enumMember for Idents at depth 1 — deeper Idents are
            // part of payload type expressions (e.g. record field names inside
            // `{ field: Type }` variant payloads) and must not be misclassified.
            ScanState::ExpectVariant(depth) => match kind {
                TokenKind::Ident if depth == 1 => {
                    push_raw(
                        &mut raw,
                        tok.span,
                        TT_ENUM_MEMBER,
                        TM_DECLARATION,
                        file_id,
                        &source_db,
                    );
                    ScanState::SkipVariant(1)
                }
                TokenKind::LBrace => ScanState::ExpectVariant(depth + 1),
                TokenKind::RBrace if depth <= 1 => ScanState::Default,
                TokenKind::RBrace => ScanState::ExpectVariant(depth - 1),
                _ => ScanState::ExpectVariant(depth),
            },

            // Skip variant payload tokens until the next `|` (new variant at
            // depth 1) or `}` (end of choice body), tracking nested `{}`.
            ScanState::SkipVariant(depth) => match kind {
                TokenKind::Pipe if depth == 1 => ScanState::ExpectVariant(1),
                TokenKind::LBrace => ScanState::SkipVariant(depth + 1),
                TokenKind::RBrace if depth <= 1 => ScanState::Default,
                TokenKind::RBrace => ScanState::SkipVariant(depth - 1),
                _ => ScanState::SkipVariant(depth),
            },

            ScanState::AfterGiven => match kind {
                TokenKind::Ident => {
                    // The class name after `given` is a type reference, not a
                    // new declaration — emit without TM_DECLARATION.
                    push_raw(&mut raw, tok.span, TT_TYPE, 0, file_id, &source_db);
                    ScanState::Default
                }
                _ => ScanState::Default,
            },
        };
    }

    SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: to_delta(raw),
    })
}

// -- Helpers ----------------------------------------------------------------

fn push_raw(
    out: &mut Vec<RawToken>,
    span: Span,
    token_type: u32,
    token_modifiers: u32,
    file_id: FileId,
    source_db: &SourceDb,
) {
    let src_len = u32::try_from(source_db.source(file_id).len()).unwrap_or(u32::MAX);
    let clamped = span.start.min(src_len);
    let (line1, col1) = source_db.lookup(file_id, clamped);
    out.push(RawToken {
        line: line1 - 1,
        start_char: col1 - 1,
        length: span.length,
        token_type,
        token_modifiers,
    });
}

/// Convert absolute-position tokens to LSP delta-encoded format.
/// Preserves lexer order (left-to-right), which guarantees monotone (line, col).
fn to_delta(raw: Vec<RawToken>) -> Vec<SemanticToken> {
    let mut result = Vec::with_capacity(raw.len());
    let mut prev_line: u32 = 0;
    let mut prev_start: u32 = 0;

    for tok in raw {
        let delta_line = tok.line - prev_line;
        let delta_start = if delta_line == 0 {
            tok.start_char - prev_start
        } else {
            tok.start_char
        };
        prev_line = tok.line;
        prev_start = tok.start_char;

        result.push(SemanticToken {
            delta_line,
            delta_start,
            length: tok.length,
            token_type: tok.token_type,
            token_modifiers_bitset: tok.token_modifiers,
        });
    }
    result
}
