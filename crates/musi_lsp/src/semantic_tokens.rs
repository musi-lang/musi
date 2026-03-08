//! Semantic token highlighting for Musi source files.
//!
//! When sema results are available, tokens are emitted from the resolver
//! side-tables (`pat_defs` for declarations, `expr_defs` for references).
//! When sema is unavailable (native-import file), the old lex-based state
//! machine is used as a fallback.

use musi_lex::TokenKind;
use musi_sema::{DefKind, Type};
use musi_shared::{FileId, SourceDb, Span};
use tower_lsp_server::ls_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensResult,
    SemanticTokensServerCapabilities,
};

use crate::analysis::{AnalyzedDoc, expr_span, find_name_token, offset_to_position};

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

// -- Raw token (absolute position before delta encoding) --------------------

struct RawToken {
    line: u32,
    start_char: u32,
    length: u32,
    token_type: u32,
    token_modifiers: u32,
}

// -- Public API -------------------------------------------------------------

/// Compute semantic tokens for a document, using sema results when available.
pub fn compute(doc: &AnalyzedDoc) -> SemanticTokensResult {
    let mut raw: Vec<RawToken> = Vec::new();

    if let Some(sema) = &doc.sema {
        // -- Sema-based emission --

        // 1. Declarations from pat_defs.
        //
        //    `pat_defs` spans can be multi-token (e.g. `a: Shape` for params,
        //    `Circle(Float)` for variants). We search the token stream for the
        //    actual Ident token whose symbol matches the definition name so that
        //    we only colour the name, not the surrounding type annotations.
        for (span, &def_id) in &sema.pat_defs {
            let Some(def) = sema.defs.get(def_id.0 as usize) else {
                continue;
            };
            let name_span =
                find_name_token(&doc.lexed.tokens, span.start, def.name).unwrap_or(Span {
                    start: span.start,
                    length: doc.interner.resolve(def.name).len() as u32,
                });
            let def_name = doc.interner.resolve(def.name);
            let (tt, tm) = classify_def(def.kind, &def.ty, sema, true, def_name, def.is_var);
            if let Some(tt) = tt {
                push_raw(&mut raw, name_span, tt, tm, doc.file_id, &doc.source_db);
            }
        }

        // 2. Named fn/type declarations — not stored in pat_defs so emitted via
        //    a lightweight lex scan.  The lex scanner gives single-token spans.
        {
            #[derive(Clone, Copy)]
            enum DeclState {
                Default,
                AfterFn,   // next Ident (skipping '['/']') = function name
                AfterType, // record | opaque | class | choice: next Ident = type name
            }
            let mut state = DeclState::Default;
            for tok in &doc.lexed.tokens {
                let kind = tok.kind;
                state = match state {
                    DeclState::Default => match kind {
                        TokenKind::Fn => DeclState::AfterFn,
                        TokenKind::Record
                        | TokenKind::Opaque
                        | TokenKind::Class
                        | TokenKind::Choice => DeclState::AfterType,
                        _ => DeclState::Default,
                    },
                    DeclState::AfterFn => match kind {
                        TokenKind::Ident => {
                            push_raw(
                                &mut raw,
                                tok.span,
                                TT_FUNCTION,
                                TM_DECLARATION,
                                doc.file_id,
                                &doc.source_db,
                            );
                            DeclState::Default
                        }
                        TokenKind::LBracket | TokenKind::RBracket => DeclState::AfterFn,
                        _ => DeclState::Default,
                    },
                    DeclState::AfterType => match kind {
                        TokenKind::Ident => {
                            push_raw(
                                &mut raw,
                                tok.span,
                                TT_TYPE,
                                TM_DECLARATION,
                                doc.file_id,
                                &doc.source_db,
                            );
                            DeclState::Default
                        }
                        _ => DeclState::Default,
                    },
                };
            }
        }

        // 2.5. Type annotation references from ty_refs.
        for (&span, &def_id) in &sema.ty_refs {
            let Some(def) = sema.defs.get(def_id.0 as usize) else {
                continue;
            };
            if matches!(def.kind, DefKind::Type | DefKind::Class) {
                let name_span =
                    find_name_token(&doc.lexed.tokens, span.start, def.name).unwrap_or(Span {
                        start: span.start,
                        length: doc.interner.resolve(def.name).len() as u32,
                    });
                push_raw(&mut raw, name_span, TT_TYPE, 0, doc.file_id, &doc.source_db);
            }
        }

        // 3. References: walk expr_defs (Idx<Expr> → DefId).
        for (&idx, &def_id) in &sema.expr_defs {
            let Some(def) = sema.defs.get(def_id.0 as usize) else {
                continue;
            };
            let span = expr_span(idx, &doc.module);
            if span.length == 0 {
                continue;
            }
            let def_name = doc.interner.resolve(def.name);
            let (tt, tm) = classify_def(def.kind, &def.ty, sema, false, def_name, def.is_var);
            if let Some(tt) = tt {
                push_raw(&mut raw, span, tt, tm, doc.file_id, &doc.source_db);
            }
        }

        // 4. Type parameters: lex-based scan for TyIdent tokens.
        for tok in &doc.lexed.tokens {
            if tok.kind == TokenKind::TyIdent {
                push_raw(
                    &mut raw,
                    tok.span,
                    TT_TYPE_PARAM,
                    0,
                    doc.file_id,
                    &doc.source_db,
                );
            }
        }

        // 5. Dot-prefix constructors: `.Name` patterns not preceded by a closer.
        let mut prev_kind: Option<TokenKind> = None;
        let mut after_dot = false;
        for tok in &doc.lexed.tokens {
            let kind = tok.kind;
            if after_dot && kind == TokenKind::Ident {
                push_raw(
                    &mut raw,
                    tok.span,
                    TT_ENUM_MEMBER,
                    0,
                    doc.file_id,
                    &doc.source_db,
                );
                after_dot = false;
            } else if kind == TokenKind::Dot {
                let is_field_access = matches!(
                    prev_kind,
                    Some(TokenKind::Ident | TokenKind::RParen | TokenKind::RBracket)
                );
                after_dot = !is_field_access;
            } else {
                after_dot = false;
            }
            prev_kind = Some(kind);
        }

        // 6. Sort by (line, col) and dedup before delta-encoding.
        raw.sort_unstable_by_key(|t| (t.line, t.start_char));
        raw.dedup_by_key(|t| (t.line, t.start_char));
    } else {
        // Fallback: lex-based state machine (no sema available).
        lex_fallback(doc, &mut raw);
    }

    SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: to_delta(raw),
    })
}

// -- Classification ---------------------------------------------------------

fn is_constant_name(name: &str) -> bool {
    !name.is_empty()
        && name.chars().next().is_some_and(|c| c.is_uppercase())
        && name.chars().all(|c| c.is_uppercase() || c.is_ascii_digit() || c == '_')
}

fn classify_def(
    kind: DefKind,
    ty: &Option<Type>,
    sema: &musi_sema::SemaResult,
    is_decl: bool,
    def_name: &str,
    is_var_param: bool,
) -> (Option<u32>, u32) {
    let decl = if is_decl { TM_DECLARATION } else { 0 };
    match kind {
        DefKind::Fn => (Some(TT_FUNCTION), decl),
        DefKind::Const => {
            let is_fn = ty
                .as_ref()
                .map(|t| matches!(sema.unify_table.resolve(t.clone()), Type::Arrow(..)))
                .unwrap_or(false);
            if is_fn {
                (Some(TT_FUNCTION), decl)
            } else if is_constant_name(def_name) {
                // UPPER_SNAKE_CASE → highlight as a constant (readonly)
                (Some(TT_VARIABLE), decl | TM_READONLY)
            } else {
                // regular `const x` → plain immutable variable (no READONLY modifier)
                (Some(TT_VARIABLE), decl)
            }
        }
        DefKind::Var => (Some(TT_VARIABLE), decl | TM_MUTABLE),
        DefKind::Param => {
            // Highlight function-typed params (e.g. `f: A -> B`) as functions.
            let is_fn = ty
                .as_ref()
                .map(|t: &Type| matches!(sema.unify_table.resolve(t.clone()), Type::Arrow(..)))
                .unwrap_or(false);
            if is_fn {
                let mods = if is_var_param { TM_MUTABLE } else { 0 };
                (Some(TT_FUNCTION), decl | mods)
            } else if is_var_param {
                (Some(TT_VARIABLE), decl | TM_MUTABLE)
            } else {
                // Immutable params — no READONLY modifier (they're not constants)
                (Some(TT_VARIABLE), decl)
            }
        }
        DefKind::Type => (Some(TT_TYPE), decl),
        DefKind::Variant => (Some(TT_ENUM_MEMBER), decl),
        DefKind::Namespace => (None, 0),
        DefKind::Class | DefKind::Given => (Some(TT_TYPE), decl),
    }
}

// -- Lex-based fallback -----------------------------------------------------

#[derive(Clone, Copy)]
enum ScanState {
    Default,
    AfterFn,
    AfterConst,
    AfterVar,
    AfterNamedTypeDef,
    AfterChoiceKw,
    WaitChoiceBrace,
    ExpectVariant(u32),
    SkipVariant(u32),
    AfterGiven,
}

fn lex_fallback(doc: &AnalyzedDoc, raw: &mut Vec<RawToken>) {
    let mut state = ScanState::Default;

    for tok in &doc.lexed.tokens {
        let kind = tok.kind;

        if kind == TokenKind::TyIdent {
            push_raw(raw, tok.span, TT_TYPE_PARAM, 0, doc.file_id, &doc.source_db);
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
                        raw,
                        tok.span,
                        TT_FUNCTION,
                        TM_DECLARATION,
                        doc.file_id,
                        &doc.source_db,
                    );
                    ScanState::Default
                }
                TokenKind::LBracket | TokenKind::RBracket => ScanState::AfterFn,
                _ => ScanState::Default,
            },

            ScanState::AfterConst => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_READONLY,
                        doc.file_id,
                        &doc.source_db,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterVar => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_MUTABLE,
                        doc.file_id,
                        &doc.source_db,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterNamedTypeDef => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_TYPE,
                        TM_DECLARATION,
                        doc.file_id,
                        &doc.source_db,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterChoiceKw => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_TYPE,
                        TM_DECLARATION,
                        doc.file_id,
                        &doc.source_db,
                    );
                    ScanState::WaitChoiceBrace
                }
                TokenKind::LBrace => ScanState::ExpectVariant(1),
                _ => ScanState::Default,
            },

            ScanState::WaitChoiceBrace => match kind {
                TokenKind::LBrace => ScanState::ExpectVariant(1),
                _ => ScanState::WaitChoiceBrace,
            },

            ScanState::ExpectVariant(depth) => match kind {
                TokenKind::Ident if depth == 1 => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_ENUM_MEMBER,
                        TM_DECLARATION,
                        doc.file_id,
                        &doc.source_db,
                    );
                    ScanState::SkipVariant(1)
                }
                TokenKind::LBrace => ScanState::ExpectVariant(depth + 1),
                TokenKind::RBrace if depth <= 1 => ScanState::Default,
                TokenKind::RBrace => ScanState::ExpectVariant(depth - 1),
                _ => ScanState::ExpectVariant(depth),
            },

            ScanState::SkipVariant(depth) => match kind {
                TokenKind::Pipe if depth == 1 => ScanState::ExpectVariant(1),
                TokenKind::LBrace => ScanState::SkipVariant(depth + 1),
                TokenKind::RBrace if depth <= 1 => ScanState::Default,
                TokenKind::RBrace => ScanState::SkipVariant(depth - 1),
                // Variant payload type names start with uppercase (e.g. `Circle(Float)`)
                TokenKind::Ident => {
                    let text = &doc.source[tok.span.start as usize
                        ..(tok.span.start + tok.span.length) as usize];
                    if text.starts_with(|c: char| c.is_uppercase()) {
                        push_raw(raw, tok.span, TT_TYPE, 0, doc.file_id, &doc.source_db);
                    }
                    ScanState::SkipVariant(depth)
                }
                _ => ScanState::SkipVariant(depth),
            },

            ScanState::AfterGiven => match kind {
                TokenKind::Ident => {
                    push_raw(raw, tok.span, TT_TYPE, 0, doc.file_id, &doc.source_db);
                    ScanState::Default
                }
                _ => ScanState::Default,
            },
        };
    }
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
    let pos = offset_to_position(file_id, span.start, source_db);
    out.push(RawToken {
        line: pos.line,
        start_char: pos.character,
        length: span.length,
        token_type,
        token_modifiers,
    });
}

/// Convert absolute-position tokens to LSP delta-encoded format.
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
