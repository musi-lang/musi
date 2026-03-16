//! Semantic token highlighting for Musi source files.

use music_ast::expr::ParamMode;
use music_lex::TokenKind;
use music_sema::{DefKind, SemaResult, Type, TypeIdx};
use music_shared::{FileId, SourceDb, Span};
use lsp_types::{
    SemanticToken, SemanticTokenModifier, SemanticTokenType, SemanticTokens,
    SemanticTokensFullOptions, SemanticTokensLegend, SemanticTokensOptions, SemanticTokensResult,
    SemanticTokensServerCapabilities,
};

use crate::analysis::{AnalyzedDoc, expr_span, find_name_token, offset_to_position};

pub const TT_TYPE: u32 = 0;
pub const TT_TYPE_PARAM: u32 = 1;
pub const TT_ENUM_MEMBER: u32 = 2;
pub const TT_FUNCTION: u32 = 3;
pub const TT_VARIABLE: u32 = 4;
pub const TT_PARAMETER: u32 = 5;
pub const TT_OPERATOR: u32 = 6;

pub const TM_DECLARATION: u32 = 1 << 0;
pub const TM_READONLY: u32 = 1 << 1;
pub const TM_MUTABLE: u32 = 1 << 2;

pub fn legend() -> SemanticTokensLegend {
    SemanticTokensLegend {
        token_types: vec![
            SemanticTokenType::TYPE,
            SemanticTokenType::TYPE_PARAMETER,
            SemanticTokenType::ENUM_MEMBER,
            SemanticTokenType::FUNCTION,
            SemanticTokenType::VARIABLE,
            SemanticTokenType::PARAMETER,
            SemanticTokenType::new("operator"),
        ],
        token_modifiers: vec![
            SemanticTokenModifier::DECLARATION,
            SemanticTokenModifier::READONLY,
            SemanticTokenModifier::new("mutable"),
        ],
    }
}

pub fn provider() -> SemanticTokensServerCapabilities {
    SemanticTokensServerCapabilities::SemanticTokensOptions(SemanticTokensOptions {
        legend: legend(),
        full: Some(SemanticTokensFullOptions::Bool(true)),
        ..SemanticTokensOptions::default()
    })
}

struct RawToken {
    line: u32,
    start_char: u32,
    length: u32,
    token_type: u32,
    token_modifiers: u32,
    priority: u8,
}

/// Compute semantic tokens for a document.
pub fn compute(doc: &AnalyzedDoc) -> SemanticTokensResult {
    let mut raw: Vec<RawToken> = Vec::new();

    if let Some(sema) = &doc.sema {
        // 1. Declarations from pat_defs.
        for (span, &def_id) in &sema.resolution.pat_defs {
            let Some(def) = sema.defs.get(def_id.0 as usize) else {
                continue;
            };
            let def_name = match doc.interner.try_resolve(def.name) {
                Some(n) => n,
                None => continue,
            };
            let name_span =
                find_name_token(&doc.lexed.tokens, span.start, def.name).unwrap_or(Span {
                    start: span.start,
                    length: u32::try_from(def_name.len()).unwrap_or(0),
                });
            let (tt, tm, prio) = classify_def(def, sema, &doc.interner, true);
            if let Some(tt) = tt {
                push_raw(
                    &mut raw,
                    name_span,
                    tt,
                    tm,
                    doc.file_id,
                    &doc.source_db,
                    prio,
                );
            }
        }

        // 2. Named let/type declarations via lex scan.
        emit_decl_names(doc, &mut raw);

        // 3. References from expr_defs.
        for (&idx, &def_id) in &sema.resolution.expr_defs {
            let Some(def) = sema.defs.get(def_id.0 as usize) else {
                continue;
            };
            let Some(span) = expr_span(idx, &doc.module) else {
                continue;
            };
            if span.length == 0 {
                continue;
            }
            let (tt, tm, prio) = classify_def(def, sema, &doc.interner, false);
            if let Some(tt) = tt {
                push_raw(&mut raw, span, tt, tm, doc.file_id, &doc.source_db, prio);
            }
        }

        // 4. Dot-prefix constructors.
        emit_dot_constructors(doc, &mut raw);

        // 5. TyIdent tokens as type parameters.
        emit_ty_ident_tokens(doc, &mut raw);

        raw.sort_by_key(|t| (t.line, t.start_char, t.priority));
        raw.dedup_by_key(|t| (t.line, t.start_char));
    } else {
        lex_fallback(doc, &mut raw);
    }

    SemanticTokensResult::Tokens(SemanticTokens {
        result_id: None,
        data: to_delta(raw),
    })
}

fn emit_ty_ident_tokens(doc: &AnalyzedDoc, raw: &mut Vec<RawToken>) {
    for tok in &doc.lexed.tokens {
        if tok.kind == TokenKind::TyIdent {
            push_raw(
                raw,
                tok.span,
                TT_TYPE_PARAM,
                0,
                doc.file_id,
                &doc.source_db,
                1,
            );
        }
    }
}

fn emit_decl_names(doc: &AnalyzedDoc, raw: &mut Vec<RawToken>) {
    #[derive(Clone, Copy)]
    enum DeclState {
        Default,
        AfterLet,
        AfterMut,
        AfterTypeDef,
        AfterLetLParen,
        AfterLetOp,
        AfterLaw,
    }
    let mut state = DeclState::Default;
    let mut op_span: Option<Span> = None;
    for tok in &doc.lexed.tokens {
        let kind = tok.kind;
        state = match state {
            DeclState::Default => match kind {
                TokenKind::KwLet => DeclState::AfterLet,
                TokenKind::KwClass
                | TokenKind::KwEffect
                | TokenKind::KwInstance
                | TokenKind::KwRecord => DeclState::AfterTypeDef,
                TokenKind::KwExport | TokenKind::KwForeign => DeclState::Default,
                TokenKind::KwLaw => DeclState::AfterLaw,
                _ => DeclState::Default,
            },
            DeclState::AfterLet => match kind {
                TokenKind::KwMut => DeclState::AfterMut,
                TokenKind::Ident => {
                    let text = doc
                        .source
                        .get(tok.span.start as usize..(tok.span.start + tok.span.length) as usize);
                    let modifier = if text.is_some_and(is_constant_name) {
                        TM_DECLARATION | TM_READONLY
                    } else {
                        TM_DECLARATION
                    };
                    push_raw(
                        raw,
                        tok.span,
                        TT_VARIABLE,
                        modifier,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                    DeclState::Default
                }
                TokenKind::LParen => DeclState::AfterLetLParen,
                _ => DeclState::Default,
            },
            DeclState::AfterMut => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_MUTABLE,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                    DeclState::Default
                }
                _ => DeclState::Default,
            },
            DeclState::AfterLetLParen => {
                if is_operator_token(kind) {
                    op_span = Some(tok.span);
                    DeclState::AfterLetOp
                } else {
                    DeclState::Default
                }
            }
            DeclState::AfterLetOp => {
                if kind == TokenKind::RParen
                    && let Some(sp) = op_span
                {
                    push_raw(
                        raw,
                        sp,
                        TT_OPERATOR,
                        TM_DECLARATION,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                }
                op_span = None;
                DeclState::Default
            }
            DeclState::AfterTypeDef => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_TYPE,
                        TM_DECLARATION,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                    DeclState::Default
                }
                _ => DeclState::Default,
            },
            DeclState::AfterLaw => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_READONLY,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                    DeclState::Default
                }
                _ => DeclState::Default,
            },
        };
    }
}

fn is_operator_token(kind: TokenKind) -> bool {
    matches!(
        kind,
        TokenKind::Plus
            | TokenKind::Minus
            | TokenKind::Star
            | TokenKind::Slash
            | TokenKind::Percent
            | TokenKind::Eq
            | TokenKind::SlashEq
            | TokenKind::Lt
            | TokenKind::Gt
            | TokenKind::LtEq
            | TokenKind::GtEq
            | TokenKind::ColonColon
            | TokenKind::PipeGt
            | TokenKind::DashGt
            | TokenKind::TildeGt
            | TokenKind::LtDash
            | TokenKind::DotDot
            | TokenKind::DotDotLt
            | TokenKind::QuestionQuestion
            | TokenKind::KwAnd
            | TokenKind::KwOr
            | TokenKind::KwXor
            | TokenKind::KwNot
            | TokenKind::BangBang
            | TokenKind::BangDot
            | TokenKind::Bang
            | TokenKind::LtLt
            | TokenKind::GtGt
            | TokenKind::ColonQuestion
            | TokenKind::ColonQuestionGt
            | TokenKind::QuestionDot
            | TokenKind::DotDotDot
            | TokenKind::EqGt
            | TokenKind::Pipe
    )
}

fn emit_dot_constructors(doc: &AnalyzedDoc, raw: &mut Vec<RawToken>) {
    let mut prev_kind: Option<TokenKind> = None;
    let mut after_dot = false;
    for tok in &doc.lexed.tokens {
        let kind = tok.kind;
        if after_dot && kind == TokenKind::Ident {
            push_raw(
                raw,
                tok.span,
                TT_ENUM_MEMBER,
                0,
                doc.file_id,
                &doc.source_db,
                1,
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
}

fn is_constant_name(name: &str) -> bool {
    !name.is_empty()
        && name.chars().next().is_some_and(|c| c.is_uppercase())
        && name
            .chars()
            .all(|c| c.is_uppercase() || c.is_ascii_digit() || c == '_')
}

fn starts_with_uppercase(name: &str) -> bool {
    name.chars().next().is_some_and(|c| c.is_uppercase())
}

fn is_fn_type(ty: TypeIdx, sema: &SemaResult) -> bool {
    let resolved = sema.unify.resolve(ty, &sema.types);
    matches!(&sema.types[resolved], Type::Fn { .. })
}

fn classify_def(
    def: &music_sema::DefInfo,
    sema: &music_sema::SemaResult,
    interner: &music_shared::Interner,
    is_decl: bool,
) -> (Option<u32>, u32, u8) {
    let decl = if is_decl { TM_DECLARATION } else { 0 };
    let def_name = interner.try_resolve(def.name).unwrap_or("");
    match def.kind {
        DefKind::Fn | DefKind::ForeignFn | DefKind::EffectOp => (Some(TT_FUNCTION), decl, 0),
        DefKind::Let => {
            let is_fn = def.ty_info.ty.is_some_and(|t| is_fn_type(t, sema));
            let has_ty_params = !def.ty_info.ty_params.is_empty();
            if is_fn {
                (Some(TT_FUNCTION), decl, 0)
            } else if has_ty_params || starts_with_uppercase(def_name) {
                (Some(TT_TYPE), decl, 0)
            } else if is_constant_name(def_name) {
                (Some(TT_VARIABLE), decl | TM_READONLY, 0)
            } else {
                (Some(TT_VARIABLE), decl, 0)
            }
        }
        DefKind::Var => {
            let is_fn = def.ty_info.ty.is_some_and(|t| is_fn_type(t, sema));
            if is_fn {
                (Some(TT_FUNCTION), decl | TM_MUTABLE, 0)
            } else {
                (Some(TT_VARIABLE), decl | TM_MUTABLE, 0)
            }
        }
        DefKind::Param => {
            let is_fn = def.ty_info.ty.is_some_and(|t| is_fn_type(t, sema));
            let is_mutable = def
                .param_mode
                .is_some_and(|m| matches!(m, ParamMode::Mut));
            let mut_mod = if is_mutable { TM_MUTABLE } else { 0 };
            if is_fn {
                (Some(TT_FUNCTION), decl | mut_mod, 0)
            } else {
                (Some(TT_PARAMETER), decl | mut_mod, 0)
            }
        }
        DefKind::Type | DefKind::OpaqueType => (Some(TT_TYPE), decl, 0),
        DefKind::Variant => (Some(TT_ENUM_MEMBER), decl, 0),
        DefKind::Import => (None, 0, 0),
        DefKind::Class | DefKind::Given | DefKind::Effect => (Some(TT_TYPE), decl, 0),
        DefKind::Law => (Some(TT_VARIABLE), decl | TM_READONLY, 0),
        DefKind::LawVar => (Some(TT_VARIABLE), decl, 0),
    }
}

fn lex_fallback(doc: &AnalyzedDoc, raw: &mut Vec<RawToken>) {
    #[derive(Clone, Copy)]
    enum ScanState {
        Default,
        AfterLet,
        AfterMut,
        AfterTypeDef,
        AfterInstance,
        AfterLaw,
    }

    let mut state = ScanState::Default;

    for tok in &doc.lexed.tokens {
        let kind = tok.kind;

        if kind == TokenKind::TyIdent {
            push_raw(
                raw,
                tok.span,
                TT_TYPE_PARAM,
                0,
                doc.file_id,
                &doc.source_db,
                1,
            );
        }

        state = match state {
            ScanState::Default => match kind {
                TokenKind::KwLet => ScanState::AfterLet,
                TokenKind::KwClass | TokenKind::KwEffect | TokenKind::KwRecord => {
                    ScanState::AfterTypeDef
                }
                TokenKind::KwInstance => ScanState::AfterInstance,
                TokenKind::KwExport | TokenKind::KwForeign => ScanState::Default,
                TokenKind::KwLaw => ScanState::AfterLaw,
                _ => ScanState::Default,
            },

            ScanState::AfterLet => match kind {
                TokenKind::KwMut => ScanState::AfterMut,
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_READONLY,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterMut => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_MUTABLE,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterTypeDef => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_TYPE,
                        TM_DECLARATION,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterInstance => match kind {
                TokenKind::Ident => {
                    push_raw(raw, tok.span, TT_TYPE, 0, doc.file_id, &doc.source_db, 1);
                    ScanState::Default
                }
                _ => ScanState::Default,
            },

            ScanState::AfterLaw => match kind {
                TokenKind::Ident => {
                    push_raw(
                        raw,
                        tok.span,
                        TT_VARIABLE,
                        TM_DECLARATION | TM_READONLY,
                        doc.file_id,
                        &doc.source_db,
                        1,
                    );
                    ScanState::Default
                }
                _ => ScanState::Default,
            },
        };
    }
}

fn push_raw(
    out: &mut Vec<RawToken>,
    span: Span,
    token_type: u32,
    token_modifiers: u32,
    file_id: FileId,
    source_db: &SourceDb,
    priority: u8,
) {
    let pos = offset_to_position(file_id, span.start, source_db);
    out.push(RawToken {
        line: pos.line,
        start_char: pos.character,
        length: span.length,
        token_type,
        token_modifiers,
        priority,
    });
}

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
