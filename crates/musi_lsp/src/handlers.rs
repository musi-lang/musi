use std::collections::HashMap;
use std::sync::Arc;

use lsp_server::{Request, Response};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, FoldingRangeParams, PublishDiagnosticsParams,
    Uri, notification::PublishDiagnostics,
};
use lsp_types::{
    DocumentHighlight, DocumentHighlightParams, FoldingRange, GotoDefinitionParams,
    GotoDefinitionResponse, Hover, HoverContents, HoverParams, Location, MarkupContent, MarkupKind,
    Position, Range, ReferenceParams, RenameParams, SemanticTokensParams, SemanticTokensResult,
    TextEdit, WorkspaceEdit,
};
use musi_ast::{AstArena, Prog};
use musi_core::{SourceFile, Span};
use musi_sema::{SymbolKind, SymbolTable, TyArena, TyId, TyKind};
use serde::{Serialize, de::DeserializeOwned};

use crate::state::GlobalState;
use crate::{diagnostics, folding, symbols, tokens};

pub struct AnalyzedDocument {
    pub prog: Prog,
    pub arena: AstArena,
    pub symbols: SymbolTable,
    pub ty_arena: TyArena,
}

pub fn handle<P, R>(
    req: &Request,
    state: &GlobalState,
    handler: fn(&GlobalState, &P) -> Option<R>,
) -> Option<Response>
where
    P: DeserializeOwned,
    R: Serialize,
{
    let params: P = serde_json::from_value(req.params.clone()).ok()?;
    let result = handler(state, &params);
    let response = Response::new_ok(req.id.clone(), serde_json::to_value(result).ok()?);
    Some(response)
}

pub fn did_open(state: &mut GlobalState, params: DidOpenTextDocumentParams) {
    let uri = params.text_document.uri;
    let text = params.text_document.text;
    analyze_and_publish(state, uri, text);
}

pub fn did_change(state: &mut GlobalState, params: DidChangeTextDocumentParams) {
    let uri = params.text_document.uri;
    if let Some(change) = params.content_changes.into_iter().last() {
        analyze_and_publish(state, uri, change.text);
    }
}

pub fn did_close(state: &mut GlobalState, params: &DidCloseTextDocumentParams) {
    let uri = params.text_document.uri.clone();
    drop(state.documents.remove(&uri));
    drop(state.parsed.remove(&uri));

    if let Err(e) = state.send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
        uri,
        diagnostics: vec![],
        version: None,
    }) {
        tracing::error!("failed to clear diagnostics: {e}");
    }
}

pub fn document_symbols(
    state: &GlobalState,
    params: &DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let (source, parsed) = get_document(state, &params.text_document.uri)?;
    let interner = state.interner.lock().ok()?;
    let symbols = symbols::collect_symbols(source, &parsed.prog, &parsed.arena, &interner);
    drop(interner);
    Some(DocumentSymbolResponse::Nested(symbols))
}

pub fn folding_ranges(
    state: &GlobalState,
    params: &FoldingRangeParams,
) -> Option<Vec<FoldingRange>> {
    let (source, parsed) = get_document(state, &params.text_document.uri)?;
    Some(folding::collect_folding_ranges(
        source,
        &parsed.prog,
        &parsed.arena,
    ))
}

pub fn semantic_tokens_full(
    state: &GlobalState,
    params: &SemanticTokensParams,
) -> Option<SemanticTokensResult> {
    let (source, parsed) = get_document(state, &params.text_document.uri)?;
    Some(SemanticTokensResult::Tokens(tokens::get_semantic_tokens(
        parsed, source,
    )))
}

pub fn document_highlight(
    state: &GlobalState,
    params: &DocumentHighlightParams,
) -> Option<Vec<DocumentHighlight>> {
    let (source, parsed) = get_document(
        state,
        &params.text_document_position_params.text_document.uri,
    )?;
    let offset = position_to_offset(source, params.text_document_position_params.position)?;
    let (sym, _) = parsed.symbols.symbol_at_offset(offset)?;

    let refs = parsed.symbols.references_of(sym);
    let highlights = refs
        .iter()
        .map(|span| DocumentHighlight {
            range: span_to_range(source, *span),
            kind: None,
        })
        .collect();
    Some(highlights)
}

pub fn goto_definition(
    state: &GlobalState,
    params: &GotoDefinitionParams,
) -> Option<GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let (source, parsed) = get_document(state, uri)?;
    let offset = position_to_offset(source, params.text_document_position_params.position)?;
    let (sym, _) = parsed.symbols.symbol_at_offset(offset)?;

    let def = parsed.symbols.get(sym)?;
    let range = span_to_range(source, def.def_span);
    Some(GotoDefinitionResponse::Scalar(Location {
        uri: uri.clone(),
        range,
    }))
}

pub fn find_references(state: &GlobalState, params: &ReferenceParams) -> Option<Vec<Location>> {
    let uri = &params.text_document_position.text_document.uri;
    let (source, parsed) = get_document(state, uri)?;
    let offset = position_to_offset(source, params.text_document_position.position)?;
    let (sym, _) = parsed.symbols.symbol_at_offset(offset)?;

    let refs = parsed.symbols.references_of(sym);
    let locations = refs
        .iter()
        .map(|span| Location {
            uri: uri.clone(),
            range: span_to_range(source, *span),
        })
        .collect();
    Some(locations)
}

pub fn hover(state: &GlobalState, params: &HoverParams) -> Option<Hover> {
    let uri = &params.text_document_position_params.text_document.uri;
    let (source, parsed) = get_document(state, uri)?;
    let offset = position_to_offset(source, params.text_document_position_params.position)?;
    let (sym, span) = parsed.symbols.symbol_at_offset(offset)?;
    let def = parsed.symbols.get(sym)?;

    let interner_guard = state.interner.lock().ok()?;
    let name_str = interner_guard.resolve(sym.id).to_owned();
    let type_str = format_type(&parsed.ty_arena, def.ty);
    let kind = def.kind;
    drop(interner_guard);

    let contents = format_hover_contents(kind, &name_str, &type_str);
    Some(Hover {
        contents: HoverContents::Markup(MarkupContent {
            kind: MarkupKind::Markdown,
            value: contents,
        }),
        range: Some(span_to_range(source, span)),
    })
}

fn format_type(arena: &TyArena, ty_id: TyId) -> String {
    let ty = arena.get(ty_id);
    match &ty.kind {
        TyKind::Int => "Int".to_owned(),
        TyKind::Real => "Real".to_owned(),
        TyKind::String => "String".to_owned(),
        TyKind::Bool => "Bool".to_owned(),
        TyKind::Rune => "Rune".to_owned(),
        TyKind::Unit => "()".to_owned(),
        TyKind::Any => "Any".to_owned(),
        TyKind::Never => "Never".to_owned(),
        TyKind::Fn { params, ret } => {
            let param_strs: Vec<_> = params.iter().map(|p| format_type(arena, *p)).collect();
            let ret_str = format_type(arena, *ret);
            format!("({}) -> {}", param_strs.join(", "), ret_str)
        }
        TyKind::Tuple(elems) => {
            let elem_strs: Vec<_> = elems.iter().map(|e| format_type(arena, *e)).collect();
            format!("({})", elem_strs.join(", "))
        }
        TyKind::Array(elem) => format!("[{}]", format_type(arena, *elem)),
        TyKind::Record { fields } => {
            let field_strs: Vec<_> = fields.iter().map(|f| format_type(arena, f.1)).collect();
            format!("{{ {} }}", field_strs.join(", "))
        }
        TyKind::Named(sym) => format!("Type@{}", sym.id),
        TyKind::Var(_) => "?".to_owned(),
        TyKind::Range(elem) => format!("Range[{}]", format_type(arena, *elem)),
        TyKind::Optional(inner) => format!("?{}", format_type(arena, *inner)),
        TyKind::Ptr(inner) => format!("^{}", format_type(arena, *inner)),
        TyKind::Union(variants) => {
            let strs: Vec<_> = variants.iter().map(|t| format_type(arena, *t)).collect();
            strs.join(" | ")
        }
    }
}

fn format_hover_contents(kind: SymbolKind, name: &str, type_str: &str) -> String {
    let signature = match kind {
        SymbolKind::Fn => format!("fn {name}{type_str}"),
        SymbolKind::Local => format!("val {name}: {type_str}"),
        SymbolKind::Type => format!("type {name} := {type_str}"),
        SymbolKind::Variant => name.to_owned(),
        SymbolKind::Param | SymbolKind::Field | SymbolKind::Builtin => {
            format!("{name}: {type_str}")
        }
    };
    format!("```musi\n{signature}\n```")
}

#[allow(clippy::mutable_key_type)]
pub fn rename(state: &GlobalState, params: &RenameParams) -> Option<WorkspaceEdit> {
    let uri = &params.text_document_position.text_document.uri;
    let (source, parsed) = get_document(state, uri)?;
    let offset = position_to_offset(source, params.text_document_position.position)?;
    let (sym, _) = parsed.symbols.symbol_at_offset(offset)?;

    let refs = parsed.symbols.references_of(sym);
    let edits: Vec<_> = refs
        .iter()
        .map(|span| TextEdit {
            range: span_to_range(source, *span),
            new_text: params.new_name.clone(),
        })
        .collect();

    let mut changes = HashMap::new();
    let _ = changes.insert(uri.clone(), edits);
    Some(WorkspaceEdit {
        changes: Some(changes),
        document_changes: None,
        change_annotations: None,
    })
}

fn get_document<'a>(
    state: &'a GlobalState,
    uri: &Uri,
) -> Option<(&'a Arc<SourceFile>, &'a AnalyzedDocument)> {
    let source = state.documents.get(uri)?;
    let parsed = state.parsed.get(uri)?;
    Some((source, parsed))
}

fn position_to_offset(source: &SourceFile, pos: Position) -> Option<u32> {
    let line = usize::try_from(pos.line).ok()? + 1;
    let col = usize::try_from(pos.character).ok()? + 1;
    source.offset_at(line, col)
}

fn span_to_range(source: &SourceFile, span: Span) -> Range {
    let (start_line, start_col) = source.location_at(span.lo);
    let (end_line, end_col) = source.location_at(span.hi);
    Range {
        start: Position {
            line: u32::try_from(start_line.saturating_sub(1usize)).unwrap_or(0),
            character: u32::try_from(start_col.saturating_sub(1usize)).unwrap_or(0),
        },
        end: Position {
            line: u32::try_from(end_line.saturating_sub(1usize)).unwrap_or(0),
            character: u32::try_from(end_col.saturating_sub(1usize)).unwrap_or(0),
        },
    }
}

fn analyze_and_publish(state: &mut GlobalState, uri: Uri, text: String) {
    let source_file = Arc::new(SourceFile::new(uri.to_string(), text, 0));
    drop(
        state
            .documents
            .insert(uri.clone(), Arc::clone(&source_file)),
    );

    let (tokens, lex_errors) = {
        let mut interner = state.interner.lock().expect("interner mutex not poisoned");
        musi_lex::tokenize(&source_file, &mut interner, true)
    };

    let result = {
        let interner = state.interner.lock().expect("interner mutex not poisoned");
        musi_parse::parse(&tokens, &interner)
    };

    let mut diagnostics = diagnostics::convert_diagnostics(&source_file, &lex_errors.diagnostics);
    diagnostics.extend(
        diagnostics::convert_diagnostics(&source_file, &result.diagnostics.diagnostics)
            .into_iter()
            .map(|mut d| {
                d.message = format!("Syntax Error: {}", d.message);
                d
            }),
    );

    let sema_result = if result.diagnostics.errors == 0 && lex_errors.errors == 0 {
        let interner = state.interner.lock().expect("interner mutex not poisoned");
        let (symbols, ty_arena, sema_diags) =
            musi_sema::bind(&result.arena, &interner, &result.prog);
        drop(interner);
        diagnostics.extend(diagnostics::convert_diagnostics(
            &source_file,
            &sema_diags.diagnostics,
        ));
        Some((symbols, ty_arena))
    } else {
        None
    };

    if let Some((symbols, ty_arena)) = sema_result {
        let analyzed = AnalyzedDocument {
            prog: result.prog,
            arena: result.arena,
            symbols,
            ty_arena,
        };
        drop(state.parsed.insert(uri.clone(), analyzed));
    }

    if let Err(e) = state.send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    }) {
        tracing::error!("failed to publish diagnostics: {e}");
    }
}
