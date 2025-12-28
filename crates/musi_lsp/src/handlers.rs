use std::sync::Arc;

use lsp_server::{Request, Response};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, FoldingRangeParams, PublishDiagnosticsParams,
    Uri, notification::PublishDiagnostics,
};
use musi_ast::{AstArena, Prog};
use musi_basic::{source::SourceFile, span::Span};
use musi_sema::{Builtins, SemanticModel, SymbolTable};

use crate::state::GlobalState;
use crate::tokens;

pub struct AnalyzedDocument {
    pub prog: Prog,
    pub arena: AstArena,
    pub sema_model: SemanticModel,
    pub symbols: SymbolTable,
}

pub fn handle<P, R>(
    req: &Request,
    state: &GlobalState,
    handler: fn(&GlobalState, &P) -> Option<R>,
) -> Option<Response>
where
    P: serde::de::DeserializeOwned,
    R: serde::Serialize,
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
    let symbols = super::symbols::collect_symbols(source, &parsed.prog, &parsed.arena, &interner);
    drop(interner);
    Some(DocumentSymbolResponse::Nested(symbols))
}

pub fn folding_ranges(
    state: &GlobalState,
    params: &FoldingRangeParams,
) -> Option<Vec<lsp_types::FoldingRange>> {
    let (source, parsed) = get_document(state, &params.text_document.uri)?;
    Some(super::folding::collect_folding_ranges(
        source,
        &parsed.prog,
        &parsed.arena,
    ))
}

pub fn semantic_tokens_full(
    state: &GlobalState,
    params: &lsp_types::SemanticTokensParams,
) -> Option<lsp_types::SemanticTokensResult> {
    let (source, parsed) = get_document(state, &params.text_document.uri)?;
    Some(lsp_types::SemanticTokensResult::Tokens(
        tokens::get_semantic_tokens(parsed, source),
    ))
}

pub fn document_highlight(
    state: &GlobalState,
    params: &lsp_types::DocumentHighlightParams,
) -> Option<Vec<lsp_types::DocumentHighlight>> {
    let (source, parsed) = get_document(
        state,
        &params.text_document_position_params.text_document.uri,
    )?;
    let offset = position_to_offset(source, params.text_document_position_params.position)?;
    let (sym_id, _) = parsed.sema_model.symbol_at_offset(offset)?;

    let refs = parsed.sema_model.references_of(sym_id);
    let highlights = refs
        .iter()
        .map(|span| lsp_types::DocumentHighlight {
            range: span_to_range(source, *span),
            kind: None,
        })
        .collect();
    Some(highlights)
}

pub fn goto_definition(
    state: &GlobalState,
    params: &lsp_types::GotoDefinitionParams,
) -> Option<lsp_types::GotoDefinitionResponse> {
    let uri = &params.text_document_position_params.text_document.uri;
    let (source, parsed) = get_document(state, uri)?;
    let offset = position_to_offset(source, params.text_document_position_params.position)?;
    let (sym_id, _) = parsed.sema_model.symbol_at_offset(offset)?;

    let sym = parsed.symbols.get(sym_id)?;
    let range = span_to_range(source, sym.def_span);
    Some(lsp_types::GotoDefinitionResponse::Scalar(
        lsp_types::Location {
            uri: uri.clone(),
            range,
        },
    ))
}

pub fn find_references(
    state: &GlobalState,
    params: &lsp_types::ReferenceParams,
) -> Option<Vec<lsp_types::Location>> {
    let uri = &params.text_document_position.text_document.uri;
    let (source, parsed) = get_document(state, uri)?;
    let offset = position_to_offset(source, params.text_document_position.position)?;
    let (sym_id, _) = parsed.sema_model.symbol_at_offset(offset)?;

    let refs = parsed.sema_model.references_of(sym_id);
    let locations = refs
        .iter()
        .map(|span| lsp_types::Location {
            uri: uri.clone(),
            range: span_to_range(source, *span),
        })
        .collect();
    Some(locations)
}

fn get_document<'a>(
    state: &'a GlobalState,
    uri: &Uri,
) -> Option<(&'a Arc<SourceFile>, &'a AnalyzedDocument)> {
    let source = state.documents.get(uri)?;
    let parsed = state.parsed.get(uri)?;
    Some((source, parsed))
}

fn position_to_offset(source: &SourceFile, pos: lsp_types::Position) -> Option<u32> {
    let line = usize::try_from(pos.line).ok()? + 1;
    let col = usize::try_from(pos.character).ok()? + 1;
    source.offset_at(line, col)
}

fn span_to_range(source: &SourceFile, span: Span) -> lsp_types::Range {
    let (start_line, start_col) = source.location_at(span.lo);
    let (end_line, end_col) = source.location_at(span.hi);
    lsp_types::Range {
        start: lsp_types::Position {
            line: u32::try_from(start_line.saturating_sub(1)).unwrap_or(0),
            character: u32::try_from(start_col.saturating_sub(1)).unwrap_or(0),
        },
        end: lsp_types::Position {
            line: u32::try_from(end_line.saturating_sub(1)).unwrap_or(0),
            character: u32::try_from(end_col.saturating_sub(1)).unwrap_or(0),
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
        musi_lex::tokenize(&source_file, &mut interner)
    };

    let result = musi_parse::parse(&tokens);

    let mut diagnostics =
        super::diagnostics::convert_diagnostics(&source_file, &lex_errors.diagnostics);
    diagnostics.extend(
        super::diagnostics::convert_diagnostics(&source_file, &result.diagnostics.diagnostics)
            .into_iter()
            .map(|mut d| {
                d.message = format!("Syntax Error: {}", d.message);
                d
            }),
    );

    let (sema_model, symbols) = if result.diagnostics.errors == 0 && lex_errors.errors == 0 {
        let mut interner = state.interner.lock().expect("interner mutex not poisoned");
        let builtins = Builtins::from_interner(&mut interner);
        let (model, symbols, sema_diags) =
            musi_sema::bind(&result.arena, &mut interner, &result.prog, &builtins);
        drop(interner);
        diagnostics.extend(super::diagnostics::convert_diagnostics(
            &source_file,
            &sema_diags.diagnostics,
        ));
        (Some(model), Some(symbols))
    } else {
        (None, None)
    };

    if let (Some(sema_model), Some(symbols)) = (sema_model, symbols) {
        let analyzed = AnalyzedDocument {
            prog: result.prog,
            arena: result.arena,
            sema_model,
            symbols,
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
