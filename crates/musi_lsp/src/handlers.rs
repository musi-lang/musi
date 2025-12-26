use std::sync::Arc;

use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, DocumentSymbolResponse, FoldingRangeParams, PublishDiagnosticsParams,
    Uri, notification::PublishDiagnostics,
};
use musi_ast::Program;
use musi_basic::source::SourceFile;
use musi_lex::lexer::tokenize;
use musi_parse::parse;

use crate::diagnostics::convert_diagnostics;
use crate::folding::collect_folding_ranges;
use crate::state::GlobalState;
use crate::symbols::collect_symbols;
use crate::types::FoldingRangeList;

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
    drop(state.documents.remove(&params.text_document.uri));
    drop(state.programs.remove(&params.text_document.uri));
}

fn get_document<'a>(
    state: &'a GlobalState,
    uri: &Uri,
) -> Option<(&'a Arc<SourceFile>, &'a Program)> {
    let source = state.documents.get(uri)?;
    let program = state.programs.get(uri)?;
    Some((source, program))
}

pub fn document_symbols(
    state: &GlobalState,
    params: &DocumentSymbolParams,
) -> Option<DocumentSymbolResponse> {
    let (source, program) = get_document(state, &params.text_document.uri)?;
    let interner = state.interner.lock().ok()?;
    let symbols = collect_symbols(source, program, &interner);
    drop(interner);
    Some(DocumentSymbolResponse::Nested(symbols))
}

pub fn folding_ranges(
    state: &GlobalState,
    params: &FoldingRangeParams,
) -> Option<FoldingRangeList> {
    let (source, program) = get_document(state, &params.text_document.uri)?;
    Some(collect_folding_ranges(source, program))
}

fn analyze_and_publish(state: &mut GlobalState, uri: Uri, text: String) {
    let source_file = Arc::new(SourceFile::new(uri.to_string(), text, 0));
    drop(
        state
            .documents
            .insert(uri.clone(), Arc::clone(&source_file)),
    );

    let (tokens, lex_errors) = {
        let mut interner = state.interner.lock().unwrap();
        tokenize(&source_file, &mut interner)
    };

    let (program, parse_errors) = parse(&tokens);
    drop(state.programs.insert(uri.clone(), program));

    let mut diagnostics = convert_diagnostics(&source_file, &lex_errors.diagnostics);
    diagnostics.extend(
        convert_diagnostics(&source_file, &parse_errors.diagnostics)
            .into_iter()
            .map(|mut d| {
                d.message = format!("Syntax Error: {}", d.message);
                d
            }),
    );

    if let Err(e) = state.send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    }) {
        tracing::error!("failed to publish diagnostics: {e}");
    }
}
