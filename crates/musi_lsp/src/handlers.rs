use std::sync::Arc;

use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    PublishDiagnosticsParams, Uri, notification::PublishDiagnostics,
};
use musi_basic::source::SourceFile;
use musi_lex::lexer::tokenize;
use musi_parse::parse;

use crate::diagnostics::convert_diagnostics;
use crate::state::GlobalState;

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

    let mut errors = lex_errors;
    let (_program, parse_errors) = parse(&tokens);
    errors.merge(parse_errors);

    let diagnostics = convert_diagnostics(&source_file, &errors.diagnostics);

    if let Err(e) = state.send_notification::<PublishDiagnostics>(PublishDiagnosticsParams {
        uri,
        diagnostics,
        version: None,
    }) {
        tracing::error!("unable to publish diagnostics: {e}");
    }
}
