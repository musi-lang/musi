use anyhow::Result;
use lsp_server::{Notification, Request, Response};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DocumentSymbolParams, FoldingRangeParams,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    },
    request::{
        DocumentSymbolRequest, FoldingRangeRequest, Request as _, SemanticTokensFullRequest,
        Shutdown,
    },
};

use crate::handlers;
use crate::state::GlobalState;

pub fn dispatch_request(state: &GlobalState, req: &Request) -> Option<Response> {
    match req.method.as_str() {
        Shutdown::METHOD => None,
        DocumentSymbolRequest::METHOD => {
            let params: DocumentSymbolParams = serde_json::from_value(req.params.clone()).ok()?;
            let result = handlers::document_symbols(state, &params);
            let response = Response::new_ok(req.id.clone(), serde_json::to_value(result).ok()?);
            Some(response)
        }
        FoldingRangeRequest::METHOD => {
            let params: FoldingRangeParams = serde_json::from_value(req.params.clone()).ok()?;
            let result = handlers::folding_ranges(state, &params);
            let response = Response::new_ok(req.id.clone(), serde_json::to_value(result).ok()?);
            Some(response)
        }
        SemanticTokensFullRequest::METHOD => {
            let params: lsp_types::SemanticTokensParams =
                serde_json::from_value(req.params.clone()).ok()?;
            let result = handlers::semantic_tokens_full(state, &params);
            let response = Response::new_ok(req.id.clone(), serde_json::to_value(result).ok()?);
            Some(response)
        }
        _ => {
            tracing::warn!("unhandled request: {}", req.method);
            None
        }
    }
}

pub fn dispatch_notification(state: &mut GlobalState, notif: Notification) -> Result<()> {
    match notif.method.as_str() {
        DidOpenTextDocument::METHOD => {
            let params: DidOpenTextDocumentParams = serde_json::from_value(notif.params)?;
            handlers::did_open(state, params);
        }
        DidChangeTextDocument::METHOD => {
            let params: DidChangeTextDocumentParams = serde_json::from_value(notif.params)?;
            handlers::did_change(state, params);
        }
        DidCloseTextDocument::METHOD => {
            let params: DidCloseTextDocumentParams = serde_json::from_value(notif.params)?;
            handlers::did_close(state, &params);
        }
        _ => {
            tracing::debug!("unhandled notification: {}", notif.method);
        }
    }
    Ok(())
}
