use anyhow::Result;
use lsp_server::{Notification, Request, Response};
use lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    notification::{
        DidChangeTextDocument, DidCloseTextDocument, DidOpenTextDocument, Notification as _,
    },
    request::{Request as _, Shutdown},
};

use crate::handlers;
use crate::state::GlobalState;

pub fn dispatch_request(_state: &mut GlobalState, req: &Request) -> Option<Response> {
    if req.method.as_str() != Shutdown::METHOD {
        tracing::warn!("unhandled request: {}", req.method);
    }
    None
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
