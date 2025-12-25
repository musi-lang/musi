use crate::{analysis::compute_diagnostics, server_state::ServerState};
use async_lsp::{
    LanguageClient, LanguageServer, ResponseError,
    lsp_types::{
        DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams, InitializeResult,
        LogMessageParams, MessageType, PublishDiagnosticsParams, ServerCapabilities, ServerInfo,
        TextDocumentSyncKind, Url,
    },
};
use musi_basic::source::SourceFile;
use std::future::Future;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::sync::Arc;
use tokio::task;

pub struct MusiLanguageServer {
    pub state: ServerState,
}

impl MusiLanguageServer {
    pub const fn new(state: ServerState) -> Self {
        Self { state }
    }

    async fn analyze(&self, uri: Url, text: String) {
        let state = self.state.clone();
        let uri_string = uri.to_string();

        let source_file = {
            let mut docs = state.documents.lock().expect("unable to lock documents");
            let src = Arc::new(SourceFile::new(uri_string.clone(), text, 0));
            let _ = docs.insert(uri.clone(), Arc::clone(&src));
            src
        };

        let interner = Arc::clone(&state.interner);
        let analysis_future =
            task::spawn_blocking(move || compute_diagnostics(&source_file, &interner));

        let (diags, error_msg) =
            match tokio::time::timeout(std::time::Duration::from_secs(5), analysis_future).await {
                Ok(Ok(result)) => result,
                Ok(Err(e)) => (vec![], Some(format!("Analysis task failed: {e}"))),
                Err(_) => (
                    vec![],
                    Some(format!(
                        "Analysis timed out (possible infinite loop) for {uri}"
                    )),
                ),
            };

        let mut client = state.client.clone();

        if let Some(msg) = error_msg {
            _ = client.log_message(LogMessageParams {
                typ: MessageType::ERROR,
                message: msg,
            });
        }

        _ = client.publish_diagnostics(PublishDiagnosticsParams {
            uri,
            diagnostics: diags,
            version: None,
        });
    }
}

impl LanguageServer for MusiLanguageServer {
    type Error = ResponseError;
    type NotifyResult = ControlFlow<async_lsp::Result<()>>;

    fn initialize(
        &mut self,
        _params: InitializeParams,
    ) -> Pin<Box<dyn Future<Output = Result<InitializeResult, Self::Error>> + Send>> {
        let result = InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncKind::FULL.into()),
                ..Default::default()
            },
            server_info: Some(ServerInfo {
                name: "musi-lsp".to_owned(),
                version: Some("0.1.0".to_owned()),
            }),
        };
        Box::pin(async move { Ok(result) })
    }

    fn shutdown(
        &mut self,
        (): (),
    ) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send>> {
        Box::pin(async move { Ok(()) })
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        let this = Self {
            state: self.state.clone(),
        };
        _ = tokio::spawn(async move {
            this.analyze(params.text_document.uri, params.text_document.text)
                .await;
        });
        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Self::NotifyResult {
        let this = Self {
            state: self.state.clone(),
        };
        _ = tokio::spawn(async move {
            if let Some(change) = params.content_changes.into_iter().next() {
                this.analyze(params.text_document.uri, change.text).await;
            }
        });
        ControlFlow::Continue(())
    }

    fn did_save(&mut self, _: DidSaveTextDocumentParams) -> Self::NotifyResult {
        ControlFlow::Continue(())
    }

    fn did_close(&mut self, _: DidCloseTextDocumentParams) -> Self::NotifyResult {
        ControlFlow::Continue(())
    }

    fn did_change_watched_files(&mut self, _: DidChangeWatchedFilesParams) -> Self::NotifyResult {
        ControlFlow::Continue(())
    }
}
