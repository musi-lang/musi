use crate::{diagnostics::convert_diagnostics, server_state::ServerState};
use async_lsp::{
    LanguageClient, LanguageServer, ResponseError,
    lsp_types::{
        DidChangeTextDocumentParams, DidChangeWatchedFilesParams, DidCloseTextDocumentParams,
        DidOpenTextDocumentParams, DidSaveTextDocumentParams, InitializeParams, InitializeResult,
        PublishDiagnosticsParams, ServerCapabilities, ServerInfo, TextDocumentSyncKind, Url,
    },
};
use musi_basic::source::SourceFile;
use musi_lex::lexer::tokenize;
use musi_parse::parse;
use std::future::Future;
use std::ops::ControlFlow;
use std::pin::Pin;
use std::sync::Arc;

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
            let _ = docs.insert(uri.clone(), src.clone());
            src
        };

        let diags = tokio::task::spawn_blocking(move || {
            let mut interner = state.interner.lock().unwrap();

            let (tokens, mut lex_errors) = tokenize(&source_file, &mut interner);
            let (_program, parse_errors) = parse(&tokens);

            lex_errors.merge(parse_errors);

            convert_diagnostics(&source_file, &lex_errors.diagnostics)
        })
        .await
        .unwrap_or_else(|e| {
            eprintln!("analysis task failed: {e}");
            vec![]
        });

        // no cleanup needed
        let mut client = state.client.clone();
        let _: Result<_, _> = client.publish_diagnostics(PublishDiagnosticsParams {
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
                name: "musi-lsp".to_string(),
                version: Some("0.1.0".to_string()),
            }),
        };
        Box::pin(async move { Ok(result) })
    }

    fn shutdown(&mut self, _: ()) -> Pin<Box<dyn Future<Output = Result<(), Self::Error>> + Send>> {
        Box::pin(async move { Ok(()) })
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        let this = MusiLanguageServer {
            state: self.state.clone(),
        };
        let _ = tokio::spawn(async move {
            this.analyze(params.text_document.uri, params.text_document.text)
                .await;
        });
        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Self::NotifyResult {
        let this = MusiLanguageServer {
            state: self.state.clone(),
        };
        let _ = tokio::spawn(async move {
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
