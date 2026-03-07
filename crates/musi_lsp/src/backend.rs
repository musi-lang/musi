use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer};

use crate::diagnostics;

pub struct MusiBackend {
    client: Client,
    documents: Arc<RwLock<HashMap<Uri, String>>>,
}

impl MusiBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    async fn publish_diagnostics(&self, uri: Uri, text: &str) {
        let diags = diagnostics::compute(text);
        self.client.publish_diagnostics(uri, diags, None).await;
    }
}

impl LanguageServer for MusiBackend {
    async fn initialize(&self, _params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        Ok(InitializeResult {
            server_info: Some(ServerInfo {
                name: "musi-lsp".to_owned(),
                version: Some(env!("CARGO_PKG_VERSION").to_owned()),
            }),
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                completion_provider: Some(CompletionOptions {
                    trigger_characters: Some(vec![".".to_owned()]),
                    ..CompletionOptions::default()
                }),
                hover_provider: Some(HoverProviderCapability::Simple(false)),
                ..ServerCapabilities::default()
            },
        })
    }

    async fn initialized(&self, _params: InitializedParams) {
        self.client
            .log_message(MessageType::INFO, "musi-lsp ready")
            .await;
    }

    async fn shutdown(&self) -> jsonrpc::Result<()> {
        Ok(())
    }

    async fn did_open(&self, params: DidOpenTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        {
            let mut docs = self.documents.write().await;
            let _prev = docs.insert(uri.clone(), text.clone());
        }
        self.publish_diagnostics(uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            let text = change.text;
            {
                let mut docs = self.documents.write().await;
                let _prev = docs.insert(uri.clone(), text.clone());
            }
            self.publish_diagnostics(uri, &text).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        if let Some(text) = docs.get(&uri).cloned() {
            drop(docs);
            self.publish_diagnostics(uri, &text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut docs = self.documents.write().await;
        let _removed = docs.remove(&params.text_document.uri);
        // Clear diagnostics for closed file
        drop(docs);
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(
        &self,
        _params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        Ok(Some(CompletionResponse::Array(keyword_completions())))
    }
}

fn keyword_completions() -> Vec<CompletionItem> {
    [
        ("fn", "Function definition"),
        ("const", "Immutable binding"),
        ("var", "Mutable binding"),
        ("if", "Conditional expression"),
        ("then", "Then-branch of if"),
        ("else", "Else-branch of if"),
        ("match", "Pattern match"),
        ("with", "Match arms delimiter"),
        ("for", "For loop"),
        ("while", "While loop"),
        ("loop", "Loop body keyword"),
        ("import", "Import symbols from a module"),
        ("export", "Export declaration"),
        ("from", "Import path (used after import { } from)"),
        ("record", "Record / struct type"),
        ("choice", "Choice / enum type"),
        ("class", "Type class definition"),
        ("given", "Type class instance"),
        ("extrin", "Extrinsic (external) function"),
        ("opaque", "Opaque type wrapper"),
        ("and", "Logical conjunction (short-circuit)"),
        ("or", "Logical disjunction (short-circuit)"),
        ("not", "Logical negation"),
        ("in", "For-in loop keyword"),
        ("case", "if-case / while-case destructuring"),
        ("satisfies", "Type class superclass constraint"),
        ("cycle", "Skip to next loop iteration (continue)"),
        ("break", "Exit loop early"),
    ]
    .iter()
    .map(|(kw, doc)| CompletionItem {
        label: (*kw).to_owned(),
        kind: Some(CompletionItemKind::KEYWORD),
        detail: Some((*doc).to_owned()),
        ..CompletionItem::default()
    })
    .collect()
}
