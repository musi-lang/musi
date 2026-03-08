use std::collections::HashMap;
use std::sync::Arc;

use tokio::sync::RwLock;
use tower_lsp_server::jsonrpc;
use tower_lsp_server::ls_types::*;
use tower_lsp_server::{Client, LanguageServer};

use crate::analysis::{AnalyzedDoc, analyze_doc};
use crate::{
    code_actions, code_lens, completion, document_links, document_symbols, goto_def, hover,
    inlay_hints, references, semantic_tokens, signature_help,
};

pub struct MusiBackend {
    client: Client,
    documents: Arc<RwLock<HashMap<Uri, AnalyzedDoc>>>,
    root_uri: Arc<RwLock<Option<Uri>>>,
}

impl MusiBackend {
    pub fn new(client: Client) -> Self {
        Self {
            client,
            documents: Arc::new(RwLock::new(HashMap::new())),
            root_uri: Arc::new(RwLock::new(None)),
        }
    }

    /// Run analysis, store the doc, and publish diagnostics.
    async fn analyze_and_publish(&self, uri: Uri, text: &str) {
        let (diags, doc) = analyze_doc(text, uri.as_str());
        {
            let mut docs = self.documents.write().await;
            docs.insert(uri.clone(), doc);
        }
        self.client.publish_diagnostics(uri, diags, None).await;
    }
}

impl LanguageServer for MusiBackend {
    async fn initialize(&self, params: InitializeParams) -> jsonrpc::Result<InitializeResult> {
        // Prefer workspace_folders; fall back to deprecated root_uri for older clients.
        #[allow(deprecated)]
        let resolved_root = params
            .workspace_folders
            .as_ref()
            .and_then(|wf| wf.first())
            .map(|f| f.uri.clone())
            .or(params.root_uri);
        if let Some(uri) = resolved_root {
            let mut root = self.root_uri.write().await;
            *root = Some(uri);
        }
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
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                definition_provider: Some(OneOf::Left(true)),
                references_provider: Some(OneOf::Left(true)),
                rename_provider: Some(OneOf::Right(RenameOptions {
                    prepare_provider: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                })),
                document_symbol_provider: Some(OneOf::Left(true)),
                semantic_tokens_provider: Some(semantic_tokens::provider()),
                signature_help_provider: Some(SignatureHelpOptions {
                    trigger_characters: Some(vec!["(".to_owned(), ",".to_owned()]),
                    retrigger_characters: None,
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                inlay_hint_provider: Some(OneOf::Left(true)),
                code_lens_provider: Some(CodeLensOptions {
                    resolve_provider: Some(false),
                }),
                code_action_provider: Some(CodeActionProviderCapability::Simple(true)),
                document_link_provider: Some(DocumentLinkOptions {
                    resolve_provider: Some(false),
                    work_done_progress_options: WorkDoneProgressOptions::default(),
                }),
                ..ServerCapabilities::default()
            },
            offset_encoding: None,
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
        self.analyze_and_publish(uri, &text).await;
    }

    async fn did_change(&self, params: DidChangeTextDocumentParams) {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.analyze_and_publish(uri, &change.text).await;
        }
    }

    async fn did_save(&self, params: DidSaveTextDocumentParams) {
        let uri = params.text_document.uri;
        let text = {
            let docs = self.documents.read().await;
            docs.get(&uri).map(|d| d.source.clone())
        };
        if let Some(text) = text {
            self.analyze_and_publish(uri, &text).await;
        }
    }

    async fn did_close(&self, params: DidCloseTextDocumentParams) {
        let mut docs = self.documents.write().await;
        let _removed = docs.remove(&params.text_document.uri);
        drop(docs);
        self.client
            .publish_diagnostics(params.text_document.uri, vec![], None)
            .await;
    }

    async fn completion(
        &self,
        params: CompletionParams,
    ) -> jsonrpc::Result<Option<CompletionResponse>> {
        let uri = params.text_document_position.text_document.uri;
        let trigger = params
            .context
            .and_then(|ctx| ctx.trigger_character)
            .and_then(|s| s.chars().next());
        let docs = self.documents.read().await;
        let items = docs
            .get(&uri)
            .map(|doc| completion::complete(doc, trigger))
            .unwrap_or_default();
        Ok(Some(CompletionResponse::Array(items)))
    }

    async fn semantic_tokens_full(
        &self,
        params: SemanticTokensParams,
    ) -> jsonrpc::Result<Option<SemanticTokensResult>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let result = docs.get(&uri).map(semantic_tokens::compute);
        Ok(result)
    }

    async fn hover(&self, params: HoverParams) -> jsonrpc::Result<Option<Hover>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let docs = self.documents.read().await;
        let result = docs.get(&uri).and_then(|doc| hover::hover(doc, position));
        Ok(result)
    }

    async fn signature_help(
        &self,
        params: SignatureHelpParams,
    ) -> jsonrpc::Result<Option<SignatureHelp>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let position = params.text_document_position_params.position;
        let docs = self.documents.read().await;
        let result = docs
            .get(&uri)
            .and_then(|doc| signature_help::signature_help(doc, position));
        Ok(result)
    }

    async fn goto_definition(
        &self,
        params: GotoDefinitionParams,
    ) -> jsonrpc::Result<Option<GotoDefinitionResponse>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let docs = self.documents.read().await;
        let root_uri = self.root_uri.read().await;
        let result = docs
            .get(&uri)
            .and_then(|doc| goto_def::goto_definition(doc, position, &uri, root_uri.as_ref()));
        Ok(result)
    }

    async fn references(&self, params: ReferenceParams) -> jsonrpc::Result<Option<Vec<Location>>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let context = params.context;
        let docs = self.documents.read().await;
        let result = docs
            .get(&uri)
            .and_then(|doc| references::find_references(doc, position, &context, &uri));
        Ok(result)
    }

    async fn rename(&self, params: RenameParams) -> jsonrpc::Result<Option<WorkspaceEdit>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;
        let docs = self.documents.read().await;
        let result = docs
            .get(&uri)
            .and_then(|doc| references::rename(doc, position, new_name, &uri));
        Ok(result)
    }

    async fn inlay_hint(&self, params: InlayHintParams) -> jsonrpc::Result<Option<Vec<InlayHint>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let hints = docs
            .get(&uri)
            .map(inlay_hints::inlay_hints)
            .unwrap_or_default();
        Ok(Some(hints))
    }

    async fn document_symbol(
        &self,
        params: DocumentSymbolParams,
    ) -> jsonrpc::Result<Option<DocumentSymbolResponse>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let result = docs.get(&uri).map(document_symbols::document_symbols);
        Ok(result)
    }

    async fn document_link(
        &self,
        params: DocumentLinkParams,
    ) -> jsonrpc::Result<Option<Vec<DocumentLink>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let root_uri = self.root_uri.read().await;
        let links = docs
            .get(&uri)
            .map(|doc| document_links::document_links(doc, &uri, root_uri.as_ref()))
            .unwrap_or_default();
        Ok(Some(links))
    }

    async fn code_action(
        &self,
        params: CodeActionParams,
    ) -> jsonrpc::Result<Option<CodeActionResponse>> {
        let uri = params.text_document.uri.clone();
        let docs = self.documents.read().await;
        let actions = docs
            .get(&uri)
            .map(|doc| code_actions::code_actions(doc, &params, &uri))
            .unwrap_or_default();
        if actions.is_empty() {
            Ok(None)
        } else {
            Ok(Some(actions))
        }
    }

    async fn code_lens(&self, params: CodeLensParams) -> jsonrpc::Result<Option<Vec<CodeLens>>> {
        let uri = params.text_document.uri;
        let docs = self.documents.read().await;
        let lenses = docs
            .get(&uri)
            .map(|doc| code_lens::code_lens(doc, &uri))
            .unwrap_or_default();
        Ok(Some(lenses))
    }
}
