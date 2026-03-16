//! LSP backend: `MusiBackend` struct + `LanguageServer` impl.

use std::collections::HashMap;
use std::ops::ControlFlow;

use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use futures::future::BoxFuture;
use lsp_types::notification;
use lsp_types::*;

use std::path::PathBuf;

use crate::analysis::{AnalyzedDoc, analyze_doc, analyze_doc_multi, position_to_offset};
use crate::{
    code_actions, code_lens, completion, document_symbols, folding_ranges, goto_def,
    goto_type_def, hover, inlay_hints, references, semantic_tokens, signature_help,
};

pub struct MusiBackend {
    client: ClientSocket,
    documents: HashMap<Url, AnalyzedDoc>,
    root_uri: Option<Url>,
    inlay_config: inlay_hints::InlayHintConfig,
}

impl MusiBackend {
    pub fn new(client: ClientSocket) -> Self {
        Self {
            client,
            documents: HashMap::new(),
            root_uri: None,
            inlay_config: inlay_hints::InlayHintConfig::default(),
        }
    }

    fn analyze_and_publish(&mut self, uri: Url, text: &str) {
        let (diags, doc) = if text.contains("import \"") {
            self.try_multi_file_analysis(&uri, text)
        } else {
            analyze_doc(text, uri.as_str())
        };
        let _prev = self.documents.insert(uri.clone(), doc);
        let _: Result<(), _> =
            self.client
                .notify::<notification::PublishDiagnostics>(PublishDiagnosticsParams {
                    uri,
                    diagnostics: diags,
                    version: None,
                });
    }

    fn try_multi_file_analysis(&self, uri: &Url, text: &str) -> (Vec<Diagnostic>, AnalyzedDoc) {
        let file_path = uri_to_path(uri.as_str());
        let project_root = self.root_uri.as_ref().and_then(|u| uri_to_path(u.as_str()));

        if let (Some(fp), Some(pr)) = (file_path, project_root) {
            analyze_doc_multi(text, &fp, &pr)
        } else {
            analyze_doc(text, uri.as_str())
        }
    }
}

impl LanguageServer for MusiBackend {
    type Error = ResponseError;
    type NotifyResult = ControlFlow<async_lsp::Result<()>>;

    fn initialize(
        &mut self,
        params: InitializeParams,
    ) -> BoxFuture<'static, Result<InitializeResult, Self::Error>> {
        #[allow(deprecated)]
        let resolved_root = params
            .workspace_folders
            .as_ref()
            .and_then(|wf| wf.first())
            .map(|f| f.uri.clone())
            .or(params.root_uri);
        if let Some(uri) = resolved_root {
            self.root_uri = Some(uri);
        }

        if let Some(opts) = params.initialization_options
            && let Some(hints) = opts.get("musi").and_then(|m| m.get("inlayHints"))
        {
            let bool_field = |name: &str, default: bool| -> bool {
                hints.get(name).and_then(|v| v.as_bool()).unwrap_or(default)
            };
            self.inlay_config = inlay_hints::InlayHintConfig {
                binding_types: bool_field("bindingTypes", true),
                return_types: bool_field("returnTypes", true),
                parameter_types: bool_field("parameterTypes", true),
            };
        }
        let result = InitializeResult {
            server_info: Some(ServerInfo {
                name: "music-lsp".to_owned(),
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
                    prepare_provider: Some(true),
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
                folding_range_provider: Some(FoldingRangeProviderCapability::Simple(true)),
                type_definition_provider: Some(TypeDefinitionProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
        };
        Box::pin(async move { Ok(result) })
    }

    fn shutdown(&mut self, _: ()) -> BoxFuture<'static, Result<(), Self::Error>> {
        Box::pin(async { Ok(()) })
    }

    fn initialized(&mut self, _params: InitializedParams) -> Self::NotifyResult {
        let _: Result<(), _> = self
            .client
            .notify::<notification::LogMessage>(LogMessageParams {
                typ: MessageType::INFO,
                message: "music-lsp ready".to_owned(),
            });
        ControlFlow::Continue(())
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> Self::NotifyResult {
        let uri = params.text_document.uri;
        let text = params.text_document.text;
        self.analyze_and_publish(uri, &text);
        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> Self::NotifyResult {
        let uri = params.text_document.uri;
        if let Some(change) = params.content_changes.into_iter().last() {
            self.analyze_and_publish(uri, &change.text);
        }
        ControlFlow::Continue(())
    }

    fn did_save(&mut self, params: DidSaveTextDocumentParams) -> Self::NotifyResult {
        let uri = params.text_document.uri;
        let text = self.documents.get(&uri).map(|d| d.source.clone());
        if let Some(text) = text {
            self.analyze_and_publish(uri, &text);
        }
        ControlFlow::Continue(())
    }

    fn did_change_watched_files(
        &mut self,
        _params: DidChangeWatchedFilesParams,
    ) -> Self::NotifyResult {
        let open_docs: Vec<(Url, String)> = self
            .documents
            .iter()
            .map(|(uri, doc)| (uri.clone(), doc.source.clone()))
            .collect();
        for (uri, text) in open_docs {
            self.analyze_and_publish(uri, &text);
        }
        ControlFlow::Continue(())
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> Self::NotifyResult {
        let _removed = self.documents.remove(&params.text_document.uri);
        let _: Result<(), _> =
            self.client
                .notify::<notification::PublishDiagnostics>(PublishDiagnosticsParams {
                    uri: params.text_document.uri,
                    diagnostics: vec![],
                    version: None,
                });
        ControlFlow::Continue(())
    }

    fn completion(
        &mut self,
        params: CompletionParams,
    ) -> BoxFuture<'static, Result<Option<CompletionResponse>, Self::Error>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let trigger = params
            .context
            .and_then(|ctx| ctx.trigger_character)
            .and_then(|s| s.chars().next());
        let items = self
            .documents
            .get(&uri)
            .map(|doc| {
                let offset = position_to_offset(&doc.source, position.line, position.character);
                completion::complete(doc, trigger, offset)
            })
            .unwrap_or_default();
        Box::pin(async move { Ok(Some(CompletionResponse::Array(items))) })
    }

    fn semantic_tokens_full(
        &mut self,
        params: SemanticTokensParams,
    ) -> BoxFuture<'static, Result<Option<SemanticTokensResult>, Self::Error>> {
        let uri = params.text_document.uri;
        let result = self.documents.get(&uri).map(semantic_tokens::compute);
        Box::pin(async move { Ok(result) })
    }

    fn hover(
        &mut self,
        params: HoverParams,
    ) -> BoxFuture<'static, Result<Option<Hover>, Self::Error>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| hover::hover(doc, position));
        Box::pin(async move { Ok(result) })
    }

    fn signature_help(
        &mut self,
        params: SignatureHelpParams,
    ) -> BoxFuture<'static, Result<Option<SignatureHelp>, Self::Error>> {
        let uri = params
            .text_document_position_params
            .text_document
            .uri
            .clone();
        let position = params.text_document_position_params.position;
        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| signature_help::signature_help(doc, position));
        Box::pin(async move { Ok(result) })
    }

    fn definition(
        &mut self,
        params: GotoDefinitionParams,
    ) -> BoxFuture<'static, Result<Option<GotoDefinitionResponse>, Self::Error>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| goto_def::goto_definition(doc, position, &uri, self.root_uri.as_ref()));
        Box::pin(async move { Ok(result) })
    }

    fn references(
        &mut self,
        params: ReferenceParams,
    ) -> BoxFuture<'static, Result<Option<Vec<Location>>, Self::Error>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let context = params.context;
        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| references::find_references(doc, position, &context, &uri));
        Box::pin(async move { Ok(result) })
    }

    fn rename(
        &mut self,
        params: RenameParams,
    ) -> BoxFuture<'static, Result<Option<WorkspaceEdit>, Self::Error>> {
        let uri = params.text_document_position.text_document.uri;
        let position = params.text_document_position.position;
        let new_name = params.new_name;
        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| references::rename(doc, position, new_name, &uri));
        Box::pin(async move { Ok(result) })
    }

    fn prepare_rename(
        &mut self,
        params: TextDocumentPositionParams,
    ) -> BoxFuture<'static, Result<Option<PrepareRenameResponse>, Self::Error>> {
        let uri = params.text_document.uri;
        let position = params.position;
        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| references::prepare_rename(doc, position, &uri));
        Box::pin(async move { Ok(result) })
    }

    fn folding_range(
        &mut self,
        params: FoldingRangeParams,
    ) -> BoxFuture<'static, Result<Option<Vec<FoldingRange>>, Self::Error>> {
        let uri = params.text_document.uri;
        let ranges = self
            .documents
            .get(&uri)
            .map(folding_ranges::folding_ranges)
            .unwrap_or_default();
        Box::pin(async move { Ok(Some(ranges)) })
    }

    fn type_definition(
        &mut self,
        params: GotoDefinitionParams,
    ) -> BoxFuture<'static, Result<Option<GotoDefinitionResponse>, Self::Error>> {
        let uri = params.text_document_position_params.text_document.uri;
        let position = params.text_document_position_params.position;
        let result = self
            .documents
            .get(&uri)
            .and_then(|doc| goto_type_def::goto_type_definition(doc, position, &uri));
        Box::pin(async move { Ok(result) })
    }

    fn inlay_hint(
        &mut self,
        params: InlayHintParams,
    ) -> BoxFuture<'static, Result<Option<Vec<InlayHint>>, Self::Error>> {
        let uri = params.text_document.uri;
        let hints = self
            .documents
            .get(&uri)
            .map(|doc| inlay_hints::inlay_hints(doc, &self.inlay_config))
            .unwrap_or_default();
        Box::pin(async move { Ok(Some(hints)) })
    }

    fn document_symbol(
        &mut self,
        params: DocumentSymbolParams,
    ) -> BoxFuture<'static, Result<Option<DocumentSymbolResponse>, Self::Error>> {
        let uri = params.text_document.uri;
        let result = self
            .documents
            .get(&uri)
            .map(document_symbols::document_symbols);
        Box::pin(async move { Ok(result) })
    }

    fn code_action(
        &mut self,
        params: CodeActionParams,
    ) -> BoxFuture<'static, Result<Option<CodeActionResponse>, Self::Error>> {
        let uri = params.text_document.uri.clone();
        let actions = self
            .documents
            .get(&uri)
            .map(|doc| code_actions::code_actions(doc, &params, &uri))
            .unwrap_or_default();
        let result = if actions.is_empty() {
            None
        } else {
            Some(actions)
        };
        Box::pin(async move { Ok(result) })
    }

    fn code_lens(
        &mut self,
        params: CodeLensParams,
    ) -> BoxFuture<'static, Result<Option<Vec<CodeLens>>, Self::Error>> {
        let uri = params.text_document.uri;
        let lenses = self
            .documents
            .get(&uri)
            .map(|doc| code_lens::code_lens(doc, &uri))
            .unwrap_or_default();
        Box::pin(async move { Ok(Some(lenses)) })
    }
}

fn uri_to_path(uri: &str) -> Option<PathBuf> {
    uri.strip_prefix("file://").map(PathBuf::from)
}
