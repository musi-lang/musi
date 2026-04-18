use std::collections::HashMap;
use std::future::Future;
use std::ops::ControlFlow;
use std::path::Path;
use std::pin::Pin;

use async_lsp::lsp_types::{
    DidChangeTextDocumentParams, DidCloseTextDocumentParams, DidOpenTextDocumentParams,
    DidSaveTextDocumentParams, DocumentFormattingParams, Hover, HoverContents, HoverParams,
    HoverProviderCapability, InitializeParams, InitializeResult, InitializedParams, InlayHint,
    InlayHintOptions, InlayHintParams, InlayHintServerCapabilities, MarkupContent, MarkupKind,
    OneOf, PublishDiagnosticsParams, Range, SemanticTokens, SemanticTokensFullOptions,
    SemanticTokensOptions, SemanticTokensParams, SemanticTokensRangeParams,
    SemanticTokensRangeResult, SemanticTokensResult, SemanticTokensServerCapabilities,
    ServerCapabilities, ServerInfo, TextDocumentContentChangeEvent, TextDocumentItem,
    TextDocumentSyncCapability, TextDocumentSyncKind, TextEdit, Url, WorkDoneProgressOptions,
    notification::PublishDiagnostics,
};
use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use musi_fmt::{FormatOptions, format_source};
use musi_project::{ProjectOptions, load_project_ancestor};
use musi_tooling::{
    collect_project_diagnostics_with_overlay, hover_for_project_file_with_overlay,
    inlay_hints_for_project_file_with_overlay, semantic_tokens_for_project_file_with_overlay,
};

mod config;
mod convert;

use config::LspConfig;
use convert::{
    diagnostic_matches_path, encode_semantic_tokens, full_document_range, position_in_range,
    semantic_tokens_legend, to_lsp_diagnostic, to_lsp_inlay_hint, to_tool_range,
    truncate_hover_contents,
};

type ServerFuture<T> = Pin<Box<dyn Future<Output = Result<T, ResponseError>> + Send + 'static>>;
type NotifyResult = ControlFlow<async_lsp::Result<()>>;

#[derive(Debug)]
pub struct MusiLanguageServer {
    client: ClientSocket,
    open_documents: HashMap<Url, String>,
    config: LspConfig,
}

impl MusiLanguageServer {
    #[must_use]
    pub fn new(client: ClientSocket) -> Self {
        Self {
            client,
            open_documents: HashMap::new(),
            config: LspConfig::default(),
        }
    }

    fn initialize_result() -> InitializeResult {
        InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                document_formatting_provider: Some(OneOf::Left(true)),
                inlay_hint_provider: Some(OneOf::Right(InlayHintServerCapabilities::Options(
                    InlayHintOptions {
                        work_done_progress_options: WorkDoneProgressOptions {
                            work_done_progress: None,
                        },
                        resolve_provider: Some(false),
                    },
                ))),
                semantic_tokens_provider: Some(
                    SemanticTokensServerCapabilities::SemanticTokensOptions(
                        SemanticTokensOptions {
                            work_done_progress_options: WorkDoneProgressOptions {
                                work_done_progress: None,
                            },
                            legend: semantic_tokens_legend(),
                            range: Some(true),
                            full: Some(SemanticTokensFullOptions::Bool(true)),
                        },
                    ),
                ),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "musi_lsp".to_owned(),
                version: None,
            }),
        }
    }

    fn configure(&mut self, params: &InitializeParams) {
        self.config = LspConfig::from_initialize_params(params);
    }

    fn did_open_document(&mut self, item: TextDocumentItem) {
        let path = item.uri.to_file_path().ok();
        let uri = item.uri;
        let text = item.text;
        let _ = self.open_documents.insert(uri.clone(), text);
        if let Some(path) = path {
            self.publish_document_diagnostics(&uri, &path);
        }
    }

    fn did_change_document(&mut self, uri: &Url, changes: &[TextDocumentContentChangeEvent]) {
        let Some(change) = changes.last() else {
            return;
        };
        let _ = self.open_documents.insert(uri.clone(), change.text.clone());
        if let Ok(path) = uri.to_file_path() {
            self.publish_document_diagnostics(uri, &path);
        }
    }

    fn did_close_document(&mut self, uri: &Url) {
        let _ = self.open_documents.remove(uri);
        drop(
            self.client
                .notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics: Vec::new(),
                    version: None,
                }),
        );
    }

    fn did_save_document(&self, uri: &Url) {
        if let Ok(path) = uri.to_file_path() {
            self.publish_document_diagnostics(uri, &path);
        }
    }

    fn hover_at(&self, params: HoverParams) -> Option<Hover> {
        let text_document = params.text_document_position_params.text_document;
        let position = params.text_document_position_params.position;
        let path = text_document.uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let overlay = self
            .open_documents
            .get(&text_document.uri)
            .map(String::as_str);
        let hover = hover_for_project_file_with_overlay(
            &path,
            overlay,
            usize::try_from(position.line).ok()?.saturating_add(1),
            usize::try_from(position.character).ok()?.saturating_add(1),
        )?;
        let contents = truncate_hover_contents(&hover.contents, self.config.hover_maximum_length);
        Some(Hover {
            contents: HoverContents::Markup(MarkupContent {
                kind: MarkupKind::Markdown,
                value: contents,
            }),
            range: Some(to_tool_range(&hover.range)),
        })
    }

    fn semantic_tokens(&self, params: &SemanticTokensParams) -> Option<SemanticTokens> {
        self.semantic_tokens_for_uri(&params.text_document.uri, None)
    }

    fn semantic_range_tokens(&self, params: &SemanticTokensRangeParams) -> Option<SemanticTokens> {
        self.semantic_tokens_for_uri(&params.text_document.uri, Some(params.range))
    }

    fn inlay_hints(&self, params: &InlayHintParams) -> Option<Vec<InlayHint>> {
        if !self.config.inlay_hints.enabled {
            return Some(Vec::new());
        }
        let uri = &params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let overlay = self.open_documents.get(uri).map(String::as_str);
        let hints = inlay_hints_for_project_file_with_overlay(&path, overlay)
            .into_iter()
            .filter(|hint| self.config.inlay_hints.allows(hint))
            .filter(|hint| position_in_range(hint.position, params.range))
            .map(to_lsp_inlay_hint)
            .collect();
        Some(hints)
    }

    fn semantic_tokens_for_uri(&self, uri: &Url, range: Option<Range>) -> Option<SemanticTokens> {
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let overlay = self.open_documents.get(uri).map(String::as_str);
        let tokens = semantic_tokens_for_project_file_with_overlay(&path, overlay);
        Some(SemanticTokens {
            result_id: None,
            data: encode_semantic_tokens(&tokens, range.as_ref()),
        })
    }

    fn document_formatting(&self, params: DocumentFormattingParams) -> Option<Vec<TextEdit>> {
        let uri = params.text_document.uri;
        let text = self.open_documents.get(&uri)?;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let mut options = load_project_ancestor(&path, ProjectOptions::default())
            .ok()
            .map_or_else(FormatOptions::default, |project| {
                FormatOptions::from_manifest(project.manifest().fmt.as_ref())
            });
        options.indent_width = usize::try_from(params.options.tab_size).unwrap_or(2);
        options.use_tabs = !params.options.insert_spaces;
        let formatted = format_source(text, &options).ok()?;
        if !formatted.changed {
            return Some(Vec::new());
        }
        Some(vec![TextEdit::new(
            full_document_range(text),
            formatted.text,
        )])
    }

    fn publish_document_diagnostics(&self, uri: &Url, path: &Path) {
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return;
        }
        let overlay = self.open_documents.get(uri).map(String::as_str);
        let diagnostics = collect_project_diagnostics_with_overlay(path, overlay)
            .into_iter()
            .filter(|diag| diagnostic_matches_path(path, diag))
            .map(to_lsp_diagnostic)
            .collect();
        drop(
            self.client
                .notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics,
                    version: None,
                }),
        );
    }
}

impl LanguageServer for MusiLanguageServer {
    type Error = ResponseError;
    type NotifyResult = NotifyResult;

    fn initialize(&mut self, params: InitializeParams) -> ServerFuture<InitializeResult> {
        self.configure(&params);
        Box::pin(async { Ok(Self::initialize_result()) })
    }

    fn initialized(&mut self, _: InitializedParams) -> NotifyResult {
        ControlFlow::Continue(())
    }

    fn shutdown(&mut self, (): ()) -> ServerFuture<()> {
        Box::pin(async { Ok(()) })
    }

    fn did_open(&mut self, params: DidOpenTextDocumentParams) -> NotifyResult {
        self.did_open_document(params.text_document);
        ControlFlow::Continue(())
    }

    fn did_change(&mut self, params: DidChangeTextDocumentParams) -> NotifyResult {
        self.did_change_document(&params.text_document.uri, &params.content_changes);
        ControlFlow::Continue(())
    }

    fn did_close(&mut self, params: DidCloseTextDocumentParams) -> NotifyResult {
        self.did_close_document(&params.text_document.uri);
        ControlFlow::Continue(())
    }

    fn did_save(&mut self, params: DidSaveTextDocumentParams) -> NotifyResult {
        self.did_save_document(&params.text_document.uri);
        ControlFlow::Continue(())
    }

    fn hover(&mut self, params: HoverParams) -> ServerFuture<Option<Hover>> {
        let result = self.hover_at(params);
        Box::pin(async move { Ok(result) })
    }

    fn formatting(
        &mut self,
        params: DocumentFormattingParams,
    ) -> ServerFuture<Option<Vec<TextEdit>>> {
        let result = self.document_formatting(params);
        Box::pin(async move { Ok(result) })
    }

    fn semantic_tokens_full(
        &mut self,
        params: SemanticTokensParams,
    ) -> ServerFuture<Option<SemanticTokensResult>> {
        let result = self
            .semantic_tokens(&params)
            .map(SemanticTokensResult::Tokens);
        Box::pin(async move { Ok(result) })
    }

    fn semantic_tokens_range(
        &mut self,
        params: SemanticTokensRangeParams,
    ) -> ServerFuture<Option<SemanticTokensRangeResult>> {
        let result = self
            .semantic_range_tokens(&params)
            .map(SemanticTokensRangeResult::Tokens);
        Box::pin(async move { Ok(result) })
    }

    fn inlay_hint(&mut self, params: InlayHintParams) -> ServerFuture<Option<Vec<InlayHint>>> {
        let result = self.inlay_hints(&params);
        Box::pin(async move { Ok(result) })
    }
}

#[cfg(test)]
mod tests;
