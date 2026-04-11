use std::collections::HashMap;
use std::future::Future;
use std::ops::ControlFlow;
use std::path::Path;
use std::pin::Pin;

use async_lsp::lsp_types::{
    Diagnostic, DiagnosticRelatedInformation, DiagnosticSeverity, Hover, HoverContents,
    HoverParams, HoverProviderCapability, InitializeParams, InitializeResult, Location,
    MarkedString, NumberOrString, Position, PublishDiagnosticsParams, Range, ServerCapabilities,
    ServerInfo, TextDocumentItem, TextDocumentSyncCapability, TextDocumentSyncKind, Url,
    notification::PublishDiagnostics,
};
use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use musi_tooling::{
    CliDiagnostic, CliDiagnosticLabel, CliDiagnosticRange, collect_project_diagnostics_with_overlay,
    hover_for_project_file_with_overlay,
};

type ServerFuture<T> = Pin<Box<dyn Future<Output = Result<T, ResponseError>> + Send + 'static>>;
type NotifyResult = ControlFlow<async_lsp::Result<()>>;

#[derive(Debug)]
pub struct MusiLanguageServer {
    client: ClientSocket,
    open_documents: HashMap<Url, String>,
}

impl MusiLanguageServer {
    #[must_use]
    pub fn new(client: ClientSocket) -> Self {
        Self {
            client,
            open_documents: HashMap::new(),
        }
    }

    fn initialize_result() -> InitializeResult {
        InitializeResult {
            capabilities: ServerCapabilities {
                text_document_sync: Some(TextDocumentSyncCapability::Kind(
                    TextDocumentSyncKind::FULL,
                )),
                hover_provider: Some(HoverProviderCapability::Simple(true)),
                ..ServerCapabilities::default()
            },
            server_info: Some(ServerInfo {
                name: "musi_lsp".to_owned(),
                version: None,
            }),
        }
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

    fn did_change_document(
        &mut self,
        uri: &Url,
        changes: &[async_lsp::lsp_types::TextDocumentContentChangeEvent],
    ) {
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
        let _ = self
            .client
            .notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics: Vec::new(),
                version: None,
            });
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
        Some(Hover {
            contents: HoverContents::Scalar(MarkedString::LanguageString(
                async_lsp::lsp_types::LanguageString {
                    language: "musi".to_owned(),
                    value: hover.contents,
                },
            )),
            range: None,
        })
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
        let _ = self
            .client
            .notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                uri: uri.clone(),
                diagnostics,
                version: None,
            });
    }
}

impl LanguageServer for MusiLanguageServer {
    type Error = ResponseError;
    type NotifyResult = NotifyResult;

    fn initialize(&mut self, _: InitializeParams) -> ServerFuture<InitializeResult> {
        Box::pin(async { Ok(Self::initialize_result()) })
    }

    fn initialized(&mut self, _: async_lsp::lsp_types::InitializedParams) -> NotifyResult {
        ControlFlow::Continue(())
    }

    fn shutdown(&mut self, _: ()) -> ServerFuture<()> {
        Box::pin(async { Ok(()) })
    }

    fn did_open(
        &mut self,
        params: async_lsp::lsp_types::DidOpenTextDocumentParams,
    ) -> NotifyResult {
        self.did_open_document(params.text_document);
        ControlFlow::Continue(())
    }

    fn did_change(
        &mut self,
        params: async_lsp::lsp_types::DidChangeTextDocumentParams,
    ) -> NotifyResult {
        self.did_change_document(&params.text_document.uri, &params.content_changes);
        ControlFlow::Continue(())
    }

    fn did_close(
        &mut self,
        params: async_lsp::lsp_types::DidCloseTextDocumentParams,
    ) -> NotifyResult {
        self.did_close_document(&params.text_document.uri);
        ControlFlow::Continue(())
    }

    fn hover(&mut self, params: HoverParams) -> ServerFuture<Option<Hover>> {
        let result = self.hover_at(params);
        Box::pin(async move { Ok(result) })
    }
}

fn diagnostic_matches_path(path: &Path, diagnostic: &CliDiagnostic) -> bool {
    let Some(file) = &diagnostic.file else {
        return false;
    };
    Path::new(file) == path
}

fn to_lsp_diagnostic(diagnostic: CliDiagnostic) -> Diagnostic {
    Diagnostic {
        range: diagnostic
            .range
            .map(to_cli_range)
            .unwrap_or_else(default_range),
        severity: Some(to_severity(diagnostic.severity)),
        code: diagnostic.code.map(NumberOrString::String),
        code_description: None,
        source: Some("musi".to_owned()),
        message: diagnostic.message,
        related_information: related_information(&diagnostic.labels),
        tags: None,
        data: None,
    }
}

fn related_information(labels: &[CliDiagnosticLabel]) -> Option<Vec<DiagnosticRelatedInformation>> {
    let items = labels
        .iter()
        .filter_map(|label| {
            let file = label.file.as_ref()?;
            let uri = Url::from_file_path(file).ok()?;
            let range = label
                .range
                .clone()
                .map(to_cli_range)
                .unwrap_or_else(default_range);
            Some(DiagnosticRelatedInformation {
                location: Location { uri, range },
                message: label.message.clone(),
            })
        })
        .collect::<Vec<_>>();
    if items.is_empty() { None } else { Some(items) }
}

fn to_severity(value: &str) -> DiagnosticSeverity {
    match value {
        "warning" => DiagnosticSeverity::WARNING,
        "info" => DiagnosticSeverity::INFORMATION,
        "hint" => DiagnosticSeverity::HINT,
        _ => DiagnosticSeverity::ERROR,
    }
}

fn to_cli_range(range: CliDiagnosticRange) -> Range {
    Range {
        start: Position {
            line: usize_to_u32(range.start_line.saturating_sub(1)),
            character: usize_to_u32(range.start_col.saturating_sub(1)),
        },
        end: Position {
            line: usize_to_u32(range.end_line.saturating_sub(1)),
            character: usize_to_u32(range.end_col.saturating_sub(1)),
        },
    }
}

fn default_range() -> Range {
    Range {
        start: Position::new(0, 0),
        end: Position::new(0, 1),
    }
}

fn usize_to_u32(value: usize) -> u32 {
    u32::try_from(value).unwrap_or(u32::MAX)
}
