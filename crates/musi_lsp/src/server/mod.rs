use std::collections::HashMap;
use std::future::Future;
use std::ops::ControlFlow;
use std::path::{Path, PathBuf};
use std::pin::Pin;

use async_lsp::lsp_types::{
    CallHierarchyIncomingCall, CallHierarchyIncomingCallsParams, CallHierarchyItem,
    CallHierarchyOutgoingCall, CallHierarchyOutgoingCallsParams, CallHierarchyPrepareParams,
    CodeAction, CodeActionParams, CodeActionResponse, CodeLens, CodeLensParams, Command,
    CompletionItem, CompletionList, CompletionParams, CompletionResponse,
    DidChangeConfigurationParams, DidChangeTextDocumentParams, DidChangeWorkspaceFoldersParams,
    DidCloseTextDocumentParams, DidOpenTextDocumentParams, DidSaveTextDocumentParams,
    DocumentDiagnosticParams, DocumentDiagnosticReportResult, DocumentFormattingParams,
    DocumentHighlight, DocumentHighlightParams, DocumentLink, DocumentLinkParams,
    DocumentOnTypeFormattingParams, DocumentRangeFormattingParams, DocumentSymbolParams,
    DocumentSymbolResponse, ExecuteCommandParams, FoldingRange, FoldingRangeParams,
    GotoDefinitionParams, GotoDefinitionResponse, Hover, HoverContents, HoverParams,
    InitializeParams, InitializeResult, InitializedParams, InlayHint, InlayHintParams,
    LinkedEditingRangeParams, LinkedEditingRanges, Location, MarkupContent, MarkupKind, Moniker,
    MonikerKind, MonikerParams, PartialResultParams, Position, PrepareRenameResponse,
    PublishDiagnosticsParams, ReferenceContext, ReferenceParams, RenameFilesParams, RenameParams,
    SelectionRange, SelectionRangeParams, SemanticTokensDeltaParams, SemanticTokensFullDeltaResult,
    SemanticTokensParams, SemanticTokensRangeParams, SemanticTokensRangeResult,
    SemanticTokensResult, SignatureHelp, SignatureHelpParams, TextDocumentContentChangeEvent,
    TextDocumentIdentifier, TextDocumentItem, TextDocumentPositionParams, TextEdit,
    UniquenessLevel, Url, WillSaveTextDocumentParams, WorkDoneProgressParams,
    WorkspaceDiagnosticParams, WorkspaceDiagnosticReportResult, WorkspaceEdit, WorkspaceSymbol,
    WorkspaceSymbolParams, WorkspaceSymbolResponse, notification::PublishDiagnostics,
};
#[cfg(test)]
use async_lsp::lsp_types::{
    CallHierarchyServerCapability, CodeActionProviderCapability, FormattingOptions,
    HoverProviderCapability, OneOf, Range, WorkDoneProgressOptions,
};
use async_lsp::{ClientSocket, LanguageServer, ResponseError};
use musi_tooling::{
    NavigationWorkspace, ToolMonikerKind, completions_for_project_file_with_overlay,
    definition_for_project_file_with_overlay, document_highlights_for_project_file_with_overlay,
    document_links_for_project_file_with_overlay, document_symbols_for_project_file_with_overlay,
    folding_ranges_for_project_file_with_overlay, hover_for_project_file_with_overlay,
    implementation_for_project_file_with_overlay, module_docs_for_project_file_with_overlay,
    moniker_for_project_file_with_overlay, outgoing_calls_for_project_file_with_overlay,
    prepare_rename_for_project_file_with_overlay, references_for_project_file_with_overlay,
    rename_for_project_file_with_overlay, selection_ranges_for_project_file_with_overlay,
    signature_help_for_project_file_with_overlay, type_definition_for_project_file_with_overlay,
};
use serde_json::{Value, json};

mod capabilities;
mod code_actions;
mod config;
mod convert;
mod diagnostics;
mod formatting;
mod hints;
mod navigation;
mod requests;
mod semantic;
mod symbols;
mod workspace;

use config::LspConfig;
#[cfg(test)]
use convert::encode_semantic_tokens;
#[cfg(test)]
use convert::full_document_range;
use convert::{
    resolve_lsp_completion, resolve_lsp_document_link, to_lsp_call_hierarchy_item,
    to_lsp_completion, to_lsp_document_highlight_kind, to_lsp_document_link, to_lsp_folding_range,
    to_lsp_range_in_text, to_lsp_selection_range, to_lsp_signature_help, to_lsp_symbol_kind,
    to_tool_position_in_text, to_tool_range, tool_location_matches_path, truncate_hover_contents,
};
#[cfg(test)]
use formatting::apply_document_formatting_options;
#[cfg(test)]
use musi_fmt::FormatOptions;
use navigation::{
    call_hierarchy_item_data_parts, call_hierarchy_items_match, caller_symbol_for_reference,
    import_definition_at, import_document_highlights, import_linked_editing_ranges,
    import_moniker_at, position_in_lsp_range, push_reference_lens, reference_lens_data_parts,
    reference_lens_title, symbol_at_position,
};
use semantic::SemanticTokenSnapshot;
use workspace::{
    collect_workspace_source_paths, import_specifier_for_target, paths_match, renamed_target_path,
    sort_dedup_paths, workspace_roots,
};

type ServerFuture<T> = Pin<Box<dyn Future<Output = Result<T, ResponseError>> + Send + 'static>>;
type DefinitionFuture = ServerFuture<Option<GotoDefinitionResponse>>;
type TextEditListFuture = ServerFuture<Option<Vec<TextEdit>>>;
type NotifyResult = ControlFlow<async_lsp::Result<()>>;
const REFERENCES_COMMAND: &str = "musi.references";
const SHOW_REFERENCES_COMMAND: &str = "musi.showReferences";

#[derive(Debug)]
pub struct MusiLanguageServer {
    client: ClientSocket,
    open_documents: HashMap<Url, String>,
    semantic_token_cache: HashMap<Url, SemanticTokenSnapshot>,
    format_options_cache: HashMap<PathBuf, formatting::FormatOptionsCacheEntry>,
    navigation_workspace: NavigationWorkspace,
    workspace_roots: Vec<PathBuf>,
    config: LspConfig,
}

impl MusiLanguageServer {
    #[must_use]
    pub fn new(client: ClientSocket) -> Self {
        Self {
            client,
            open_documents: HashMap::new(),
            semantic_token_cache: HashMap::new(),
            format_options_cache: HashMap::new(),
            navigation_workspace: NavigationWorkspace::new(),
            workspace_roots: Vec::new(),
            config: LspConfig::default(),
        }
    }

    fn initialize_result() -> InitializeResult {
        capabilities::initialize_result(REFERENCES_COMMAND)
    }

    fn configure(&mut self, params: &InitializeParams) {
        self.config = LspConfig::from_initialize_params(params);
        self.workspace_roots = workspace_roots(params);
        self.navigation_workspace.clear();
    }

    fn did_open_document(&mut self, item: TextDocumentItem) {
        let uri = item.uri;
        let text = item.text;
        self.invalidate_format_options_cache_if_manifest(&uri);
        self.invalidate_navigation_workspace_for_uri(&uri);
        let _ = self.open_documents.insert(uri, text);
    }

    fn did_change_document(&mut self, uri: &Url, changes: &[TextDocumentContentChangeEvent]) {
        let Some(change) = changes.last() else {
            return;
        };
        self.invalidate_format_options_cache_if_manifest(uri);
        self.invalidate_navigation_workspace_for_uri(uri);
        let _ = self.open_documents.insert(uri.clone(), change.text.clone());
    }

    fn did_close_document(&mut self, uri: &Url) {
        let _ = self.open_documents.remove(uri);
        let _ = self.semantic_token_cache.remove(uri);
        self.invalidate_format_options_cache_if_manifest(uri);
        self.invalidate_navigation_workspace_for_uri(uri);
        drop(
            self.client
                .notify::<PublishDiagnostics>(PublishDiagnosticsParams {
                    uri: uri.clone(),
                    diagnostics: Vec::new(),
                    version: None,
                }),
        );
    }

    fn update_configuration(&mut self, params: &DidChangeConfigurationParams) {
        self.config = LspConfig::from_settings(&params.settings);
    }

    fn update_workspace_folders(&mut self, params: DidChangeWorkspaceFoldersParams) {
        self.format_options_cache.clear();
        self.navigation_workspace.clear();
        for folder in params.event.removed {
            if let Ok(path) = folder.uri.to_file_path() {
                self.workspace_roots
                    .retain(|root| !paths_match(root, &path));
            }
        }
        for folder in params.event.added {
            if let Ok(path) = folder.uri.to_file_path()
                && !self
                    .workspace_roots
                    .iter()
                    .any(|root| paths_match(root, &path))
            {
                self.workspace_roots.push(path);
            }
        }
    }

    fn invalidate_navigation_workspace_for_uri(&mut self, uri: &Url) {
        if let Ok(path) = uri.to_file_path() {
            self.navigation_workspace.invalidate_path(&path);
        }
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
        self.invalidate_format_options_cache_if_manifest(&params.text_document.uri);
        self.invalidate_navigation_workspace_for_uri(&params.text_document.uri);
        ControlFlow::Continue(())
    }

    fn will_save(&mut self, _: WillSaveTextDocumentParams) -> NotifyResult {
        ControlFlow::Continue(())
    }

    fn did_change_configuration(&mut self, params: DidChangeConfigurationParams) -> NotifyResult {
        self.update_configuration(&params);
        ControlFlow::Continue(())
    }

    fn did_change_workspace_folders(
        &mut self,
        params: DidChangeWorkspaceFoldersParams,
    ) -> NotifyResult {
        self.update_workspace_folders(params);
        ControlFlow::Continue(())
    }

    fn will_rename_files(
        &mut self,
        params: RenameFilesParams,
    ) -> ServerFuture<Option<WorkspaceEdit>> {
        let edit = self.will_rename_files_edit(&params);
        Box::pin(async move { Ok(edit) })
    }

    fn will_save_wait_until(&mut self, params: WillSaveTextDocumentParams) -> TextEditListFuture {
        let formatting_response = self.will_save_formatting(params);
        Box::pin(async move { Ok(formatting_response) })
    }

    fn document_diagnostic(
        &mut self,
        params: DocumentDiagnosticParams,
    ) -> ServerFuture<DocumentDiagnosticReportResult> {
        let report = self.document_diagnostics(&params);
        Box::pin(async move { Ok(report) })
    }

    fn workspace_diagnostic(
        &mut self,
        params: WorkspaceDiagnosticParams,
    ) -> ServerFuture<WorkspaceDiagnosticReportResult> {
        let report = self.workspace_diagnostics(params);
        Box::pin(async move { Ok(report) })
    }

    fn completion(&mut self, params: CompletionParams) -> ServerFuture<Option<CompletionResponse>> {
        let completion_response = self.completions(params);
        Box::pin(async move { Ok(completion_response) })
    }

    fn completion_item_resolve(&mut self, params: CompletionItem) -> ServerFuture<CompletionItem> {
        let completion = Self::resolve_completion(params);
        Box::pin(async move { Ok(completion) })
    }

    fn hover(&mut self, params: HoverParams) -> ServerFuture<Option<Hover>> {
        let hover_response = self.hover_at(params);
        Box::pin(async move { Ok(hover_response) })
    }

    fn signature_help(
        &mut self,
        params: SignatureHelpParams,
    ) -> ServerFuture<Option<SignatureHelp>> {
        let signature_help_response = self.signature_help_at(params);
        Box::pin(async move { Ok(signature_help_response) })
    }

    fn definition(&mut self, params: GotoDefinitionParams) -> DefinitionFuture {
        let definition_response = self.definition_at(params);
        Box::pin(async move { Ok(definition_response) })
    }

    fn declaration(&mut self, params: GotoDefinitionParams) -> DefinitionFuture {
        let declaration_response = self.definition_at(params);
        Box::pin(async move { Ok(declaration_response) })
    }

    fn type_definition(&mut self, params: GotoDefinitionParams) -> DefinitionFuture {
        let type_definition_response = self.type_definition_at(params);
        Box::pin(async move { Ok(type_definition_response) })
    }

    fn implementation(&mut self, params: GotoDefinitionParams) -> DefinitionFuture {
        let implementation_response = self.implementation_at(params);
        Box::pin(async move { Ok(implementation_response) })
    }

    fn references(&mut self, params: ReferenceParams) -> ServerFuture<Option<Vec<Location>>> {
        let reference_locations = self.references_at(params);
        Box::pin(async move { Ok(reference_locations) })
    }

    fn moniker(&mut self, params: MonikerParams) -> ServerFuture<Option<Vec<Moniker>>> {
        let monikers = self.monikers_at(params);
        Box::pin(async move { Ok(monikers) })
    }

    fn document_highlight(
        &mut self,
        params: DocumentHighlightParams,
    ) -> ServerFuture<Option<Vec<DocumentHighlight>>> {
        let document_highlights = self.document_highlights(params);
        Box::pin(async move { Ok(document_highlights) })
    }

    fn linked_editing_range(
        &mut self,
        params: LinkedEditingRangeParams,
    ) -> ServerFuture<Option<LinkedEditingRanges>> {
        let linked_ranges = self.linked_editing_ranges(params);
        Box::pin(async move { Ok(linked_ranges) })
    }

    fn document_symbol(
        &mut self,
        params: DocumentSymbolParams,
    ) -> ServerFuture<Option<DocumentSymbolResponse>> {
        let document_symbols = self.document_symbols(params);
        Box::pin(async move { Ok(document_symbols) })
    }

    fn prepare_call_hierarchy(
        &mut self,
        params: CallHierarchyPrepareParams,
    ) -> ServerFuture<Option<Vec<CallHierarchyItem>>> {
        let call_hierarchy_items = self.prepare_call_hierarchy_at(params);
        Box::pin(async move { Ok(call_hierarchy_items) })
    }

    fn incoming_calls(
        &mut self,
        params: CallHierarchyIncomingCallsParams,
    ) -> ServerFuture<Option<Vec<CallHierarchyIncomingCall>>> {
        let incoming_calls = self.call_hierarchy_incoming_calls(&params);
        Box::pin(async move { Ok(incoming_calls) })
    }

    fn outgoing_calls(
        &mut self,
        params: CallHierarchyOutgoingCallsParams,
    ) -> ServerFuture<Option<Vec<CallHierarchyOutgoingCall>>> {
        let outgoing_calls = self.call_hierarchy_outgoing_calls(&params);
        Box::pin(async move { Ok(outgoing_calls) })
    }

    fn document_link(
        &mut self,
        params: DocumentLinkParams,
    ) -> ServerFuture<Option<Vec<DocumentLink>>> {
        let document_links = self.document_links(params);
        Box::pin(async move { Ok(document_links) })
    }

    fn document_link_resolve(&mut self, params: DocumentLink) -> ServerFuture<DocumentLink> {
        let document_link = Self::resolve_document_link(params);
        Box::pin(async move { Ok(document_link) })
    }

    fn code_lens(&mut self, params: CodeLensParams) -> ServerFuture<Option<Vec<CodeLens>>> {
        let code_lenses = self.code_lenses(params);
        Box::pin(async move { Ok(code_lenses) })
    }

    fn code_lens_resolve(&mut self, params: CodeLens) -> ServerFuture<CodeLens> {
        let code_lens = self.resolve_code_lens(params);
        Box::pin(async move { Ok(code_lens) })
    }

    fn execute_command(&mut self, params: ExecuteCommandParams) -> ServerFuture<Option<Value>> {
        let command_response = self.execute_command_request(&params);
        Box::pin(async move { Ok(command_response) })
    }

    fn code_action(
        &mut self,
        params: CodeActionParams,
    ) -> ServerFuture<Option<CodeActionResponse>> {
        let code_actions = self.code_actions(params);
        Box::pin(async move { Ok(code_actions) })
    }

    fn code_action_resolve(&mut self, params: CodeAction) -> ServerFuture<CodeAction> {
        let code_action = self.resolve_code_action(params);
        Box::pin(async move { Ok(code_action) })
    }

    fn folding_range(
        &mut self,
        params: FoldingRangeParams,
    ) -> ServerFuture<Option<Vec<FoldingRange>>> {
        let folding_ranges = self.folding_ranges(params);
        Box::pin(async move { Ok(folding_ranges) })
    }

    fn selection_range(
        &mut self,
        params: SelectionRangeParams,
    ) -> ServerFuture<Option<Vec<SelectionRange>>> {
        let selection_ranges = self.selection_ranges(params);
        Box::pin(async move { Ok(selection_ranges) })
    }

    fn symbol(
        &mut self,
        params: WorkspaceSymbolParams,
    ) -> ServerFuture<Option<WorkspaceSymbolResponse>> {
        let workspace_symbols = self.workspace_symbols(&params);
        Box::pin(async move { Ok(Some(workspace_symbols)) })
    }

    fn workspace_symbol_resolve(
        &mut self,
        params: WorkspaceSymbol,
    ) -> ServerFuture<WorkspaceSymbol> {
        let workspace_symbol = Self::resolve_workspace_symbol(params);
        Box::pin(async move { Ok(workspace_symbol) })
    }

    fn prepare_rename(
        &mut self,
        params: TextDocumentPositionParams,
    ) -> ServerFuture<Option<PrepareRenameResponse>> {
        let prepare_rename_response = self.prepare_rename_at(params);
        Box::pin(async move { Ok(prepare_rename_response) })
    }

    fn rename(&mut self, params: RenameParams) -> ServerFuture<Option<WorkspaceEdit>> {
        let rename_response = self.rename_at(params);
        Box::pin(async move { Ok(rename_response) })
    }

    fn formatting(&mut self, params: DocumentFormattingParams) -> TextEditListFuture {
        let formatting_response = self.document_formatting(params);
        Box::pin(async move { Ok(formatting_response) })
    }

    fn range_formatting(&mut self, params: DocumentRangeFormattingParams) -> TextEditListFuture {
        let formatting_response = self.document_range_formatting(params);
        Box::pin(async move { Ok(formatting_response) })
    }

    fn on_type_formatting(&mut self, params: DocumentOnTypeFormattingParams) -> TextEditListFuture {
        let formatting_response = self.document_on_type_formatting(params);
        Box::pin(async move { Ok(formatting_response) })
    }

    fn semantic_tokens_full(
        &mut self,
        params: SemanticTokensParams,
    ) -> ServerFuture<Option<SemanticTokensResult>> {
        let semantic_tokens_response = self
            .semantic_tokens_full_response(&params)
            .map(SemanticTokensResult::Tokens);
        Box::pin(async move { Ok(semantic_tokens_response) })
    }

    fn semantic_tokens_full_delta(
        &mut self,
        params: SemanticTokensDeltaParams,
    ) -> ServerFuture<Option<SemanticTokensFullDeltaResult>> {
        let semantic_tokens_response = self.semantic_token_delta(&params);
        Box::pin(async move { Ok(semantic_tokens_response) })
    }

    fn semantic_tokens_range(
        &mut self,
        params: SemanticTokensRangeParams,
    ) -> ServerFuture<Option<SemanticTokensRangeResult>> {
        let semantic_range_response = self
            .semantic_range_tokens(&params)
            .map(SemanticTokensRangeResult::Tokens);
        Box::pin(async move { Ok(semantic_range_response) })
    }

    fn inlay_hint(&mut self, params: InlayHintParams) -> ServerFuture<Option<Vec<InlayHint>>> {
        let inlay_hints_response = self.inlay_hints(&params);
        Box::pin(async move { Ok(inlay_hints_response) })
    }

    fn inlay_hint_resolve(&mut self, params: InlayHint) -> ServerFuture<InlayHint> {
        let hint = Self::resolve_inlay_hint(params);
        Box::pin(async move { Ok(hint) })
    }
}

#[cfg(test)]
mod tests;
