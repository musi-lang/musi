//! Document and workspace symbol helpers for the LSP server.

use async_lsp::lsp_types::{
    DocumentSymbolParams, DocumentSymbolResponse, WorkspaceSymbol, WorkspaceSymbolParams,
    WorkspaceSymbolResponse,
};
use musi_tooling::{
    ToolSymbolKind, document_symbols_for_project_file_with_overlay,
    workspace_symbols_for_project_file_with_overlay, workspace_symbols_for_project_root,
};

use super::MusiLanguageServer;
use super::convert::{
    resolve_lsp_workspace_symbol, to_lsp_document_symbol, to_lsp_workspace_symbol,
};
use super::workspace::paths_match;

impl MusiLanguageServer {
    pub(super) fn document_symbols(
        &self,
        params: DocumentSymbolParams,
    ) -> Option<DocumentSymbolResponse> {
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let overlay = self.open_documents.get(&uri).map(String::as_str);
        let symbols = document_symbols_for_project_file_with_overlay(&path, overlay)
            .into_iter()
            .map(to_lsp_document_symbol)
            .collect();
        Some(DocumentSymbolResponse::Nested(symbols))
    }

    pub(super) fn workspace_symbols(
        &self,
        params: &WorkspaceSymbolParams,
    ) -> WorkspaceSymbolResponse {
        let open_paths = self
            .open_documents
            .keys()
            .filter_map(|uri| uri.to_file_path().ok())
            .collect::<Vec<_>>();
        let mut symbols = self
            .workspace_query_roots()
            .iter()
            .flat_map(|root| workspace_symbols_for_project_root(root, &params.query))
            .filter(|symbol| {
                symbol.kind == ToolSymbolKind::Module
                    || !open_paths
                        .iter()
                        .any(|path| paths_match(path, &symbol.location.path))
            })
            .collect::<Vec<_>>();
        symbols.extend(
            self.open_documents
                .iter()
                .filter_map(|(uri, text)| {
                    let path = uri.to_file_path().ok()?;
                    Some(workspace_symbols_for_project_file_with_overlay(
                        &path,
                        Some(text),
                        &params.query,
                    ))
                })
                .flatten(),
        );
        symbols.sort_by_key(|symbol| {
            (
                symbol.name.clone(),
                symbol.location.path.clone(),
                symbol.location.range.start_line,
                symbol.location.range.start_col,
            )
        });
        symbols.dedup_by_key(|symbol| {
            (
                symbol.name.clone(),
                symbol.location.path.clone(),
                symbol.location.range.start_line,
                symbol.location.range.start_col,
            )
        });
        let symbols = symbols
            .into_iter()
            .filter_map(to_lsp_workspace_symbol)
            .collect();
        WorkspaceSymbolResponse::Nested(symbols)
    }

    pub(super) fn resolve_workspace_symbol(symbol: WorkspaceSymbol) -> WorkspaceSymbol {
        resolve_lsp_workspace_symbol(symbol)
    }
}
