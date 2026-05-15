//! Diagnostic report helpers for the LSP server.

use std::path::{Path, PathBuf};

use async_lsp::lsp_types::{
    Diagnostic, DocumentDiagnosticParams, DocumentDiagnosticReport, DocumentDiagnosticReportResult,
    FullDocumentDiagnosticReport, RelatedFullDocumentDiagnosticReport, Url,
    WorkspaceDiagnosticParams, WorkspaceDiagnosticReport, WorkspaceDiagnosticReportResult,
    WorkspaceDocumentDiagnosticReport, WorkspaceFullDocumentDiagnosticReport,
};
use musi_tooling::collect_project_diagnostics_with_overlay;

use super::MusiLanguageServer;
use super::convert::{diagnostic_matches_path, to_lsp_diagnostic};
use super::workspace::{paths_match, sort_dedup_paths, workspace_module_paths};

impl MusiLanguageServer {
    pub(super) fn document_diagnostics(
        &self,
        params: &DocumentDiagnosticParams,
    ) -> DocumentDiagnosticReportResult {
        let Some(path) = params.text_document.uri.to_file_path().ok() else {
            return full_document_diagnostic_report(Vec::new());
        };
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return full_document_diagnostic_report(Vec::new());
        }
        let overlay = self
            .open_documents
            .get(&params.text_document.uri)
            .map(String::as_str);
        let diagnostics = collect_project_diagnostics_with_overlay(&path, overlay)
            .into_iter()
            .filter(|diag| diagnostic_matches_path(&path, diag))
            .map(to_lsp_diagnostic)
            .collect();
        full_document_diagnostic_report(diagnostics)
    }

    pub(super) fn workspace_diagnostics(
        &self,
        _params: WorkspaceDiagnosticParams,
    ) -> WorkspaceDiagnosticReportResult {
        let paths = self.workspace_diagnostic_paths();
        let items = paths
            .into_iter()
            .filter_map(|path| {
                if path.file_name().is_some_and(|name| name == "musi.json") {
                    return None;
                }
                let open_document = self.open_document_for_path(&path);
                let uri = open_document.map_or_else(
                    || Url::from_file_path(&path).ok(),
                    |(uri, _)| Some(uri.clone()),
                )?;
                let overlay = open_document.map(|(_, text)| text);
                let diagnostics = collect_project_diagnostics_with_overlay(&path, overlay)
                    .into_iter()
                    .filter(|diag| diagnostic_matches_path(&path, diag))
                    .map(to_lsp_diagnostic)
                    .collect();
                Some(WorkspaceDocumentDiagnosticReport::Full(
                    WorkspaceFullDocumentDiagnosticReport {
                        uri,
                        version: None,
                        full_document_diagnostic_report: FullDocumentDiagnosticReport {
                            result_id: None,
                            items: diagnostics,
                        },
                    },
                ))
            })
            .collect();
        WorkspaceDiagnosticReportResult::Report(WorkspaceDiagnosticReport { items })
    }

    pub(super) fn workspace_diagnostic_paths(&self) -> Vec<PathBuf> {
        let mut paths = self
            .workspace_query_roots()
            .iter()
            .flat_map(|root| workspace_module_paths(root))
            .collect::<Vec<_>>();
        paths.extend(
            self.open_documents
                .keys()
                .filter_map(|uri| uri.to_file_path().ok()),
        );
        sort_dedup_paths(paths)
    }

    pub(super) fn open_document_for_path(&self, path: &Path) -> Option<(&Url, &str)> {
        self.open_documents.iter().find_map(|(uri, text)| {
            let open_path = uri.to_file_path().ok()?;
            paths_match(&open_path, path).then_some((uri, text.as_str()))
        })
    }
}

const fn full_document_diagnostic_report(
    diagnostics: Vec<Diagnostic>,
) -> DocumentDiagnosticReportResult {
    DocumentDiagnosticReportResult::Report(DocumentDiagnosticReport::Full(
        RelatedFullDocumentDiagnosticReport {
            related_documents: None,
            full_document_diagnostic_report: FullDocumentDiagnosticReport {
                result_id: None,
                items: diagnostics,
            },
        },
    ))
}
