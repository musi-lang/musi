//! Inlay hint helpers for the LSP server.

use std::fs::read_to_string;

use async_lsp::lsp_types::{InlayHint, InlayHintParams};
use musi_tooling::inlay_hints_for_project_file_with_overlay;

use super::MusiLanguageServer;
use super::convert::{position_in_range, resolve_lsp_inlay_hint, to_lsp_inlay_hint};

impl MusiLanguageServer {
    pub(super) fn inlay_hints(&self, params: &InlayHintParams) -> Option<Vec<InlayHint>> {
        if !self.config.inlay_hints.enabled {
            return Some(Vec::new());
        }
        let uri = &params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let file_text;
        let text = if let Some(text) = self.open_documents.get(uri) {
            text.as_str()
        } else {
            file_text = read_to_string(&path).ok()?;
            file_text.as_str()
        };
        let hints = inlay_hints_for_project_file_with_overlay(&path, Some(text))
            .into_iter()
            .filter(|hint| self.config.inlay_hints.allows(hint))
            .filter(|hint| position_in_range(text, hint.position, params.range))
            .map(|hint| to_lsp_inlay_hint(text, hint))
            .collect();
        Some(hints)
    }

    pub(super) fn resolve_inlay_hint(hint: InlayHint) -> InlayHint {
        resolve_lsp_inlay_hint(hint)
    }
}
