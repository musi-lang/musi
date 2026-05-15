//! Code action helpers for the LSP server.

use std::collections::HashMap;

use async_lsp::lsp_types::{
    CodeAction, CodeActionKind, CodeActionOrCommand, CodeActionParams, CodeActionResponse,
    TextEdit, Url, WorkspaceEdit,
};
use musi_fmt::organize_imports;
use serde_json::{Value, json};

use super::MusiLanguageServer;
use super::convert::full_document_range;
use super::navigation::code_action_kind_requested;

impl MusiLanguageServer {
    pub(super) fn code_actions(&self, params: CodeActionParams) -> Option<CodeActionResponse> {
        if !code_action_kind_requested(
            params.context.only.as_deref(),
            &CodeActionKind::SOURCE_ORGANIZE_IMPORTS,
        ) {
            return Some(Vec::new());
        }
        let uri = params.text_document.uri;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        if self.organize_imports_edit(&uri).is_none() {
            return Some(Vec::new());
        }
        Some(vec![CodeActionOrCommand::CodeAction(CodeAction {
            title: "Organize imports".to_owned(),
            kind: Some(CodeActionKind::SOURCE_ORGANIZE_IMPORTS),
            diagnostics: None,
            edit: None,
            command: None,
            is_preferred: Some(true),
            disabled: None,
            data: Some(json!({
                "uri": uri.as_str(),
            })),
        })])
    }

    pub(super) fn resolve_code_action(&self, mut action: CodeAction) -> CodeAction {
        if action.edit.is_some() {
            return action;
        }
        if action.kind.as_ref() != Some(&CodeActionKind::SOURCE_ORGANIZE_IMPORTS) {
            return action;
        }
        let Some(uri) = action
            .data
            .as_ref()
            .and_then(|data| data.get("uri"))
            .and_then(Value::as_str)
            .and_then(|uri| Url::parse(uri).ok())
        else {
            return action;
        };
        action.edit = self.organize_imports_edit(&uri);
        action
    }

    fn organize_imports_edit(&self, uri: &Url) -> Option<WorkspaceEdit> {
        let text = self.open_documents.get(uri)?;
        let path = uri.to_file_path().ok()?;
        if path.file_name().is_some_and(|name| name == "musi.json") {
            return None;
        }
        let organized = organize_imports(text)?;
        Some(WorkspaceEdit {
            changes: Some(HashMap::from([(
                uri.clone(),
                vec![TextEdit::new(full_document_range(text), organized)],
            )])),
            document_changes: None,
            change_annotations: None,
        })
    }
}
