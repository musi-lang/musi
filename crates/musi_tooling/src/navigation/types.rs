use std::collections::HashMap;
use std::path::PathBuf;

use crate::analysis::{ToolRange, ToolSymbolKind};

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolLocation {
    pub path: PathBuf,
    pub range: ToolRange,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolTextEdit {
    pub range: ToolRange,
    pub new_text: String,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolWorkspaceEdit {
    pub changes: HashMap<PathBuf, Vec<ToolTextEdit>>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolDocumentSymbol {
    pub name: String,
    pub kind: ToolSymbolKind,
    pub range: ToolRange,
    pub selection_range: ToolRange,
    pub children: Vec<Self>,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolReferenceLens {
    pub range: ToolRange,
    pub reference_count: usize,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolDocumentHighlightKind {
    Read,
    Text,
    Write,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolDocumentHighlight {
    pub location: ToolLocation,
    pub kind: ToolDocumentHighlightKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolWorkspaceSymbol {
    pub name: String,
    pub kind: ToolSymbolKind,
    pub location: ToolLocation,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ToolMonikerKind {
    Import,
    Local,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolMoniker {
    pub location: ToolLocation,
    pub kind: ToolMonikerKind,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolCallHierarchyItem {
    pub name: String,
    pub kind: ToolSymbolKind,
    pub location: ToolLocation,
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ToolOutgoingCall {
    pub to: ToolCallHierarchyItem,
    pub from_ranges: Vec<ToolRange>,
}
