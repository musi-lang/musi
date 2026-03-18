use std::collections::HashMap;
use std::path::PathBuf;

use music_resolve::graph::ModuleId;
use music_sema::{DefId, ExportBinding};
use music_shared::Symbol;

use crate::analysis::DepSource;

/// Sorted span index: `(start, end, DefId)` for O(log n) offset→def lookups.
pub type SpanIndex = Vec<(u32, u32, DefId)>;

/// Maps dependency module key → lexed source + def spans.
pub type DepSourceMap = HashMap<String, DepSource>;

/// Maps import path symbol → resolved filesystem path.
pub type ResolvedImports = HashMap<Symbol, PathBuf>;

/// Maps module ID → its exported bindings.
pub type ModuleExportMap = HashMap<ModuleId, Vec<ExportBinding>>;
