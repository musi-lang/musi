use std::collections::HashMap;

use msc_ast::ParsedModule;
use msc_lex::LexedSource;
use msc_sema::SemaResult;
use msc_shared::{FileId, Interner, SourceDb, Span, Symbol};

use crate::types::{DepSourceMap, ResolvedImports, SpanIndex};

pub type LspSemaOutput = (SemaResult, ParsedModule, FileId, LexedSource, DepSourceMap);

/// Lexed artifacts for a single imported stdlib module, stored for doc-comment lookup.
pub struct DepSource {
    pub source: String,
    /// Top-level named definitions: interned name -> def span.
    pub def_spans: HashMap<Symbol, Span>,
}

/// All artifacts produced by analyzing a single open document.
pub struct AnalyzedDoc {
    pub source: String,
    pub module: ParsedModule,
    pub interner: Interner,
    pub source_db: SourceDb,
    pub file_id: FileId,
    pub lexed: LexedSource,
    pub sema: Option<SemaResult>,
    pub dep_sources: DepSourceMap,
    /// Maps import path `Symbol` → resolved filesystem `PathBuf`.
    pub resolved_imports: ResolvedImports,
    /// Sorted span index: (start, end, DefId) for O(log n) offset→DefId lookups.
    pub span_index: SpanIndex,
}
