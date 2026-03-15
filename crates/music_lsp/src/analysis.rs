//! Centralized analysis pipeline for the Musi LSP.
//!
//! All document handlers call `analyze_doc()` once; results are stored in
//! `AnalyzedDoc` so that semantic-token, hover, goto-def, and reference
//! handlers can reuse the same sema pass without re-running it.

use std::collections::HashMap;
use std::path::Path;

use music_ast::{ExprIdx, ParsedModule};
use music_lex::{LexedSource, Token, TokenKind, lex};
use music_parse::parse;
use music_resolve::graph::ModuleId;
use music_resolve::{ModuleGraph, ModuleNode, ResolverConfig, build_module_graph};
use music_sema::{
    DefInfo, ExportBinding, ImportNames, ModuleSemaOutput, SemaResult, SharedAnalysisState, Type,
    TypeIdx, analyze, collect_exports,
};
use music_sema::types::RecordField;
use music_shared::{DiagnosticBag, FileId, Interner, Severity, SourceDb, Span, Symbol};
use lsp_types::{Diagnostic, DiagnosticSeverity, DiagnosticTag, Position, Range};

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
    pub dep_sources: HashMap<String, DepSource>,
}

fn parse_src(
    label: &str,
    src: &str,
    interner: &mut Interner,
    source_db: &mut SourceDb,
    diags: &mut DiagnosticBag,
) -> (FileId, ParsedModule, LexedSource) {
    let file_id = source_db.add(label, src);
    let lexed = lex(src, file_id, interner, diags);
    let module = parse(&lexed.tokens, file_id, diags, interner);
    (file_id, module, lexed)
}

/// Run the full pipeline on `source` and return (diagnostics, analyzed doc).
pub fn analyze_doc(source: &str, _uri: &str) -> (Vec<Diagnostic>, AnalyzedDoc) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let (file_id, module, lexed) = parse_src(
        "<document>",
        source,
        &mut interner,
        &mut source_db,
        &mut diags,
    );

    let sema = analyze(&module, &mut interner, file_id, &mut diags);
    let lsp_diags = to_lsp_diags(
        diags.iter().filter(|d| d.primary.file_id == file_id),
        &source_db,
    );

    let doc = AnalyzedDoc {
        source: source.to_owned(),
        module,
        interner,
        source_db,
        file_id,
        lexed,
        sema: Some(sema),
        dep_sources: HashMap::new(),
    };
    (lsp_diags, doc)
}

/// Run the multi-file pipeline: build module graph, toposort, run sema in
/// dependency order. Falls back to single-file `analyze_doc()` on failure.
pub fn analyze_doc_multi(
    source: &str,
    file_path: &Path,
    project_root: &Path,
) -> (Vec<Diagnostic>, AnalyzedDoc) {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();
    let mut parsed_modules: HashMap<ModuleId, ParsedModule> = HashMap::new();

    let config = ResolverConfig::from_project(project_root, None);

    let graph = match build_module_graph(
        file_path,
        &config,
        &mut interner,
        &mut source_db,
        &mut diags,
        &mut parsed_modules,
        Some(source),
    ) {
        Ok(g) => g,
        Err(_) => return analyze_doc(source, "<document>"),
    };

    let order = match graph.toposort() {
        Ok(o) => o,
        Err(_) => return analyze_doc(source, "<document>"),
    };

    let (sema, entry_parsed, entry_file_id, lexed) = match run_lsp_sema_in_order(
        &graph,
        &order,
        &mut parsed_modules,
        &mut interner,
        &mut diags,
        source,
    ) {
        Some(result) => result,
        None => return analyze_doc(source, "<document>"),
    };

    let lsp_diags = to_lsp_diags(
        diags.iter().filter(|d| d.primary.file_id == entry_file_id),
        &source_db,
    );

    let doc = AnalyzedDoc {
        source: source.to_owned(),
        module: entry_parsed,
        interner,
        source_db,
        file_id: entry_file_id,
        lexed,
        sema: Some(sema),
        dep_sources: HashMap::new(),
    };
    (lsp_diags, doc)
}

fn run_lsp_sema_in_order(
    graph: &ModuleGraph,
    order: &[ModuleId],
    parsed_modules: &mut HashMap<ModuleId, ParsedModule>,
    interner: &mut Interner,
    diags: &mut DiagnosticBag,
    entry_source: &str,
) -> Option<(SemaResult, ParsedModule, FileId, LexedSource)> {
    let mut state = SharedAnalysisState::new(interner);
    let mut module_exports: HashMap<ModuleId, Vec<ExportBinding>> = HashMap::new();

    let entry_id = ModuleId(0);
    let entry_file_id = graph.get(entry_id).file_id;
    let mut entry_output: Option<(ModuleSemaOutput, ParsedModule)> = None;

    for &module_id in order {
        let node = graph.get(module_id);
        let file_id = node.file_id;

        // Built-in modules have no source; inject their exports directly.
        if node.builtin {
            let exports = builtin_module_exports(node, &state);
            let _prev = module_exports.insert(module_id, exports);
            continue;
        }

        let import_names = build_import_names(node, &module_exports);
        let import_types = build_import_types(node, &module_exports, &mut state.types);

        let Some(parsed) = parsed_modules.remove(&module_id) else {
            continue;
        };

        let output = music_sema::analyze_shared(
            &parsed,
            &mut state,
            interner,
            file_id,
            diags,
            &import_names,
            &import_types,
        );

        if module_id == entry_id {
            entry_output = Some((output, parsed));
        } else {
            let defs_vec: Vec<_> = state.defs.iter().cloned().collect();
            let exports = collect_exports(&parsed, &defs_vec, &output.resolution.pat_defs);
            let _prev = module_exports.insert(module_id, exports.bindings);
        }
    }

    let (output, parsed) = entry_output?;
    let sema = state.into_sema_result(output);

    // Re-lex the entry file for token data used by LSP features (semantic
    // tokens, find-name, etc.). The interner already has all symbols from
    // the graph-build phase, so this is cheap.
    let lexed = lex(entry_source, entry_file_id, interner, diags);

    Some((sema, parsed, entry_file_id, lexed))
}

fn builtin_module_exports(
    node: &ModuleNode,
    state: &SharedAnalysisState,
) -> Vec<ExportBinding> {
    let path_str = node.path.to_string_lossy();
    if path_str == "<musi:ffi>" {
        let wk = &state.well_known;
        let c_string_def = state.defs.get(wk.ffi.c_string);
        let ptr_def = state.defs.get(wk.ffi.ptr);
        vec![
            ExportBinding {
                name: c_string_def.name,
                ty: TypeIdx::from_raw(0),
                def_id: wk.ffi.c_string,
            },
            ExportBinding {
                name: ptr_def.name,
                ty: TypeIdx::from_raw(0),
                def_id: wk.ffi.ptr,
            },
        ]
    } else if path_str == "<musi:core>" {
        let core = &state.well_known.core;
        let make = |did| {
            let def = state.defs.get(did);
            ExportBinding {
                name: def.name,
                ty: TypeIdx::from_raw(0),
                def_id: did,
            }
        };
        vec![
            make(core.int_abs),
            make(core.int_min),
            make(core.int_max),
            make(core.int_clamp),
            make(core.int_pow),
            make(core.str_len),
            make(core.str_contains),
            make(core.str_starts_with),
            make(core.str_ends_with),
            make(core.arr_len),
            make(core.arr_push),
            make(core.arr_pop),
            make(core.arr_reverse),
        ]
    } else {
        vec![]
    }
}

fn build_import_names(
    node: &ModuleNode,
    module_exports: &HashMap<ModuleId, Vec<ExportBinding>>,
) -> ImportNames {
    let mut import_names = ImportNames::new();
    for &(dep_id, import_sym) in &node.imports {
        if let Some(exports) = module_exports.get(&dep_id) {
            let names: Vec<_> = exports.iter().map(|eb| (eb.name, eb.def_id)).collect();
            let _prev = import_names.insert(import_sym, names);
        }
    }
    import_names
}

fn build_import_types(
    node: &ModuleNode,
    module_exports: &HashMap<ModuleId, Vec<ExportBinding>>,
    types: &mut music_shared::Arena<Type>,
) -> HashMap<Symbol, TypeIdx> {
    let mut map = HashMap::new();
    for &(dep_id, import_sym) in &node.imports {
        if let Some(exports) = module_exports.get(&dep_id) {
            let fields: Vec<_> = exports
                .iter()
                .map(|b| RecordField {
                    name: b.name,
                    ty: b.ty,
                })
                .collect();
            let ty = types.alloc(Type::Record {
                fields,
                open: false,
            });
            let _prev = map.insert(import_sym, ty);
        }
    }
    map
}

/// Returns the span of any expression node, or `None` if the index is out of bounds.
pub fn expr_span(idx: ExprIdx, module: &ParsedModule) -> Option<Span> {
    if idx.raw() as usize >= module.arenas.exprs.len() {
        return None;
    }
    use music_ast::Expr;
    let span = match &module.arenas.exprs[idx] {
        Expr::Lit { span, .. }
        | Expr::Name { span, .. }
        | Expr::Paren { span, .. }
        | Expr::Tuple { span, .. }
        | Expr::Block { span, .. }
        | Expr::Let { span, .. }
        | Expr::Fn { span, .. }
        | Expr::Call { span, .. }
        | Expr::Field { span, .. }
        | Expr::Index { span, .. }
        | Expr::Update { span, .. }
        | Expr::Record { span, .. }
        | Expr::Array { span, .. }
        | Expr::Variant { span, .. }
        | Expr::Choice { span, .. }
        | Expr::RecordDef { span, .. }
        | Expr::BinOp { span, .. }
        | Expr::UnaryOp { span, .. }
        | Expr::Piecewise { span, .. }
        | Expr::Match { span, .. }
        | Expr::Return { span, .. }
        | Expr::Import { span, .. }
        | Expr::Export { span, .. }
        | Expr::Annotated { span, .. }
        | Expr::Binding { span, .. }
        | Expr::Class { span, .. }
        | Expr::Instance { span, .. }
        | Expr::Effect { span, .. }
        | Expr::Foreign { span, .. }
        | Expr::TypeCheck { span, .. }
        | Expr::Handle { span, .. }
        | Expr::Error { span, .. } => *span,
    };
    Some(span)
}

/// Convert a 0-based (line, character) LSP position to a byte offset in `source`.
pub fn position_to_offset(source: &str, line: u32, character: u32) -> u32 {
    let mut cur_line = 0u32;
    let mut line_start = 0usize;
    for (i, ch) in source.char_indices() {
        if cur_line == line {
            break;
        }
        if ch == '\n' {
            cur_line += 1;
            line_start = i + 1;
        }
    }
    u32::try_from(line_start + character as usize).unwrap_or(u32::MAX)
}

/// Convert a byte offset to an LSP `Position`.
pub fn offset_to_position(file_id: FileId, offset: u32, source_db: &SourceDb) -> Position {
    let src_len = u32::try_from(source_db.source(file_id).len()).unwrap_or(u32::MAX);
    let clamped = offset.min(src_len);
    let (line1, col1) = source_db.lookup(file_id, clamped);
    Position {
        line: line1 - 1,
        character: col1 - 1,
    }
}

/// Convert a `Span` to an LSP `Range`.
pub fn span_to_range(file_id: FileId, span: Span, source_db: &SourceDb) -> Range {
    Range {
        start: offset_to_position(file_id, span.start, source_db),
        end: offset_to_position(file_id, span.end(), source_db),
    }
}

/// Extract doc-comment text for the definition at `def_start` by scanning
/// source lines directly. Walks backward from the line containing `def_start`,
/// collecting consecutive `///` lines.
pub fn extract_doc_comments_from_source(def_start: u32, source: &str) -> String {
    let prefix = source.get(..def_start as usize).unwrap_or("");
    let def_line_start = prefix.rfind('\n').map_or(0, |i| i + 1);

    let mut doc_lines: Vec<&str> = Vec::new();
    let mut pos = def_line_start;

    loop {
        if pos == 0 {
            break;
        }
        let prev_end = pos - 1;
        let before = source.get(..prev_end).unwrap_or("");
        let prev_start = before.rfind('\n').map_or(0, |i| i + 1);
        let line = source.get(prev_start..prev_end).unwrap_or("");
        let trimmed = line.trim_start();

        if let Some(rest) = trimmed.strip_prefix("///") {
            let content = rest.strip_prefix(' ').unwrap_or(rest);
            doc_lines.push(content);
            pos = prev_start;
        } else if trimmed.starts_with("#[") || trimmed.is_empty() {
            // Skip attribute lines and blank lines between doc comments and the definition.
            pos = prev_start;
        } else {
            break;
        }
    }

    doc_lines.reverse();
    doc_lines.join("\n")
}

pub(crate) fn to_lsp_diags<'a>(
    iter: impl Iterator<Item = &'a music_shared::Diagnostic>,
    source_db: &SourceDb,
) -> Vec<Diagnostic> {
    iter.map(|d| {
        let start = offset_to_position(d.primary.file_id, d.primary.span.start, source_db);
        let end = offset_to_position(d.primary.file_id, d.primary.span.end(), source_db);
        let tags = if d.severity == Severity::Warning && d.message.starts_with("unused") {
            Some(vec![DiagnosticTag::UNNECESSARY])
        } else {
            None
        };
        Diagnostic {
            range: Range { start, end },
            severity: Some(severity_to_lsp(d.severity)),
            message: d.message.to_string(),
            source: Some("musi".to_owned()),
            tags,
            ..Diagnostic::default()
        }
    })
    .collect()
}

fn severity_to_lsp(s: Severity) -> DiagnosticSeverity {
    match s {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::INFORMATION,
    }
}

/// Find the Ident token at or after `start` whose interned symbol equals `name`.
pub fn find_name_token(tokens: &[Token], start: u32, name: Symbol) -> Option<Span> {
    let idx = tokens.partition_point(|t| t.span.start < start);
    for tok in tokens[idx..].iter().take(64) {
        if tok.span.start > start.saturating_add(200) {
            break;
        }
        if tok.kind == TokenKind::Ident && tok.symbol == Some(name) {
            return Some(tok.span);
        }
    }
    None
}

/// The span of a definition's name token (narrower than the full `def.span`).
pub fn def_name_span(def: &DefInfo, tokens: &[Token]) -> Span {
    find_name_token(tokens, def.span.start, def.name).unwrap_or(def.span)
}

/// Find the definition whose reference or declaration site contains `offset`.
pub fn def_at_cursor(offset: u32, doc: &AnalyzedDoc) -> Option<&DefInfo> {
    let sema = doc.sema.as_ref()?;

    // 1. expr_defs (reference sites)
    if let Some(def_id) = sema
        .resolution
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            let span = expr_span(idx, &doc.module)?;
            if span.start <= offset && offset <= span.end() {
                Some((def_id, span.length))
            } else {
                None
            }
        })
        .min_by_key(|&(_, len)| len)
        .map(|(def_id, _)| def_id)
    {
        return sema.defs.get(def_id.0 as usize);
    }

    // 2. pat_defs (binding sites)
    if let Some(def_id) = sema
        .resolution
        .pat_defs
        .iter()
        .filter_map(|(span, &def_id)| {
            if span.start <= offset && offset <= span.end() {
                Some((def_id, span.length))
            } else {
                None
            }
        })
        .min_by_key(|&(_, len)| len)
        .map(|(def_id, _)| def_id)
    {
        return sema.defs.get(def_id.0 as usize);
    }

    // 3. Definition name tokens
    sema.defs.iter().find(|def| {
        if def.span == Span::DUMMY {
            return false;
        }
        let name_span =
            find_name_token(&doc.lexed.tokens, def.span.start, def.name).unwrap_or(def.span);
        name_span.start <= offset && offset <= name_span.end()
    })
}

fn _build_dep_source(source: &str, _lexed: LexedSource, sema: &SemaResult) -> DepSource {
    let mut def_spans: HashMap<Symbol, Span> = HashMap::new();
    for def in &sema.defs {
        let _prev = def_spans.entry(def.name).or_insert(def.span);
    }
    DepSource {
        source: source.to_owned(),
        def_spans,
    }
}
