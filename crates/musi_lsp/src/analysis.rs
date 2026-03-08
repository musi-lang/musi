//! Centralized analysis pipeline for the Musi LSP.
//!
//! All document handlers call `analyze_doc()` once; results are stored in
//! `AnalyzedDoc` so that semantic-token, hover, goto-def, and reference
//! handlers can reuse the same sema pass without re-running it.

use std::collections::{HashMap, HashSet};

use musi_lex::{LexedSource, Token, TokenKind, Trivia, lex};
use musi_parse::{ParsedModule, ast::Expr, parse};
use musi_sema::{DefInfo, ModuleExports, SemaResult, analyze, exports_of};
use musi_shared::{DiagnosticBag, FileId, Idx, Interner, Severity, Span, SourceDb, Symbol};
use tower_lsp_server::ls_types::{
    Diagnostic, DiagnosticSeverity, DiagnosticTag, Position, Range,
};

const PRELUDE_SRC: &str = include_str!("../../../std/prelude.ms");

/// Embedded std library sources available without filesystem access.
fn embedded_std(path: &str) -> Option<&'static str> {
    match path {
        "std/assert" => Some(include_str!("../../../std/assert.ms")),
        "std/math" => Some(include_str!("../../../std/math.ms")),
        "std/string" => Some(include_str!("../../../std/string.ms")),
        "std/core/bool" => Some(include_str!("../../../std/core/bool.ms")),
        "std/core/option" => Some(include_str!("../../../std/core/option.ms")),
        "std/core/ordering" => Some(include_str!("../../../std/core/ordering.ms")),
        "std/core/pair" => Some(include_str!("../../../std/core/pair.ms")),
        "std/core/result" => Some(include_str!("../../../std/core/result.ms")),
        "std/collections/array" => Some(include_str!("../../../std/collections/array.ms")),
        "std/collections/list" => Some(include_str!("../../../std/collections/list.ms")),
        "std/collections/map" => Some(include_str!("../../../std/collections/map.ms")),
        "std/collections/set" => Some(include_str!("../../../std/collections/set.ms")),
        "std/collections/queue" => Some(include_str!("../../../std/collections/queue.ms")),
        "std/encoding/csv" => Some(include_str!("../../../std/encoding/csv.ms")),
        "std/encoding/json" => Some(include_str!("../../../std/encoding/json.ms")),
        "std/io" => Some(include_str!("../../../std/io.ms")),
        "std/path" => Some(include_str!("../../../std/path.ms")),
        _ => None,
    }
}

fn is_prelude_uri(uri: &str) -> bool {
    uri.ends_with("std/prelude.ms")
}

fn collect_dep_paths(module: &ParsedModule, interner: &Interner) -> Vec<String> {
    let mut paths = Vec::new();
    for &item_idx in module.ctx.expr_lists.get_slice(module.items) {
        let raw: &Symbol = match module.ctx.exprs.get(item_idx) {
            Expr::Import { path, .. } | Expr::Export { path, .. } => path,
            _ => continue,
        };
        let stripped = interner.resolve(*raw).trim_matches('"');
        paths.push(stripped.to_owned());
    }
    paths
}

/// Stub sources for `musi:*` native modules.
/// These allow the sema pass to resolve types for native-imported symbols
/// without having access to the actual native runtime implementations.
fn embedded_native(path: &str) -> Option<&'static str> {
    match path {
        "musi:math" => Some(
            "export extrin fn sqrt(x: Float): Float;\n\
             export extrin fn pow(x: Float, y: Float): Float;\n\
             export extrin fn floor(x: Float): Float;\n\
             export extrin fn ceil(x: Float): Float;\n\
             export extrin fn round(x: Float): Float;\n\
             export extrin fn fabs(x: Float): Float;\n\
             export extrin fn fmin(a: Float, b: Float): Float;\n\
             export extrin fn fmax(a: Float, b: Float): Float;\n\
             export extrin fn fclamp(x: Float, lo: Float, hi: Float): Float;\n",
        ),
        "musi:string" => Some(
            "export extrin fn string_length(s: String): Int;\n\
             export extrin fn string_slice(s: String, start: Int, end: Int): String;\n\
             export extrin fn string_to_int(s: String): Option[Int];\n\
             export extrin fn string_contains(s: String, sub: String): Bool;\n\
             export extrin fn nat_to_string(n: Nat): String;\n\
             export extrin fn string_split(s: String, delim: String): []String;\n\
             export extrin fn string_trim(s: String): String;\n\
             export extrin fn string_to_lower(s: String): String;\n\
             export extrin fn string_to_upper(s: String): String;\n\
             export extrin fn string_to_float(s: String): Option[Float];\n\
             export extrin fn string_index_of(s: String, sub: String): Option[Int];\n",
        ),
        "musi:fs" => Some(
            "export extrin fn fs_read_file(path: String): Option[String];\n\
             export extrin fn fs_write_file(path: String, content: String): Bool;\n\
             export extrin fn fs_append_file(path: String, content: String): Bool;\n\
             export extrin fn fs_read_dir(path: String): []String;\n\
             export extrin fn fs_exists(path: String): Bool;\n\
             export extrin fn fs_remove(path: String): Bool;\n\
             export extrin fn fs_rename(src: String, dst: String): Bool;\n\
             export extrin fn fs_make_dir(path: String): Bool;\n",
        ),
        "musi:path" => Some(
            "export extrin fn path_join(base: String, part: String): String;\n\
             export extrin fn path_dirname(p: String): Option[String];\n\
             export extrin fn path_basename(p: String): Option[String];\n\
             export extrin fn path_extension(p: String): Option[String];\n\
             export extrin fn path_is_absolute(p: String): Bool;\n\
             export extrin fn path_normalize(p: String): String;\n\
             export extrin fn path_stem(p: String): Option[String];\n",
        ),
        "musi:io" => Some("export extrin fn read_line(): String;\n"),
        "musi:os" => Some(
            "export extrin fn env_get(key: String): Option[String];\n\
             export extrin fn env_set(key: String, val: String): Unit;\n\
             export extrin fn env_vars(): []String;\n\
             export extrin fn os_name(): String;\n\
             export extrin fn cwd(): String;\n\
             export extrin fn set_cwd(path: String): Bool;\n",
        ),
        "musi:process" => Some(
            "export extrin fn exit(code: Int): Unit;\n\
             export extrin fn args(): []String;\n\
             export extrin fn spawn(cmd: String, args: []String): Int;\n\
             export extrin fn shell(cmd: String): String;\n",
        ),
        "musi:time" => Some(
            "export extrin fn now_millis(): Int;\n\
             export extrin fn sleep_ms(ms: Int): Unit;\n",
        ),
        _ => None,
    }
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

// -- AnalyzedDoc ------------------------------------------------------------

/// Lexed artifacts for a single imported stdlib module, stored for doc-comment lookup.
pub struct DepSource {
    pub source: String,
    pub tokens: Vec<Token>,
    pub trivia: Vec<Trivia>,
    /// Top-level named definitions: interned name → def span (start of fn/record/choice keyword).
    pub def_spans: HashMap<Symbol, Span>,
}

/// All artifacts produced by analyzing a single open document.
pub struct AnalyzedDoc {
    pub source: String,
    pub module: ParsedModule,
    pub interner: Interner,
    pub source_db: SourceDb,
    pub file_id: FileId,
    /// Tokens + trivia for the user document (used by semantic-token and hover handlers).
    pub lexed: LexedSource,
    /// `None` when the document imports native (`musi:*`) modules we cannot resolve.
    pub sema: Option<SemaResult>,
    /// Lexed artifacts for imported stdlib modules (prelude + explicit imports).
    /// Used to extract doc comments from stdlib definitions.
    pub dep_sources: HashMap<String, DepSource>,
}

// -- Public API -------------------------------------------------------------

/// Run the full pipeline on `source` and return (diagnostics, analyzed doc).
pub fn analyze_doc(source: &str, uri: &str) -> (Vec<Diagnostic>, AnalyzedDoc) {
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

    let mut import_map: HashMap<String, ModuleExports> = HashMap::new();
    let mut dep_sources: HashMap<String, DepSource> = HashMap::new();

    if !is_prelude_uri(uri) {
        let mut prelude_diags = DiagnosticBag::new();
        let (prelude_file_id, prelude_module, prelude_lexed) = parse_src(
            "<prelude>",
            PRELUDE_SRC,
            &mut interner,
            &mut source_db,
            &mut prelude_diags,
        );
        let empty: HashMap<String, ModuleExports> = HashMap::new();
        let prelude_result = analyze(
            &prelude_module,
            &interner,
            prelude_file_id,
            &mut prelude_diags,
            &empty,
        );
        let prelude_exports = exports_of(&prelude_result, &prelude_module, &interner);
        dep_sources.insert(
            "<prelude>".to_owned(),
            build_dep_source(PRELUDE_SRC, prelude_lexed, &prelude_result),
        );
        import_map.insert("<prelude>".to_owned(), prelude_exports);
    }

    let mut dep_queue = collect_dep_paths(&module, &interner);
    let mut visited: HashSet<String> = HashSet::new();
    let mut qi = 0;
    while qi < dep_queue.len() {
        let path = dep_queue[qi].clone();
        qi += 1;
        if !visited.insert(path.clone()) {
            continue;
        }
        let Some(dep_src) = embedded_std(&path).or_else(|| embedded_native(&path)) else {
            continue;
        };
        let mut dep_diags = DiagnosticBag::new();
        let (dep_file_id, dep_module, dep_lexed) = parse_src(
            &path,
            dep_src,
            &mut interner,
            &mut source_db,
            &mut dep_diags,
        );
        for p in collect_dep_paths(&dep_module, &interner) {
            dep_queue.push(p);
        }
        let dep_result = analyze(
            &dep_module,
            &interner,
            dep_file_id,
            &mut dep_diags,
            &import_map,
        );
        let dep_exports = exports_of(&dep_result, &dep_module, &interner);
        // Only add to dep_sources when there's actual source to extract doc comments from.
        if embedded_std(&path).is_some() {
            dep_sources.insert(path.clone(), build_dep_source(dep_src, dep_lexed, &dep_result));
        }
        import_map.insert(path, dep_exports);
    }

    let sema = analyze(&module, &interner, file_id, &mut diags, &import_map);
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
        dep_sources,
    };
    (lsp_diags, doc)
}

// -- Helpers ----------------------------------------------------------------

/// Returns the span of any expression node.
pub fn expr_span(idx: Idx<Expr>, module: &ParsedModule) -> Span {
    match module.ctx.exprs.get(idx) {
        Expr::Lit { span, .. }
        | Expr::Ident { span, .. }
        | Expr::Unit { span, .. }
        | Expr::Paren { span, .. }
        | Expr::Tuple { span, .. }
        | Expr::Block { span, .. }
        | Expr::Array { span, .. }
        | Expr::AnonRec { span, .. }
        | Expr::If { span, .. }
        | Expr::Match { span, .. }
        | Expr::While { span, .. }
        | Expr::Loop { span, .. }
        | Expr::For { span, .. }
        | Expr::Return { span, .. }
        | Expr::Break { span, .. }
        | Expr::Cycle { span, .. }
        | Expr::Defer { span, .. }
        | Expr::Import { span, .. }
        | Expr::Export { span, .. }
        | Expr::Using { span, .. }
        | Expr::Record { span, .. }
        | Expr::Choice { span, .. }
        | Expr::FnDef { span, .. }
        | Expr::Lambda { span, .. }
        | Expr::ClassDef { span, .. }
        | Expr::GivenDef { span, .. }
        | Expr::Bind { span, .. }
        | Expr::Prefix { span, .. }
        | Expr::Binary { span, .. }
        | Expr::Assign { span, .. }
        | Expr::Postfix { span, .. }
        | Expr::DotPrefix { span, .. }
        | Expr::Error { span, .. } => *span,
    }
}

/// Convert a 0-based (line, character) LSP position to a byte offset in `source`.
///
/// Uses byte-offset columns, which is correct for ASCII and close enough for
/// the position → span lookup used in navigation features.
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
    (line_start + character as usize) as u32
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
        end: offset_to_position(file_id, span.start + span.length, source_db),
    }
}

/// Extract doc-comment text for the definition at `def_start` from `source`.
///
/// Scans the leading trivia of the token at `def_start` and collects `///` lines.
pub fn extract_doc_comments_from_source(
    def_start: u32,
    source: &str,
    tokens: &[Token],
    trivia: &[musi_lex::Trivia],
) -> String {
    let tok = tokens.iter().find(|t| t.span.start == def_start);
    let Some(tok) = tok else { return String::new() };

    let tr_start = tok.leading_trivia.start as usize;
    let tr_end = tr_start + tok.leading_trivia.len as usize;
    let leading = &trivia[tr_start..tr_end];

    // Collect doc comment spans from the END of the leading trivia block
    // (so they are directly adjacent to the definition).
    let mut spans: Vec<(u32, u32)> = Vec::new(); // (start, end) byte pairs
    for tr in leading.iter().rev() {
        match &tr.kind {
            musi_lex::TriviaKind::LineComment { doc_style: true } => {
                spans.push((tr.span.start, tr.span.start + tr.span.length));
            }
            musi_lex::TriviaKind::Whitespace | musi_lex::TriviaKind::Newline => continue,
            _ => break,
        }
    }
    spans.reverse();

    if spans.is_empty() {
        return String::new();
    }

    spans
        .iter()
        .filter_map(|&(s, e)| {
            let text = source.get(s as usize..e as usize)?;
            // Strip `/// ` or `///` prefix.
            let stripped = text
                .strip_prefix("/// ")
                .or_else(|| text.strip_prefix("///"))
                .unwrap_or(text);
            Some(stripped)
        })
        .collect::<Vec<_>>()
        .join("\n")
}

pub(crate) fn to_lsp_diags<'a>(
    iter: impl Iterator<Item = &'a musi_shared::Diagnostic>,
    source_db: &SourceDb,
) -> Vec<Diagnostic> {
    iter.map(|d| {
        let start = offset_to_position(d.primary.file_id, d.primary.span.start, source_db);
        let end = offset_to_position(
            d.primary.file_id,
            d.primary.span.start + d.primary.span.length,
            source_db,
        );
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

/// Build a `DepSource` from a dependency's source text, lexed artifacts, and sema result.
///
/// `def_spans` maps each exported name to its definition span (the keyword token start)
/// in the dep source, so that `hover` can retrieve doc comments for imported symbols.
// -- Cursor-based lookup helpers -------------------------------------------

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
///
/// Checks (in order):
/// 1. `expr_defs` — reference sites (`Ident` expression nodes)
/// 2. `pat_defs`  — binding-site spans (`const x`, `var y`, params)
/// 3. `sema.defs` — name tokens of top-level definitions (fn / record / choice)
pub fn def_at_cursor<'a>(offset: u32, doc: &'a AnalyzedDoc) -> Option<&'a DefInfo> {
    let sema = doc.sema.as_ref()?;

    // 1. expr_defs (reference sites)
    if let Some(def_id) = sema
        .expr_defs
        .iter()
        .filter_map(|(&idx, &def_id)| {
            let span = expr_span(idx, &doc.module);
            if span.start <= offset && offset <= span.start + span.length {
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

    // 2. pat_defs (binding sites: const, var, params, variants)
    if let Some(def_id) = sema
        .pat_defs
        .iter()
        .filter_map(|(span, &def_id)| {
            if span.start <= offset && offset <= span.start + span.length {
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

    // 3. Definition name tokens (fn / record / choice declaration sites)
    sema.defs.iter().find(|def| {
        if def.span == musi_shared::Span::DUMMY {
            return false;
        }
        let name_span = find_name_token(&doc.lexed.tokens, def.span.start, def.name)
            .unwrap_or(def.span);
        name_span.start <= offset && offset <= name_span.start + name_span.length
    })
}

fn build_dep_source(source: &str, lexed: LexedSource, sema: &SemaResult) -> DepSource {
    let mut def_spans: HashMap<Symbol, Span> = HashMap::new();
    for def in &sema.defs {
        // First span wins — avoids overwriting real defs with auto-injected prelude entries.
        def_spans.entry(def.name).or_insert(def.span);
    }
    DepSource {
        source: source.to_owned(),
        tokens: lexed.tokens,
        trivia: lexed.trivia,
        def_spans,
    }
}
