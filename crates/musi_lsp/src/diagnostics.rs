use std::collections::{HashMap, HashSet};

use musi_lex::lex;
use musi_parse::{ParsedModule, ast::Expr, parse};
use musi_sema::{ModuleExports, analyze, exports_of};
use musi_shared::{DiagnosticBag, FileId, Interner, Severity, SourceDb, Symbol};
use tower_lsp_server::ls_types::{Diagnostic, DiagnosticSeverity, DiagnosticTag, Position, Range};

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

/// Returns true if the given URI identifies the prelude (no prelude injection needed).
fn is_prelude_uri(uri: &str) -> bool {
    uri.ends_with("std/prelude.ms")
}

/// Extract import path strings from top-level `import`/`export { } from` statements.
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

/// Returns true if the module has any `musi:*` native import that we cannot resolve.
fn has_native_import(module: &ParsedModule, interner: &Interner) -> bool {
    collect_dep_paths(module, interner)
        .iter()
        .any(|p| p.starts_with("musi:"))
}

/// Lex + parse a source string, returning (file_id, module). Errors go into `diags`.
fn parse_src(
    label: &str,
    src: &str,
    interner: &mut Interner,
    source_db: &mut SourceDb,
    diags: &mut DiagnosticBag,
) -> (FileId, ParsedModule) {
    let file_id = source_db.add(label, src);
    let lexed = lex(src, file_id, interner, diags);
    let module = parse(&lexed.tokens, file_id, diags, interner);
    (file_id, module)
}

/// Run the full Musi lex + parse + sema pipeline on `source` and return LSP diagnostics.
///
/// `uri` is the document URI (used to detect when the user is editing the prelude
/// itself, in which case prelude auto-injection is suppressed to avoid duplicate
/// definition errors).
pub fn compute(source: &str, uri: &str) -> Vec<Diagnostic> {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();

    // -- Parse the user document first (needed to detect native imports) --
    let mut diags = DiagnosticBag::new();
    let (file_id, module) =
        parse_src("<document>", source, &mut interner, &mut source_db, &mut diags);

    // If the document imports from native (musi:*) modules we can't resolve,
    // skip sema to avoid flooding with undefined-name errors.
    if has_native_import(&module, &interner) {
        return to_lsp_diags(diags.iter().filter(|d| d.primary.file_id == file_id), &source_db);
    }

    // -- Prelude: analyze with a throw-away bag so its errors don't surface --
    let mut import_map: HashMap<String, ModuleExports> = HashMap::new();

    if !is_prelude_uri(uri) {
        // Only inject prelude context when the user document is NOT the prelude itself,
        // to avoid "duplicate definition" errors when editing prelude.ms.
        let mut prelude_diags = DiagnosticBag::new();
        let (prelude_file_id, prelude_module) = parse_src(
            "<prelude>",
            PRELUDE_SRC,
            &mut interner,
            &mut source_db,
            &mut prelude_diags,
        );
        let empty: HashMap<String, ModuleExports> = HashMap::new();
        let prelude_result =
            analyze(&prelude_module, &interner, prelude_file_id, &mut prelude_diags, &empty);
        let prelude_exports = exports_of(&prelude_result, &prelude_module, &interner);
        import_map.insert("<prelude>".to_owned(), prelude_exports);
    }

    // -- BFS through embedded imports --
    let mut dep_queue = collect_dep_paths(&module, &interner)
        .into_iter()
        .filter(|p| !p.starts_with("musi:"))
        .collect::<Vec<_>>();
    let mut visited: HashSet<String> = HashSet::new();
    let mut qi = 0;
    while qi < dep_queue.len() {
        let path = dep_queue[qi].clone();
        qi += 1;
        if !visited.insert(path.clone()) {
            continue;
        }
        let Some(dep_src) = embedded_std(&path) else {
            continue; // filesystem imports silently skipped in LSP context
        };
        let mut dep_diags = DiagnosticBag::new();
        let (dep_file_id, dep_module) =
            parse_src(&path, dep_src, &mut interner, &mut source_db, &mut dep_diags);
        for p in collect_dep_paths(&dep_module, &interner) {
            if !p.starts_with("musi:") {
                dep_queue.push(p);
            }
        }
        let dep_result = analyze(&dep_module, &interner, dep_file_id, &mut dep_diags, &import_map);
        let dep_exports = exports_of(&dep_result, &dep_module, &interner);
        import_map.insert(path, dep_exports);
    }

    // -- Analyze user document with full import context --
    let _sema = analyze(&module, &interner, file_id, &mut diags, &import_map);

    // -- Convert to LSP diagnostics (user document only) --
    to_lsp_diags(diags.iter().filter(|d| d.primary.file_id == file_id), &source_db)
}

fn to_lsp_diags<'a>(
    iter: impl Iterator<Item = &'a musi_shared::Diagnostic>,
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

fn offset_to_position(file_id: FileId, offset: u32, source_db: &SourceDb) -> Position {
    let src_len = u32::try_from(source_db.source(file_id).len()).unwrap_or(u32::MAX);
    let clamped = offset.min(src_len);
    let (line1, col1) = source_db.lookup(file_id, clamped);
    Position {
        // LSP positions are 0-based; source_db returns 1-based
        line: line1 - 1,
        character: col1 - 1,
    }
}

fn severity_to_lsp(s: Severity) -> DiagnosticSeverity {
    match s {
        Severity::Error => DiagnosticSeverity::ERROR,
        Severity::Warning => DiagnosticSeverity::WARNING,
        Severity::Note => DiagnosticSeverity::INFORMATION,
    }
}
