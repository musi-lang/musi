//! Shared compilation pipeline used by all CLI commands.

use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use musi_codegen::{Module, emit};
use musi_lex::lex;
use musi_parse::{ParsedModule, parse};
use musi_shared::{DiagnosticBag, FileId, Interner, SourceDb};

pub(crate) const PRELUDE_SRC: &str = include_str!("../../../std/prelude.ms");
pub(crate) const PRELUDE_FILENAME: &str = "<prelude>";

pub(crate) fn read_file(file_path: &str) -> String {
    match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read '{file_path}': {e}");
            process::exit(1);
        }
    }
}

pub(crate) fn parse_file(
    file_path: &str,
    src: &str,
    interner: &mut Interner,
    source_db: &mut SourceDb,
    diags: &mut DiagnosticBag,
) -> (FileId, ParsedModule) {
    let file_id = source_db.add(file_path, src);
    let lexed = lex(src, file_id, interner, diags);
    let module = parse(&lexed.tokens, file_id, diags, interner);
    (file_id, module)
}

pub(crate) fn print_diags_and_exit(diags: &DiagnosticBag, source_db: &SourceDb) -> ! {
    use std::io::IsTerminal;
    let use_color = std::io::stderr().is_terminal();
    for diag in diags.iter() {
        eprintln!("{}", diag.render_rich(source_db, use_color));
    }
    process::exit(1);
}

/// Returns all dependency path strings from top-level `import` and `export { } from` statements.
pub(crate) fn collect_dep_paths(module: &ParsedModule, interner: &Interner) -> Vec<String> {
    use musi_parse::ast::Expr;
    let mut paths = Vec::new();
    for &item_idx in module.ctx.expr_lists.get_slice(module.items) {
        let raw = match module.ctx.exprs.get(item_idx) {
            Expr::Import { path, .. } | Expr::Export { path, .. } => interner.resolve(*path),
            _ => continue,
        };
        let stripped = raw.trim_matches('"');
        paths.push(stripped.to_owned());
    }
    paths
}

/// Resolves an import path string to a filesystem path.
pub(crate) fn resolve_import_path(import_path: &str, importer: &Path) -> PathBuf {
    if import_path.starts_with("./") || import_path.starts_with("../") {
        let base = importer.parent().unwrap_or_else(|| Path::new("."));
        base.join(import_path).with_extension("ms")
    } else {
        let base = env::var("MUSI_STD").map_or_else(|_| PathBuf::from("."), PathBuf::from);
        base.join(import_path).with_extension("ms")
    }
}

/// Load project config from the current working directory (walks up to find mspackage.json).
pub(crate) fn load_project_config() -> Option<(crate::config::MusiConfig, PathBuf)> {
    let cwd = env::current_dir().ok()?;
    crate::config::find_and_load(&cwd)
}

/// Resolved dependency: import path string, parsed module, and file ID.
pub(crate) type ResolvedDep = (String, ParsedModule, FileId);

/// BFS-collect and parse all transitive dependencies of `root_module`.
/// When `tolerate_missing` is true, missing non-native imports are silently skipped
/// (used by `check` which doesn't need every file to exist).
pub(crate) fn collect_and_parse_deps(
    root_module: &ParsedModule,
    root_file_path: &Path,
    interner: &mut Interner,
    source_db: &mut SourceDb,
    diags: &mut DiagnosticBag,
    tolerate_missing: bool,
) -> Vec<ResolvedDep> {
    let mut queue: Vec<String> = collect_dep_paths(root_module, interner);
    let mut compiled: HashSet<String> = HashSet::new();
    let mut deps: Vec<ResolvedDep> = Vec::new();
    let mut qi = 0;

    while qi < queue.len() {
        let import_path = queue[qi].clone();
        qi += 1;

        let dep_src_owned: String;
        let key: String;
        if let Some(native_src) = musi_native::source_for(&import_path) {
            key = import_path.clone();
            if !compiled.insert(key.clone()) {
                continue;
            }
            dep_src_owned = native_src.to_owned();
        } else {
            let full_path = resolve_import_path(&import_path, root_file_path);
            key = full_path.to_string_lossy().into_owned();
            if !compiled.insert(key.clone()) {
                continue;
            }
            dep_src_owned = match fs::read_to_string(&full_path) {
                Ok(s) => s,
                Err(e) => {
                    if tolerate_missing {
                        continue;
                    }
                    eprintln!("error: cannot read import '{}': {e}", full_path.display());
                    process::exit(1);
                }
            };
        }

        let (dep_file_id, dep_module) =
            parse_file(&key, &dep_src_owned, interner, source_db, diags);
        for path in collect_dep_paths(&dep_module, interner) {
            queue.push(path);
        }
        deps.push((import_path, dep_module, dep_file_id));
    }
    deps
}

/// Full compile pipeline: parse + BFS deps + codegen. Exits on error.
pub(crate) fn compile_file(file_path: &str) -> Module {
    let src = read_file(file_path);

    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let (_user_file_id, user_module) =
        parse_file(file_path, &src, &mut interner, &mut source_db, &mut diags);

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
    }

    let prelude_file_id = source_db.add(PRELUDE_FILENAME, PRELUDE_SRC);
    let prelude_lexed = lex(PRELUDE_SRC, prelude_file_id, &mut interner, &mut diags);
    let prelude_module = parse(
        &prelude_lexed.tokens,
        prelude_file_id,
        &mut diags,
        &interner,
    );

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
    }

    let user_file_path = Path::new(file_path);
    let deps = collect_and_parse_deps(
        &user_module,
        user_file_path,
        &mut interner,
        &mut source_db,
        &mut diags,
        false,
    );

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
    }

    let dep_paths: Vec<&str> = deps.iter().map(|(p, _, _)| p.as_str()).collect();
    let dep_modules: Vec<&ParsedModule> = deps.iter().map(|(_, m, _)| m).collect();
    match emit(
        &prelude_module,
        &dep_modules,
        &dep_paths,
        &user_module,
        &interner,
    ) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("codegen error: {e}");
            process::exit(1);
        }
    }
}
