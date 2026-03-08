//! Shared compilation pipeline used by all CLI commands.

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use musi_codegen::{Module, emit};
use musi_lex::lex;
use musi_parse::{ParsedModule, parse};
use musi_shared::{DiagnosticBag, FileId, Interner, SourceDb};

use crate::config::{self, MusiConfig, find_and_load};
use crate::deps;
use crate::fetch::{self, resolve_package_import};
use crate::lock::LockFile;

pub const PRELUDE_SRC: &str = include_str!("../../../std/prelude.ms");
pub const PRELUDE_FILENAME: &str = "<prelude>";

/// Embedded std library sources -- served without disk access.
/// Keys match the import path strings used in Musi source (without .ms extension).
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
        "std/encoding/csv" => Some(include_str!("../../../std/encoding/csv.ms")),
        "std/encoding/json" => Some(include_str!("../../../std/encoding/json.ms")),
        "std/io" => Some(include_str!("../../../std/io.ms")),
        "std/path" => Some(include_str!("../../../std/path.ms")),
        "std/collections/queue" => Some(include_str!("../../../std/collections/queue.ms")),
        _ => None,
    }
}

pub fn read_file(file_path: &str) -> String {
    match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read '{file_path}': {e}");
            process::exit(1);
        }
    }
}

pub fn parse_file(
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

pub fn print_diags_and_exit(diags: &DiagnosticBag, source_db: &SourceDb) -> ! {
    use std::io::{IsTerminal, stderr};
    let use_color = stderr().is_terminal();
    for diag in diags.iter() {
        eprintln!("{}", diag.render_rich(source_db, use_color));
    }
    process::exit(1);
}

/// Returns all dependency path strings from top-level `import` and `export { } from` statements.
pub fn collect_dep_paths(module: &ParsedModule, interner: &Interner) -> Vec<String> {
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
pub fn resolve_import_path(import_path: &str, importer: &Path) -> PathBuf {
    if import_path.starts_with("./") || import_path.starts_with("../") {
        let base = importer.parent().unwrap_or_else(|| Path::new("."));
        base.join(import_path).with_extension("ms")
    } else {
        let base = env::var("MUSI_STD").map_or_else(|_| PathBuf::from("."), PathBuf::from);
        base.join(import_path).with_extension("ms")
    }
}

/// Load project config from the current working directory (walks up to find mspackage.json).
pub fn load_project_config() -> Option<(MusiConfig, PathBuf)> {
    let cwd = env::current_dir().ok()?;
    find_and_load(&cwd)
}

/// Resolved dependency: import path string, parsed module, and file ID.
pub type ResolvedDep = (String, ParsedModule, FileId);

/// BFS-collect and parse all transitive dependencies of `root_module`.
/// When `tolerate_missing` is true, missing non-native imports are silently skipped
/// (used by `check` which doesn't need every file to exist).
pub fn collect_and_parse_deps(
    root_module: &ParsedModule,
    root_file_path: &Path,
    interner: &mut Interner,
    source_db: &mut SourceDb,
    diags: &mut DiagnosticBag,
    tolerate_missing: bool,
) -> Vec<ResolvedDep> {
    // Load project config to get package dependencies
    let (mut project_config, project_dir) = load_project_config().unzip();
    let mut project_deps: HashMap<String, String> = project_config
        .as_ref()
        .map(|cfg| cfg.dependencies.clone())
        .unwrap_or_default();

    // Track newly added URL imports
    let mut added_deps: Vec<(String, String)> = Vec::new();

    let mut queue: Vec<String> = collect_dep_paths(root_module, interner);
    let mut compiled: HashSet<String> = HashSet::new();
    let mut result_deps: Vec<ResolvedDep> = Vec::new();
    let mut qi = 0;

    while qi < queue.len() {
        let import_path = queue[qi].clone();
        qi += 1;

        let dep_src_owned: String;
        let key: String;
        if let Some(std_src) = embedded_std(&import_path) {
            key = import_path.clone();
            if !compiled.insert(key.clone()) {
                continue;
            }
            dep_src_owned = std_src.to_owned();
        } else if let Some(native_src) = musi_native::source_for(&import_path) {
            key = import_path.clone();
            if !compiled.insert(key.clone()) {
                continue;
            }
            dep_src_owned = native_src.to_owned();
        } else if deps::is_url_import(&import_path) {
            // URL import (github:scope/repo or https://github.com/...)
            let Some((spec, subpath)) = deps::parse_url_import(&import_path) else {
                if tolerate_missing {
                    continue;
                }
                eprintln!("error: invalid URL import '{import_path}'");
                process::exit(1);
            };

            // Fetch/install the package
            let resolved = match fetch::ensure_installed(&spec) {
                Ok(r) => r,
                Err(e) => {
                    if tolerate_missing {
                        continue;
                    }
                    eprintln!("error: failed to fetch '{import_path}': {e}");
                    process::exit(1);
                }
            };

            // Determine the file to load
            let pkg_path = if let Some(sub) = &subpath {
                resolved.cache_path.join(sub).with_extension("ms")
            } else {
                resolve_package_main(&resolved.cache_path)
            };

            key = pkg_path.to_string_lossy().into_owned();
            if !compiled.insert(key.clone()) {
                continue;
            }

            dep_src_owned = match fs::read_to_string(&pkg_path) {
                Ok(s) => s,
                Err(e) => {
                    if tolerate_missing {
                        continue;
                    }
                    eprintln!("error: cannot read URL import '{import_path}': {e}");
                    process::exit(1);
                }
            };

            // Add to project deps so subsequent imports resolve
            let version_str = format!("^{}", resolved.version);
            if !project_deps.contains_key(&spec.name) {
                let _prev = project_deps.insert(spec.name.clone(), version_str.clone());
                added_deps.push((spec.name.clone(), version_str));
                eprintln!("  Resolved {} -> v{}", spec.name, resolved.version);
            }
        } else if let Some(pkg_path) = resolve_package_import(&import_path, &project_deps) {
            // Package dependency from mspackage.json
            let main_file = resolve_package_main(&pkg_path);
            key = main_file.to_string_lossy().into_owned();
            if !compiled.insert(key.clone()) {
                continue;
            }
            dep_src_owned = match fs::read_to_string(&main_file) {
                Ok(s) => s,
                Err(e) => {
                    if tolerate_missing {
                        continue;
                    }
                    eprintln!("error: cannot read package '{import_path}': {e}");
                    process::exit(1);
                }
            };
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
        result_deps.push((import_path, dep_module, dep_file_id));
    }

    // Auto-add URL imports to mspackage.json
    if !added_deps.is_empty()
        && let Some(ref mut cfg) = project_config
        && let Some(ref dir) = project_dir
    {
        for (name, version) in &added_deps {
            let _prev = cfg.dependencies.insert(name.clone(), version.clone());
        }
        if let Err(e) = config::save_to_dir(dir, cfg) {
            eprintln!("warning: failed to update mspackage.json: {e}");
        } else {
            eprintln!("  Added {} dependencies to mspackage.json", added_deps.len());
        }

        // Update lock file
        let mut lock = LockFile::read_from(dir).unwrap_or_default();
        for (name, _version) in &added_deps {
            if let Some(spec) = deps::parse_dep_spec(name, project_deps.get(name).unwrap_or(&"*".to_owned())).ok()
                && let Ok(resolved) = fetch::ensure_installed(&spec)
            {
                lock.add_resolved(&resolved);
            }
        }
        if let Err(e) = lock.write_to(dir) {
            eprintln!("warning: failed to update musi.lock: {e}");
        }
    }

    result_deps
}

/// Resolve the main entry point for a package directory.
fn resolve_package_main(pkg_path: &Path) -> PathBuf {
    // Try to read mspackage.json for the "main" field
    let config_path = pkg_path.join("mspackage.json");
    if let Ok(text) = fs::read_to_string(&config_path)
        && let Ok(config) = serde_json::from_str::<MusiConfig>(&text)
        && let Some(main) = config.main
    {
        return pkg_path.join(main.trim_start_matches("./"));
    }
    // Default to index.ms
    pkg_path.join("index.ms")
}

/// Run semantic analysis on parsed prelude, deps, and user module.
/// Returns `true` if clean (no errors). Diagnostics are accumulated in `diags`.
pub fn run_sema(
    prelude_module: &ParsedModule,
    prelude_file_id: FileId,
    deps: &[ResolvedDep],
    user_module: &ParsedModule,
    user_file_id: FileId,
    interner: &Interner,
    diags: &mut DiagnosticBag,
) -> bool {
    use std::collections::HashMap;

    let empty_imports: HashMap<String, musi_sema::ModuleExports> = HashMap::new();
    let prelude_result = musi_sema::analyze(
        prelude_module,
        interner,
        prelude_file_id,
        diags,
        &empty_imports,
    );
    let prelude_exports = musi_sema::exports_of(&prelude_result, prelude_module, interner);

    let mut import_map: HashMap<String, musi_sema::ModuleExports> = HashMap::new();
    let _ = import_map.insert(PRELUDE_FILENAME.to_owned(), prelude_exports);

    for (path, dep_module, dep_file_id) in deps {
        let dep_result = musi_sema::analyze(dep_module, interner, *dep_file_id, diags, &import_map);
        let exports = musi_sema::exports_of(&dep_result, dep_module, interner);
        let _ = import_map.insert(path.clone(), exports);
    }

    let _result = musi_sema::analyze(user_module, interner, user_file_id, diags, &import_map);

    !diags.has_errors()
}

/// Full compile pipeline: parse + BFS deps + codegen. Exits on error.
pub fn compile_file(file_path: &str) -> Module {
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
