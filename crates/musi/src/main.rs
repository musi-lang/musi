//! The `musi` command-line driver.
//!
//! Usage: `musi run <file.ms>`

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

use std::collections::HashSet;
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use musi_codegen::emit;
use musi_lex::lex;
use musi_parse::ast::Expr;
use musi_parse::{ParsedModule, parse};
use musi_shared::{DiagnosticBag, Interner, SourceDb};
use musi_vm::Vm;

const PRELUDE_SRC: &str = include_str!("../../../std/prelude.ms");
const PRELUDE_FILENAME: &str = "<prelude>";

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 3 || args[1] != "run" {
        eprintln!("Usage: musi run <file.ms>");
        process::exit(2);
    }

    let file_path = &args[2];

    let src = match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read '{file_path}': {e}");
            process::exit(1);
        }
    };

    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let user_file_id = source_db.add(file_path.as_str(), src.as_str());
    let user_lexed = lex(&src, user_file_id, &mut interner, &mut diags);
    let user_module = parse(&user_lexed.tokens, user_file_id, &mut diags, &interner);

    if diags.has_errors() {
        for diag in diags.iter() {
            eprintln!("{}", diag.render_simple(&source_db));
        }
        process::exit(1);
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
        for diag in diags.iter() {
            eprintln!("{}", diag.render_simple(&source_db));
        }
        process::exit(1);
    }

    // BFS over import + export-from paths, transitively resolving deps
    let user_file_path = Path::new(file_path);
    let mut queue: Vec<String> = collect_dep_paths(&user_module, &interner);
    let mut compiled: HashSet<String> = HashSet::new();
    let mut dep_modules: Vec<ParsedModule> = Vec::new();
    let mut qi = 0;

    while qi < queue.len() {
        let import_path = queue[qi].clone();
        qi += 1;

        let full_path = resolve_import_path(&import_path, user_file_path);
        let key = full_path.to_string_lossy().into_owned();
        if !compiled.insert(key.clone()) {
            continue;
        }

        let dep_src = match fs::read_to_string(&full_path) {
            Ok(s) => s,
            Err(e) => {
                eprintln!("error: cannot read import '{}': {e}", full_path.display());
                process::exit(1);
            }
        };

        let dep_file_id = source_db.add(key.as_str(), dep_src.as_str());
        let dep_lexed = lex(&dep_src, dep_file_id, &mut interner, &mut diags);
        let dep_module = parse(&dep_lexed.tokens, dep_file_id, &mut diags, &interner);

        // Enqueue transitive deps from this module
        for path in collect_dep_paths(&dep_module, &interner) {
            queue.push(path);
        }

        dep_modules.push(dep_module);
    }

    if diags.has_errors() {
        for diag in diags.iter() {
            eprintln!("{}", diag.render_simple(&source_db));
        }
        process::exit(1);
    }

    let dep_refs: Vec<&ParsedModule> = dep_modules.iter().collect();

    let module = match emit(&prelude_module, &dep_refs, &user_module, &interner) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("codegen error: {e}");
            process::exit(1);
        }
    };

    let main_fn_idx =
        u16::try_from(module.function_table.len() - 1).expect("function table index fits u16");

    let mut vm = Vm::new(module);
    match vm.run(main_fn_idx) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("runtime error: {e}");
            process::exit(1);
        }
    }
}

/// Returns all dependency path strings from top-level `import` and `export { } from` statements.
/// String literal symbols include surrounding quotes; these are stripped here.
fn collect_dep_paths(module: &ParsedModule, interner: &Interner) -> Vec<String> {
    let mut paths = Vec::new();
    for &item_idx in &module.items {
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
///
/// - Relative paths (`./foo`, `../bar`) resolve relative to the importing file's directory.
/// - All other paths (e.g. `std/core/Bool`) are resolved relative to the `MUSI_STD`
///   environment variable if set, otherwise relative to the current working directory
///   under `std/`.
fn resolve_import_path(import_path: &str, importer: &Path) -> PathBuf {
    if import_path.starts_with("./") || import_path.starts_with("../") {
        let base = importer.parent().unwrap_or_else(|| Path::new("."));
        base.join(import_path).with_extension("ms")
    } else {
        // If MUSI_STD is set, use it as the base for all non-relative paths.
        // Otherwise resolve relative to the current working directory so that
        // "std/core/Bool" finds ./std/core/Bool.ms when run from the project root.
        let base = env::var("MUSI_STD")
            .map_or_else(|_| PathBuf::from("."), PathBuf::from);
        base.join(import_path).with_extension("ms")
    }
}
