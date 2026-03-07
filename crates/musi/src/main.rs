//! The `musi` command-line driver.
//!
//! Usage:
//!   musi run <file.ms>    -- compile and execute
//!   musi check <file.ms>  -- type-check only

#![allow(clippy::module_name_repetitions)]
#![allow(clippy::exhaustive_structs)]
#![allow(clippy::exhaustive_enums)]

use std::collections::{HashMap, HashSet};
use std::env;
use std::fs;
use std::path::{Path, PathBuf};
use std::process;

use musi_codegen::{emit, Module};
use musi_lex::lex;
use musi_parse::ast::Expr;
use musi_parse::{ParsedModule, parse};
use musi_shared::{DiagnosticBag, Interner, SourceDb};
use musi_vm::Vm;

const PRELUDE_SRC: &str = include_str!("../../../std/prelude.ms");
const PRELUDE_FILENAME: &str = "<prelude>";

fn print_usage() {
    eprintln!("Usage: musi <command> [options]");
    eprintln!();
    eprintln!("Commands:");
    eprintln!("  run <file.ms>    Run a Musi program");
    eprintln!("  check <file.ms>  Type-check a Musi program");
    eprintln!("  test <file.ms>   Run tests in a Musi file");
}

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() < 2 {
        print_usage();
        process::exit(2);
    }

    let command = &args[1];

    match command.as_str() {
        "run" | "check" | "test" if args.len() != 3 => {
            print_usage();
            process::exit(2);
        }
        "run" => cmd_run(&args[2]),
        "check" => cmd_check(&args[2]),
        "test" => cmd_test(&args[2]),
        _ => {
            eprintln!("error: unknown command '{command}'");
            eprintln!();
            print_usage();
            process::exit(2);
        }
    }
}

fn read_file(file_path: &str) -> String {
    match fs::read_to_string(file_path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: cannot read '{file_path}': {e}");
            process::exit(1);
        }
    }
}

fn parse_file(
    file_path: &str,
    src: &str,
    interner: &mut Interner,
    source_db: &mut SourceDb,
    diags: &mut DiagnosticBag,
) -> (musi_shared::FileId, ParsedModule) {
    let file_id = source_db.add(file_path, src);
    let lexed = lex(src, file_id, interner, diags);
    let module = parse(&lexed.tokens, file_id, diags, interner);
    (file_id, module)
}

fn print_diags_and_exit(diags: &DiagnosticBag, source_db: &SourceDb) -> ! {
    for diag in diags.iter() {
        eprintln!("{}", diag.render_simple(source_db));
    }
    process::exit(1);
}

fn cmd_check(file_path: &str) {
    let src = read_file(file_path);

    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let (file_id, user_module) = parse_file(file_path, &src, &mut interner, &mut source_db, &mut diags);

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
    }

    // BFS over imports, analyze each in order, accumulate exports
    let user_file_path = Path::new(file_path);
    let mut queue: Vec<String> = collect_dep_paths(&user_module, &interner);
    let mut compiled: HashSet<String> = HashSet::new();
    let mut dep_modules: Vec<(String, ParsedModule, musi_shared::FileId)> = Vec::new();
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
            Err(_) => continue, // skip missing std files in check mode
        };
        let (dep_file_id, dep_module) = parse_file(key.as_str(), &dep_src, &mut interner, &mut source_db, &mut diags);
        for path in collect_dep_paths(&dep_module, &interner) {
            queue.push(path);
        }
        dep_modules.push((import_path, dep_module, dep_file_id));
    }

    // Analyze deps in order, collecting exports
    let mut import_map: HashMap<String, musi_sema::ModuleExports> = HashMap::new();
    let empty_imports = HashMap::new();
    for (path, dep_module, dep_file_id) in &dep_modules {
        let dep_result = musi_sema::analyze(dep_module, &interner, *dep_file_id, &mut diags, &empty_imports);
        let exports = musi_sema::exports_of(&dep_result, dep_module, &interner);
        let _prev = import_map.insert(path.clone(), exports);
    }

    let _result = musi_sema::analyze(&user_module, &interner, file_id, &mut diags, &import_map);

    if diags.has_errors() {
        for diag in diags.iter() {
            eprintln!("{}", diag.render_simple(&source_db));
        }
        process::exit(1);
    }

    process::exit(0);
}

fn compile_file(file_path: &str) -> Module {
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
    let prelude_module = parse(&prelude_lexed.tokens, prelude_file_id, &mut diags, &interner);

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
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

        for path in collect_dep_paths(&dep_module, &interner) {
            queue.push(path);
        }

        dep_modules.push(dep_module);
    }

    if diags.has_errors() {
        print_diags_and_exit(&diags, &source_db);
    }

    let dep_refs: Vec<&ParsedModule> = dep_modules.iter().collect();

    match emit(&prelude_module, &dep_refs, &user_module, &interner) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("codegen error: {e}");
            process::exit(1);
        }
    }
}

fn cmd_run(file_path: &str) {
    let module = compile_file(file_path);

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

fn cmd_test(file_path: &str) {
    let module = compile_file(file_path);

    let main_fn_idx =
        u16::try_from(module.function_table.len() - 1).expect("function table index fits u16");

    let mut vm = Vm::new(module);
    match vm.run_tests(main_fn_idx) {
        Ok(_) => {}
        Err(e) => {
            eprintln!("runtime error: {e}");
            process::exit(1);
        }
    }

    let results = &vm.test_results;
    let passed = results.iter().filter(|r| r.passed).count();
    let failed = results.iter().filter(|r| !r.passed).count();

    for r in results {
        if r.passed {
            println!("PASS  {}", r.label);
        } else {
            let msg = r.error.as_deref().unwrap_or("assertion failed");
            println!("FAIL  {}: {msg}", r.label);
        }
    }

    if !results.is_empty() {
        println!();
        println!("{passed} passed, {failed} failed");
    } else {
        println!("no tests found");
    }

    if failed > 0 {
        process::exit(1);
    }
}

/// Returns all dependency path strings from top-level `import` and `export { } from` statements.
fn collect_dep_paths(module: &ParsedModule, interner: &Interner) -> Vec<String> {
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
fn resolve_import_path(import_path: &str, importer: &Path) -> PathBuf {
    if import_path.starts_with("./") || import_path.starts_with("../") {
        let base = importer.parent().unwrap_or_else(|| Path::new("."));
        base.join(import_path).with_extension("ms")
    } else {
        let base = env::var("MUSI_STD")
            .map_or_else(|_| PathBuf::from("."), PathBuf::from);
        base.join(import_path).with_extension("ms")
    }
}
