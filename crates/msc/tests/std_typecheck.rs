//! Typecheck all `stdlib/` stub files using the full multi-module pipeline.
//!
//! Run with: `cargo test -p msc --test std_typecheck -- --nocapture`
#![allow(clippy::tests_outside_test_module)]

use std::collections::HashMap;
use std::fs;
use std::panic;
use std::path::{Path, PathBuf};

use msc_ast::ParsedModule;
use msc_resolve::ResolverConfig;
use msc_resolve::graph::{ModuleId, build_module_graph};
use msc_sema::types::RecordField;
use msc_sema::{
    ExportBinding, ImportNames, ModuleAnalysisCtx, SharedAnalysisState, SubModuleExports,
    collect_exports,
};
use msc_sema::{SemaOptions, analyze_shared};
use msc_shared::{Arena, DiagnosticBag, FileId, Interner, Severity, SourceDb, Symbol};

fn collect_ms_files(dir: &Path) -> Vec<PathBuf> {
    let mut files = vec![];
    let mut stack = vec![dir.to_path_buf()];
    while let Some(current) = stack.pop() {
        let Ok(entries) = fs::read_dir(&current) else {
            continue;
        };
        for entry in entries.flatten() {
            let path = entry.path();
            if path.is_dir() {
                stack.push(path);
            } else if path.extension().is_some_and(|e| e == "ms") {
                files.push(path);
            }
        }
    }
    files.sort();
    files
}

fn build_import_names(
    node: &msc_resolve::ModuleNode,
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
    node: &msc_resolve::ModuleNode,
    module_exports: &HashMap<ModuleId, Vec<ExportBinding>>,
    types: &mut Arena<msc_sema::Type>,
    interner: &Interner,
) -> HashMap<Symbol, msc_sema::TypeIdx> {
    let mut map = HashMap::new();
    for &(dep_id, import_sym) in &node.imports {
        if let Some(exports) = module_exports.get(&dep_id) {
            let mut fields: Vec<_> = exports
                .iter()
                .map(|b| RecordField {
                    name: b.name,
                    ty: b.ty,
                    ty_params: b.ty_params.clone(),
                    binding: None,
                })
                .collect();
            fields.sort_by(|a, b| interner.resolve(a.name).cmp(interner.resolve(b.name)));
            let ty = types.alloc(msc_sema::Type::Record { fields, rest: None });
            let _prev = map.insert(import_sym, ty);
        }
    }
    map
}

/// Runs sema over all modules in `order`, threading exports between them.
/// Diagnostics accumulate in `diags`.
fn run_sema_pass(
    graph: &msc_resolve::ModuleGraph,
    order: &[ModuleId],
    parsed_modules: &mut HashMap<ModuleId, ParsedModule>,
    interner: &mut Interner,
    diags: &mut DiagnosticBag,
) {
    let mut state = SharedAnalysisState::new(interner);
    let mut module_exports: HashMap<ModuleId, Vec<ExportBinding>> = HashMap::new();
    let mut sub_module_exports: SubModuleExports = HashMap::new();
    let options = SemaOptions::default();

    for &module_id in order {
        let node = graph.get(module_id);
        let file_id = node.file_id;

        if node.builtin {
            let _prev = module_exports.insert(module_id, vec![]);
            continue;
        }

        let import_names = build_import_names(node, &module_exports);
        let import_types = build_import_types(node, &module_exports, &mut state.types, interner);

        let Some(parsed) = parsed_modules.remove(&module_id) else {
            continue;
        };

        let output = analyze_shared(
            &parsed,
            &mut state,
            interner,
            file_id,
            diags,
            &ModuleAnalysisCtx {
                import_names: &import_names,
                import_types: &import_types,
                sub_module_exports: &sub_module_exports,
                options: &options,
            },
        );

        let defs_vec: Vec<_> = state.defs.iter().cloned().collect();
        let exports = collect_exports(&parsed, &defs_vec, &output.resolution.pat_defs);

        for &(dep_id, import_sym) in &node.imports {
            for export in exports.bindings.iter().filter(|e| e.name == import_sym) {
                if let Some(sub_exports) = module_exports.get(&dep_id) {
                    let names: Vec<_> = sub_exports.iter().map(|eb| (eb.name, eb.def_id)).collect();
                    let _prev = sub_module_exports.insert(export.def_id, names);
                }
            }
        }

        let _prev = module_exports.insert(module_id, exports.bindings);
        state.collect_lang_items(interner);
        state.inject_lang_items_into_prelude();
    }
}

#[test]
fn typecheck_all_std_files() {
    let manifest_dir = PathBuf::from(env!("CARGO_MANIFEST_DIR"));
    let std_dir = manifest_dir.join("../../stdlib");
    let std_dir = std_dir
        .canonicalize()
        .expect("stdlib/ directory should exist");

    let files = collect_ms_files(&std_dir);
    if files.is_empty() {
        eprintln!("no .ms files found under stdlib/ - skipping");
        return;
    }

    // project_root is the repo root so that ResolverConfig can locate stdlib/
    // via discover_std_root.
    let repo_root = std_dir.parent().expect("stdlib/ has a parent directory");

    let mut total_errors = 0u32;
    let mut total_warnings = 0u32;
    let mut total_panics = 0u32;
    let mut file_count = 0u32;

    for path in &files {
        let rel_path = path
            .strip_prefix(&std_dir)
            .unwrap_or(path)
            .display()
            .to_string();

        let config = ResolverConfig::from_project(repo_root, None);
        let mut interner = Interner::new();
        let mut source_db = SourceDb::new();
        let mut diags = DiagnosticBag::new();
        let mut parsed_modules: HashMap<ModuleId, ParsedModule> = HashMap::new();

        let graph = match build_graph(
            path,
            &config,
            &mut interner,
            &mut source_db,
            &mut diags,
            &mut parsed_modules,
        ) {
            GraphOutcome::Ok(g) => g,
            GraphOutcome::Err(e) => {
                eprintln!("\n=== stdlib/{rel_path} ===");
                eprintln!("  graph error: {e}");
                file_count += 1;
                continue;
            }
        };

        // Entry module is always ModuleId(0); track its FileId to filter
        // diagnostics to only those originating in this file (not its deps).
        let entry_file_id: FileId = graph.get(ModuleId(0)).file_id;

        let order = match graph.toposort() {
            Ok(o) => o,
            Err(e) => {
                eprintln!("\n=== stdlib/{rel_path} ===");
                eprintln!("  toposort error: {e}");
                file_count += 1;
                continue;
            }
        };

        let panicked = panic::catch_unwind(panic::AssertUnwindSafe(|| {
            run_sema_pass(
                &graph,
                &order,
                &mut parsed_modules,
                &mut interner,
                &mut diags,
            );
        }))
        .is_err();

        if panicked {
            eprintln!("\n=== stdlib/{rel_path} ===");
            eprintln!("  PANIC during sema");
            total_panics += 1;
            file_count += 1;
            continue;
        }

        let entry_diags: Vec<_> = diags
            .iter()
            .filter(|d| d.primary.file_id == entry_file_id)
            .collect();

        let errors = entry_diags
            .iter()
            .filter(|d| d.severity == Severity::Error)
            .count();
        let warnings = entry_diags
            .iter()
            .filter(|d| d.severity == Severity::Warning)
            .count();

        if errors > 0 || warnings > 0 {
            eprintln!("\n=== stdlib/{rel_path} ===");
            for d in &entry_diags {
                eprintln!("  {}", d.render_simple(&source_db));
            }
            eprintln!("  {errors} error(s), {warnings} warning(s)");
        }

        total_errors += u32::try_from(errors).expect("count fits");
        total_warnings += u32::try_from(warnings).expect("count fits");
        file_count += 1;
    }

    eprintln!("\n========================================");
    eprintln!(
        "Checked {file_count} files: {total_errors} total error(s), {total_warnings} total warning(s), {total_panics} panic(s)"
    );
    eprintln!("========================================");

    // Catalogue test - does NOT assert zero errors.
}

enum GraphOutcome {
    Ok(msc_resolve::ModuleGraph),
    Err(String),
}

fn build_graph(
    path: &Path,
    config: &ResolverConfig,
    interner: &mut Interner,
    source_db: &mut SourceDb,
    diags: &mut DiagnosticBag,
    parsed_modules: &mut HashMap<ModuleId, ParsedModule>,
) -> GraphOutcome {
    // `build_module_graph` takes mutable references that are not `UnwindSafe`
    // so we cannot use catch_unwind here directly. Panics from the graph
    // builder are therefore not caught - they will surface as test failures
    // rather than being silently ignored.
    match build_module_graph(
        path,
        config,
        interner,
        source_db,
        diags,
        parsed_modules,
        None,
    ) {
        Ok(g) => GraphOutcome::Ok(g),
        Err(e) => GraphOutcome::Err(e.to_string()),
    }
}
