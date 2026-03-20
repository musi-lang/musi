//! Analysis pipeline: parse, resolve, and run sema on open documents.
//!
//! `analyze_doc` handles single-file mode; `analyze_doc_multi` builds the full
//! module graph and runs sema in dependency order.

use std::collections::HashMap;
use std::path::Path;

use lsp_types::Diagnostic;
use msc_ast::ParsedModule;
use msc_lex::{LexedSource, lex};
use msc_parse::parse;
use msc_resolve::graph::ModuleId;
use msc_resolve::{ModuleGraph, ModuleNode, ResolverConfig, build_module_graph};
use msc_sema::types::RecordField;
use msc_sema::{
    DefInfo, ExportBinding, ImportNames, ModuleAnalysisCtx, ModuleSemaOutput, SemaOptions,
    SharedAnalysisState, SubModuleExports, Type, TypeIdx, analyze, analyze_shared, collect_exports,
};
use msc_shared::{Arena, DiagnosticBag, FileId, Interner, SourceDb, Span, Symbol};

use crate::analysis::cursor::build_span_index;
use crate::analysis::doc::{AnalyzedDoc, DepSource, LspSemaOutput};
use crate::to_proto::to_lsp_diags;
use crate::types::{DepSourceMap, ModuleExportMap, ResolvedImports};

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

    let sema = analyze(
        &module,
        &mut interner,
        file_id,
        &mut diags,
        &SemaOptions::default(),
    );
    let lsp_diags = to_lsp_diags(
        diags.iter().filter(|d| d.primary.file_id == file_id),
        &source_db,
    );

    let span_index = build_span_index(&sema, &module);
    let doc = AnalyzedDoc {
        source: source.to_owned(),
        module,
        interner,
        source_db,
        file_id,
        lexed,
        sema: Some(sema),
        dep_sources: HashMap::new(),
        resolved_imports: HashMap::new(),
        span_index,
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

    let resolved_imports = build_resolved_imports(&graph);

    let (sema, entry_parsed, entry_file_id, lexed, dep_sources) = match run_lsp_sema_in_order(
        &graph,
        &order,
        &mut parsed_modules,
        &mut interner,
        &mut diags,
        source,
        project_root,
    ) {
        Some(result) => result,
        None => return analyze_doc(source, "<document>"),
    };

    let lsp_diags = to_lsp_diags(
        diags.iter().filter(|d| d.primary.file_id == entry_file_id),
        &source_db,
    );

    let span_index = build_span_index(&sema, &entry_parsed);
    let doc = AnalyzedDoc {
        source: source.to_owned(),
        module: entry_parsed,
        interner,
        source_db,
        file_id: entry_file_id,
        lexed,
        sema: Some(sema),
        dep_sources,
        resolved_imports,
        span_index,
    };
    (lsp_diags, doc)
}

/// Build a `Symbol → PathBuf` map from the entry module's imports in the graph.
fn build_resolved_imports(graph: &ModuleGraph) -> ResolvedImports {
    let entry = graph.get(ModuleId(0));
    let mut resolved = HashMap::new();
    for &(dep_id, import_sym) in &entry.imports {
        let dep = graph.get(dep_id);
        if !dep.builtin {
            resolved.insert(import_sym, dep.path.clone());
        }
    }
    resolved
}

fn run_lsp_sema_in_order(
    graph: &ModuleGraph,
    order: &[ModuleId],
    parsed_modules: &mut HashMap<ModuleId, ParsedModule>,
    interner: &mut Interner,
    diags: &mut DiagnosticBag,
    entry_source: &str,
    project_root: &Path,
) -> Option<LspSemaOutput> {
    let mut state = SharedAnalysisState::new(interner);
    let mut module_exports: ModuleExportMap = HashMap::new();
    let mut sub_module_exports: SubModuleExports = HashMap::new();
    let mut dep_sources: DepSourceMap = HashMap::new();

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
                options: &SemaOptions::default(),
            },
        );

        if module_id == entry_id {
            entry_output = Some((output, parsed));
        } else {
            let defs_vec: Vec<_> = state.defs.iter().cloned().collect();
            let exports = collect_exports(&parsed, &defs_vec, &output.resolution.pat_defs);

            for &(dep_id, import_sym) in &node.imports {
                for export in &exports.bindings {
                    if export.name == import_sym
                        && let Some(sub_exports) = module_exports.get(&dep_id)
                    {
                        let names: Vec<_> =
                            sub_exports.iter().map(|eb| (eb.name, eb.def_id)).collect();
                        let _prev = sub_module_exports.insert(export.def_id, names);
                    }
                }
            }

            let _prev = module_exports.insert(module_id, exports.bindings);

            let mod_key = module_key(&node.path, project_root);
            let dep_lexed = lex(&node.source, file_id, interner, diags);
            let dep_src = build_dep_source(&node.source, dep_lexed, &defs_vec, file_id);
            let _prev2 = dep_sources.insert(mod_key, dep_src);
        }
    }

    let (output, parsed) = entry_output?;
    let sema = state.into_sema_result(output);

    // Re-lex the entry file for token data used by LSP features (semantic
    // tokens, find-name, etc.). The interner already has all symbols from
    // the graph-build phase, so this is cheap.
    let lexed = lex(entry_source, entry_file_id, interner, diags);

    Some((sema, parsed, entry_file_id, lexed, dep_sources))
}

/// Compute a stable string key for a dep module path relative to the project root.
///
/// For paths under the project root, produces e.g. `"lib/utils"` (no `.ms`).
/// For stdlib paths containing `std/`, strips everything before `std/`.
/// Falls back to the full path string for paths outside the project.
fn module_key(path: &Path, project_root: &Path) -> String {
    if let Ok(rel) = path.strip_prefix(project_root) {
        let s = rel.to_string_lossy();
        return s.trim_end_matches(".ms").to_owned();
    }
    // stdlib: extract from `std/` onwards
    let path_str = path.to_string_lossy();
    if let Some(idx) = path_str.find("std/") {
        return path_str[idx..].trim_end_matches(".ms").to_owned();
    }
    path_str.trim_end_matches(".ms").to_owned()
}

fn builtin_module_exports(node: &ModuleNode, state: &SharedAnalysisState) -> Vec<ExportBinding> {
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
                ty_params: vec![],
            },
            ExportBinding {
                name: ptr_def.name,
                ty: TypeIdx::from_raw(0),
                def_id: wk.ffi.ptr,
                ty_params: vec![],
            },
        ]
    } else {
        vec![]
    }
}

fn build_import_names(node: &ModuleNode, module_exports: &ModuleExportMap) -> ImportNames {
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
    module_exports: &ModuleExportMap,
    types: &mut Arena<Type>,
    interner: &Interner,
) -> HashMap<Symbol, TypeIdx> {
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
            let ty = types.alloc(Type::Record { fields, rest: None });
            let _prev = map.insert(import_sym, ty);
        }
    }
    map
}

fn build_dep_source(
    source: &str,
    _lexed: LexedSource,
    defs: &[DefInfo],
    file_id: FileId,
) -> DepSource {
    let mut def_spans: HashMap<Symbol, Span> = HashMap::new();
    for def in defs {
        if def.file_id == file_id {
            let _prev = def_spans.entry(def.name).or_insert(def.span);
        }
    }
    DepSource {
        source: source.to_owned(),
        def_spans,
    }
}
