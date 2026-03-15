//! Frontend pipeline: lex -> parse -> type-check.

use std::collections::HashMap;
use std::fs;
use std::io::{IsTerminal, stderr};
use std::path::Path;

use musi_manifest::MusiManifest;
use music_ast::ParsedModule;
use music_emit::emit;
use music_lex::lex;
use music_parse::parse;
use music_resolve::graph::ModuleId;
use music_resolve::{ModuleGraph, ModuleNode, build_module_graph};
use music_sema::{
    ExportBinding, ImportNames, SemaResult, SharedAnalysisState, Type, TypeIdx, analyze,
    collect_exports,
};
use music_sema::types::RecordField;
use music_shared::{DiagnosticBag, FileId, Interner, SourceDb};

use crate::resolve_config;

/// A dependency module processed during multi-file compilation.
pub struct DepModule {
    pub parsed: ParsedModule,
    pub resolution: music_sema::ResolutionMap,
    pub expr_types: HashMap<music_ast::ExprIdx, music_sema::TypeIdx>,
    pub file_id: FileId,
}

/// Output of a successful frontend run.
pub struct FrontendOutput {
    pub sema: SemaResult,
    pub parsed: ParsedModule,
    pub dep_modules: Vec<DepModule>,
    pub interner: Interner,
    pub source_db: SourceDb,
    pub file_id: FileId,
}

/// Runs lex -> parse -> sema on `path`.
///
/// Prints diagnostics to stderr. Returns `Err(())` if any errors occurred.
pub fn run_frontend(path: &Path) -> Result<FrontendOutput, ()> {
    let source = match fs::read_to_string(path) {
        Ok(s) => s,
        Err(e) => {
            eprintln!("error: {}: {e}", path.display());
            return Err(());
        }
    };

    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let file_id = source_db.add(path.display().to_string(), source.as_str());
    let lexed = lex(&source, file_id, &mut interner, &mut diags);
    let parsed = parse(&lexed.tokens, file_id, &mut diags, &mut interner);
    let sema = analyze(&parsed, &mut interner, file_id, &mut diags);

    render_diagnostics(&diags, &source_db);

    if diags.has_errors() {
        Err(())
    } else {
        Ok(FrontendOutput {
            sema,
            parsed,
            dep_modules: vec![],
            interner,
            source_db,
            file_id,
        })
    }
}

/// Runs the multi-file frontend: builds a module graph, topologically sorts it,
/// and runs sema on each module in dependency order.
///
/// The entry module (root file) is processed last. Other modules contribute
/// their exported types to downstream modules via record types.
///
/// Prints diagnostics to stderr. Returns `Err(())` if any errors occurred.
pub fn run_frontend_multi(
    path: &Path,
    manifest: Option<&MusiManifest>,
    project_root_hint: Option<&Path>,
) -> Result<FrontendOutput, ()> {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let project_root = project_root_hint.map_or_else(
        || {
            path.parent()
                .unwrap_or_else(|| Path::new("."))
                .to_path_buf()
        },
        Path::to_path_buf,
    );

    let config = resolve_config::build(&project_root, manifest);
    let mut parsed_modules: HashMap<ModuleId, ParsedModule> = HashMap::new();

    let graph = match build_module_graph(
        path,
        &config,
        &mut interner,
        &mut source_db,
        &mut diags,
        &mut parsed_modules,
        None,
    ) {
        Ok(g) => g,
        Err(e) => {
            eprintln!("error: {e}");
            render_diagnostics(&diags, &source_db);
            return Err(());
        }
    };

    let order = match graph.toposort() {
        Ok(o) => o,
        Err(e) => {
            eprintln!("error: {e}");
            render_diagnostics(&diags, &source_db);
            return Err(());
        }
    };

    let result = run_sema_in_order(
        &graph,
        &order,
        &mut parsed_modules,
        &mut interner,
        &mut diags,
    );

    render_diagnostics(&diags, &source_db);

    if diags.has_errors() {
        return Err(());
    }

    let (sema, parsed, dep_modules) = result.ok_or(())?;
    let file_id = graph.get(ModuleId(0)).file_id;
    Ok(FrontendOutput {
        sema,
        parsed,
        dep_modules,
        interner,
        source_db,
        file_id,
    })
}

fn run_sema_in_order(
    graph: &ModuleGraph,
    order: &[ModuleId],
    parsed_modules: &mut HashMap<ModuleId, ParsedModule>,
    interner: &mut Interner,
    diags: &mut DiagnosticBag,
) -> Option<(SemaResult, ParsedModule, Vec<DepModule>)> {
    let mut state = SharedAnalysisState::new(interner);
    let mut module_exports: HashMap<ModuleId, Vec<ExportBinding>> = HashMap::new();
    let mut dep_modules: Vec<DepModule> = Vec::new();

    let entry_id = ModuleId(0);
    let mut entry_output: Option<(music_sema::ModuleSemaOutput, ParsedModule)> = None;

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
            dep_modules.push(DepModule {
                parsed,
                resolution: output.resolution,
                expr_types: output.expr_types,
                file_id,
            });
        }
    }

    let (output, parsed) = entry_output?;
    Some((state.into_sema_result(output), parsed, dep_modules))
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
) -> HashMap<music_shared::Symbol, TypeIdx> {
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

/// Runs bytecode emission.
///
/// Returns the raw `.msbc` bytes on success, or `Err(())` after printing
/// the error to stderr.
pub fn run_backend(out: &mut FrontendOutput) -> Result<Vec<u8>, ()> {
    if !out.dep_modules.is_empty() {
        for dep in &out.dep_modules {
            let dep_src = out.source_db.source(dep.file_id);
            eprintln!(
                "note: dependency module ({} bytes, {} stmts, {} names resolved, {} types)",
                dep_src.len(),
                dep.parsed.stmts.len(),
                dep.resolution.pat_defs.len(),
                dep.expr_types.len(),
            );
        }
    }
    match emit(&out.parsed, &out.sema, &mut out.interner, out.file_id) {
        Ok(emit_out) => Ok(emit_out.bytes),
        Err(e) => {
            render_diagnostics(&DiagnosticBag::new(), &out.source_db);
            eprintln!("error: {e}");
            Err(())
        }
    }
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

fn render_diagnostics(diags: &DiagnosticBag, source_db: &SourceDb) {
    let use_color = stderr().is_terminal();
    for diag in diags.iter() {
        eprintln!("{}", diag.render_rich(source_db, use_color));
    }
}
