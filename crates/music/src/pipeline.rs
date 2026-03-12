//! Frontend pipeline: lex → parse → type-check.

use std::collections::HashMap;
use std::fs;
use std::io::{IsTerminal, stderr};
use std::path::Path;

use musi_manifest::MusiManifest;
use music_ast::ParsedModule;
use music_emit::emit;
use music_ir::lower::lower as lower_ir;
use music_lex::lex;
use music_parse::parse;
use music_resolve::graph::ModuleId;
use music_resolve::{ModuleGraph, ModuleNode, build_module_graph};
use music_sema::{SemaResult, TypeIdx, analyze, analyze_with_imports};
use music_shared::{DiagnosticBag, Interner, SourceDb, Symbol};

use crate::resolve_config;

/// Output of a successful frontend run.
pub struct FrontendOutput {
    /// The semantic analysis result.
    pub sema: SemaResult,
    /// The parsed module (needed for IR lowering).
    pub parsed: ParsedModule,
    /// The symbol interner (needed for IR lowering and emit).
    pub interner: Interner,
}

/// Runs lex → parse → sema on `path`.
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
    let parsed = parse(&lexed.tokens, file_id, &mut diags, &interner);
    let sema = analyze(&parsed, &mut interner, file_id, &mut diags);

    render_diagnostics(&diags, &source_db);

    if diags.has_errors() {
        Err(())
    } else {
        Ok(FrontendOutput {
            sema,
            parsed,
            interner,
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
) -> Result<FrontendOutput, ()> {
    let mut interner = Interner::new();
    let mut source_db = SourceDb::new();
    let mut diags = DiagnosticBag::new();

    let project_root = path
        .parent()
        .unwrap_or_else(|| Path::new("."))
        .to_path_buf();

    let config = resolve_config::build(&project_root, manifest);
    let mut parsed_modules: HashMap<ModuleId, ParsedModule> = HashMap::new();

    let graph = match build_module_graph(
        path,
        &config,
        &mut interner,
        &mut source_db,
        &mut diags,
        &mut parsed_modules,
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

    let (sema, parsed) = result.ok_or(())?;
    Ok(FrontendOutput {
        sema,
        parsed,
        interner,
    })
}

fn run_sema_in_order(
    graph: &ModuleGraph,
    order: &[ModuleId],
    parsed_modules: &mut HashMap<ModuleId, ParsedModule>,
    interner: &mut Interner,
    diags: &mut DiagnosticBag,
) -> Option<(SemaResult, ParsedModule)> {
    let mut module_sema: HashMap<ModuleId, SemaResult> = HashMap::new();

    let entry_id = ModuleId(0);
    let mut entry_result: Option<(SemaResult, ParsedModule)> = None;

    for &module_id in order {
        let node = graph.get(module_id);
        let file_id = node.file_id;

        let import_types = build_import_types_for_module(node, &module_sema);

        let Some(parsed) = parsed_modules.remove(&module_id) else {
            continue;
        };

        let sema = analyze_with_imports(&parsed, interner, file_id, diags, &import_types);

        if module_id == entry_id {
            entry_result = Some((sema, parsed));
        } else {
            let _prev = module_sema.insert(module_id, sema);
        }
    }

    entry_result
}

fn build_import_types_for_module(
    _node: &ModuleNode,
    _module_sema: &HashMap<ModuleId, SemaResult>,
) -> HashMap<Symbol, TypeIdx> {
    // Cross-module type sharing blocked on shared type arena (imports type as Unit).
    HashMap::new()
}

/// Runs IR lowering and bytecode emission.
///
/// Returns the raw `.msbc` bytes on success, or `Err(())` after printing
/// the error to stderr.
pub fn run_backend(out: &FrontendOutput) -> Result<Vec<u8>, ()> {
    let ir = match lower_ir(&out.parsed, &out.sema, &out.interner) {
        Ok(m) => m,
        Err(e) => {
            eprintln!("error: {e}");
            return Err(());
        }
    };
    match emit(&ir, &out.interner) {
        Ok(emit_out) => Ok(emit_out.bytes),
        Err(e) => {
            eprintln!("error: {e}");
            Err(())
        }
    }
}

fn render_diagnostics(diags: &DiagnosticBag, source_db: &SourceDb) {
    let use_color = stderr().is_terminal();
    for diag in diags.iter() {
        eprintln!("{}", diag.render_rich(source_db, use_color));
    }
}
