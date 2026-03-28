use std::collections::HashMap;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::{fmt, fs, io, mem};

use music_ast::expr::{ExprKind, ImportKind};
use music_db::Db;
use music_shared::diag::Diag;
use music_shared::{Interner, SourceMap};
use music_lex::Lexer;
use music_parse::parse;

use crate::def::Visibility;
use crate::errors::ResolveError;
use crate::graph::{ExportInfo, ModuleExports, ModuleGraph, ModuleId};
use crate::loader::{ModuleLoader, ResolvedImport};
use crate::queries::{ResolutionMap, ResolveDb};

/// The result of resolving an entire project starting from an entrypoint.
pub struct ProjectResolution {
    pub graph: ModuleGraph,
    pub modules: HashMap<ModuleId, ModuleResult>,
    pub order: Vec<ModuleId>,
}

/// Per-module resolution output.
pub struct ModuleResult {
    pub db: Db,
    pub resolution: ResolutionMap,
    pub errors: Vec<ResolveError>,
    pub diagnostics: Vec<Diag>,
    pub has_errors: bool,
}

/// Errors that can occur during project-level resolution.
#[derive(Debug)]
pub enum ProjectError {
    Io { path: PathBuf, source: io::Error },
}

impl fmt::Display for ProjectError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::Io { path, source } => {
                write!(f, "failed to read `{}`: {source}", path.display())
            }
        }
    }
}

impl Error for ProjectError {}

/// Resolve an entire project starting from an entrypoint.
///
/// Recursively discovers and resolves all modules reachable from
/// `entrypoint`, building a dependency graph and processing modules
/// in topological order (dependencies first).
///
/// # Errors
///
/// Returns `ProjectError` if a source file cannot be read.
pub fn resolve_project(
    entrypoint: &Path,
    loader: &ModuleLoader,
) -> Result<ProjectResolution, ProjectError> {
    let mut graph = ModuleGraph::new();
    let mut modules: HashMap<ModuleId, ModuleResult> = HashMap::new();

    let _root = resolve_module_recursive(entrypoint, loader, &mut graph, &mut modules)?;

    let order = graph.topo_sort().unwrap_or_default();

    Ok(ProjectResolution {
        graph,
        modules,
        order,
    })
}

fn resolve_module_recursive(
    file: &Path,
    loader: &ModuleLoader,
    graph: &mut ModuleGraph,
    modules: &mut HashMap<ModuleId, ModuleResult>,
) -> Result<ModuleId, ProjectError> {
    let canonical = file.canonicalize().unwrap_or_else(|_| file.to_path_buf());
    let mod_id = graph.add_module(canonical.clone());

    if graph.is_loaded(mod_id) {
        return Ok(mod_id);
    }

    let source = fs::read_to_string(&canonical).map_err(|e| ProjectError::Io {
        path: canonical.clone(),
        source: e,
    })?;

    let (tokens, lex_errors) = Lexer::new(&source).lex();
    let mut interner = Interner::new();
    let (ast, parse_errors) = parse(&tokens, &source, &mut interner);
    let mut source_map = SourceMap::default();
    let source_id = source_map.add(&canonical, &source);
    let db = Db::new(ast, interner, source_map);

    let import_paths = extract_imports(&db);
    for (import_str, _kind) in &import_paths {
        if let Some(ResolvedImport::File(dep_path) | ResolvedImport::Git(dep_path)) =
            loader.resolve(import_str, &canonical)
        {
            let dep_id = resolve_module_recursive(&dep_path, loader, graph, modules)?;
            graph.add_edge(mod_id, dep_id);
        }
    }

    // Temporarily take the graph out so ResolveDb can own it.
    // ResolveDb needs ownership for resolve_import to check/update module state.
    let owned_graph = mem::take(graph);

    let mut resolve_db = ResolveDb::with_graph(db, loader.clone(), owned_graph, canonical);
    resolve_db.seed_builtins();
    resolve_db.resolve_module();

    let (db, resolution, errors, returned_graph) = resolve_db.finish_with_graph();

    let has_errors = !lex_errors.is_empty() || !parse_errors.is_empty() || !errors.is_empty();
    let mut diagnostics = Vec::new();
    for e in lex_errors {
        diagnostics.push(Diag::error(e.to_string()).with_label(e.span, source_id, ""));
    }
    for e in parse_errors {
        diagnostics.push(Diag::error(e.to_string()).with_label(e.span, source_id, ""));
    }
    for e in &errors {
        diagnostics.push(Diag::error(e.to_string()).with_label(e.span, source_id, ""));
    }

    *graph = returned_graph;

    let exports = collect_exports(&db.interner, &resolution);
    graph.set_exports(mod_id, exports);

    let _prev = modules.insert(
        mod_id,
        ModuleResult {
            db,
            resolution,
            errors,
            diagnostics,
            has_errors,
        },
    );

    Ok(mod_id)
}

/// Extract import paths from a parsed module's AST.
fn extract_imports(db: &Db) -> Vec<(String, ImportKind)> {
    let mut imports = Vec::new();
    for &expr_id in &db.ast.root {
        let expr = db.ast.exprs.get(expr_id);
        if let ExprKind::Import { path, kind } = &expr.kind {
            let path_str = db.interner.resolve(*path).to_owned();
            imports.push((path_str, kind.clone()));
        }
    }
    imports
}

/// Collect exported definitions from a resolved module.
fn collect_exports(interner: &Interner, resolution: &ResolutionMap) -> ModuleExports {
    let mut exports = HashMap::new();
    for (_idx, def) in &resolution.defs {
        if matches!(def.vis, Visibility::Exported | Visibility::Opaque) {
            let name = interner.resolve(def.name).to_owned();
            let _prev = exports.insert(
                name.clone(),
                ExportInfo {
                    name,
                    kind: def.kind,
                },
            );
        }
    }
    ModuleExports { exports }
}
