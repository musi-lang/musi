use std::collections::HashMap;
use std::error::Error;
use std::path::{Path, PathBuf};
use std::{fmt, fs, io, mem};

use music_ast::expr::{ExprKind, ImportKind};
use music_db::Db;
use music_shared::{Interner, SourceMap};
use music_lex::Lexer;
use music_parse::parse;

use crate::def::Visibility;
use crate::errors::ResolveError;
use crate::graph::{ModuleExports, ModuleGraph, ModuleId};
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

    let (tokens, _lex_errors) = Lexer::new(&source).lex();
    let mut interner = Interner::new();
    let (ast, _parse_errors) = parse(&tokens, &source, &mut interner);
    let db = Db::new(ast, interner, SourceMap::default());

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

    let resolve_loader = ModuleLoader::new(loader.root().to_path_buf());
    let mut resolve_db = ResolveDb::with_graph(db, resolve_loader, owned_graph, canonical);
    resolve_db.seed_builtins();
    resolve_db.resolve_module();

    let (db, resolution, errors, returned_graph) = resolve_db.finish_with_graph();

    *graph = returned_graph;

    let exports = collect_exports(&resolution);
    graph.set_exports(mod_id, exports);

    let _prev = modules.insert(
        mod_id,
        ModuleResult {
            db,
            resolution,
            errors,
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
fn collect_exports(resolution: &ResolutionMap) -> ModuleExports {
    let mut exports = HashMap::new();
    for (idx, def) in &resolution.defs {
        if matches!(def.vis, Visibility::Exported | Visibility::Opaque) {
            let _prev = exports.insert(def.name, idx);
        }
    }
    ModuleExports { exports }
}
