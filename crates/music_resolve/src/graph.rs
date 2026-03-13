//! Module graph: DAG construction, cycle detection, and topological sort.

#[cfg(test)]
mod tests;

use std::collections::{HashMap, HashSet, VecDeque};
use std::fs;
use std::hash::BuildHasher;
use std::path::{Path, PathBuf};

use music_ast::{AstArenas, Expr, ExprIdx, ParsedModule};
use music_lex::lex;
use music_parse::parse;
use music_shared::{DiagnosticBag, FileId, Interner, SourceDb, Span, Symbol};

use crate::error::ResolveError;
use crate::resolver::{ResolverConfig, resolve_import};
use crate::specifier::parse_specifier;

/// Opaque module identifier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(pub u32);

/// A single module in the graph.
pub struct ModuleNode {
    pub id: ModuleId,
    pub path: PathBuf,
    pub source: String,
    pub file_id: FileId,
    pub imports: Vec<(ModuleId, Symbol)>,
    /// `true` for `musi:*` built-in modules whose exports are injected by the pipeline.
    pub builtin: bool,
}

/// A directed acyclic graph of modules.
pub struct ModuleGraph {
    nodes: Vec<ModuleNode>,
    path_to_id: HashMap<PathBuf, ModuleId>,
}

impl ModuleGraph {
    #[must_use]
    pub fn new() -> Self {
        Self {
            nodes: Vec::new(),
            path_to_id: HashMap::new(),
        }
    }

    /// Adds a module or returns its existing ID if already present.
    ///
    /// # Panics
    ///
    /// Panics if the number of modules exceeds `u32::MAX`.
    pub fn add_module(&mut self, path: PathBuf, source: String, file_id: FileId) -> ModuleId {
        if let Some(&existing) = self.path_to_id.get(&path) {
            return existing;
        }
        let id = ModuleId(u32::try_from(self.nodes.len()).expect("too many modules"));
        let _prev = self.path_to_id.insert(path.clone(), id);
        self.nodes.push(ModuleNode {
            id,
            path,
            source,
            file_id,
            imports: Vec::new(),
            builtin: false,
        });
        id
    }

    /// Adds a built-in module (no source file) or returns its existing ID.
    pub fn add_builtin_module(&mut self, path: PathBuf, file_id: FileId) -> ModuleId {
        if let Some(&existing) = self.path_to_id.get(&path) {
            return existing;
        }
        let id = ModuleId(u32::try_from(self.nodes.len()).expect("too many modules"));
        let _prev = self.path_to_id.insert(path.clone(), id);
        self.nodes.push(ModuleNode {
            id,
            path,
            source: String::new(),
            file_id,
            imports: Vec::new(),
            builtin: true,
        });
        id
    }

    /// Adds a directed edge from `from` to `to`.
    ///
    /// # Panics
    ///
    /// Panics if `from` is out of range.
    pub fn add_edge(&mut self, from: ModuleId, to: ModuleId, import_sym: Symbol) {
        let idx = module_idx(from);
        self.nodes[idx].imports.push((to, import_sym));
    }

    /// Returns `Some(id)` if this canonical path is already in the graph.
    #[must_use]
    pub fn lookup(&self, path: &Path) -> Option<ModuleId> {
        self.path_to_id.get(path).copied()
    }

    /// Returns a reference to the module with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if `id` is out of range.
    #[must_use]
    pub fn get(&self, id: ModuleId) -> &ModuleNode {
        &self.nodes[module_idx(id)]
    }

    /// Returns a mutable reference to the module with the given ID.
    ///
    /// # Panics
    ///
    /// Panics if `id` is out of range.
    pub fn get_mut(&mut self, id: ModuleId) -> &mut ModuleNode {
        let idx = module_idx(id);
        &mut self.nodes[idx]
    }

    /// Returns the number of modules in the graph.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Returns `true` if the graph contains no modules.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Topologically sorts the graph (leaves first).
    ///
    /// Returns the module IDs in an order such that if module A imports module B,
    /// B appears before A in the result.
    ///
    /// # Errors
    ///
    /// Returns [`ResolveError::CircularImport`] if the graph contains a cycle.
    pub fn toposort(&self) -> Result<Vec<ModuleId>, ResolveError> {
        let n = self.nodes.len();

        // Build reverse adjacency: for each module, count how many modules
        // depend on it (i.e. import it). We want leaves (modules with no
        // dependents importing them... actually no) — we want to process
        // importees before importers. Use reverse-edge Kahn's: count how
        // many modules each module *imports* (out-degree in original graph),
        // then start from modules that import nothing (leaves).
        let mut import_count = vec![0usize; n];
        let mut reverse_edges: Vec<Vec<ModuleId>> = vec![Vec::new(); n];
        for node in &self.nodes {
            import_count[module_idx(node.id)] = node.imports.len();
            for &(target, _) in &node.imports {
                reverse_edges[module_idx(target)].push(node.id);
            }
        }

        let mut queue: VecDeque<ModuleId> = VecDeque::new();
        for node in &self.nodes {
            if import_count[module_idx(node.id)] == 0 {
                queue.push_back(node.id);
            }
        }

        let mut order = Vec::with_capacity(n);
        while let Some(id) = queue.pop_front() {
            order.push(id);
            for &dependent in &reverse_edges[module_idx(id)] {
                import_count[module_idx(dependent)] -= 1;
                if import_count[module_idx(dependent)] == 0 {
                    queue.push_back(dependent);
                }
            }
        }

        if order.len() < n {
            let cycle_desc = detect_cycle_path(self);
            return Err(ResolveError::CircularImport {
                cycle: Box::from(cycle_desc),
            });
        }

        Ok(order)
    }
}

impl Default for ModuleGraph {
    fn default() -> Self {
        Self::new()
    }
}

fn module_idx(id: ModuleId) -> usize {
    usize::try_from(id.0).expect("module id fits in usize")
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum Color {
    White,
    Gray,
    Black,
}

fn detect_cycle_path(graph: &ModuleGraph) -> String {
    let n = graph.nodes.len();

    let mut color = vec![Color::White; n];
    let mut parent = vec![None::<usize>; n];

    for start in 0..n {
        if color[start] != Color::White {
            continue;
        }
        let mut stack = vec![(start, 0usize)];
        color[start] = Color::Gray;

        while let Some((node, edge_idx)) = stack.last_mut() {
            let imports = &graph.nodes[*node].imports;
            if *edge_idx < imports.len() {
                let (target, _) = imports[*edge_idx];
                *edge_idx += 1;
                let t = module_idx(target);
                match color[t] {
                    Color::White => {
                        color[t] = Color::Gray;
                        parent[t] = Some(*node);
                        stack.push((t, 0));
                    }
                    Color::Gray => {
                        return format_cycle_path(graph, &parent, *node, t);
                    }
                    Color::Black => {}
                }
            } else {
                let (popped, _) = stack.pop().expect("non-empty stack");
                color[popped] = Color::Black;
            }
        }
    }

    "unknown cycle".to_owned()
}

fn format_cycle_path(
    graph: &ModuleGraph,
    parent: &[Option<usize>],
    from: usize,
    to: usize,
) -> String {
    let mut path = vec![to];
    let mut cur = from;
    while cur != to {
        path.push(cur);
        cur = parent[cur].unwrap_or(to);
    }
    path.push(to);
    path.reverse();
    path.iter()
        .map(|&i| {
            graph.nodes[i]
                .path
                .file_name()
                .map_or("?", |n| n.to_str().unwrap_or("?"))
                .to_owned()
        })
        .collect::<Vec<_>>()
        .join(" -> ")
}

fn collect_import_paths(module: &ParsedModule) -> Vec<(Symbol, Span)> {
    let mut imports = Vec::new();
    for stmt in &module.stmts {
        collect_imports_from_expr(stmt.expr, &module.arenas, &mut imports);
    }
    imports
}

fn collect_imports_from_expr(expr_idx: ExprIdx, arenas: &AstArenas, out: &mut Vec<(Symbol, Span)>) {
    match &arenas.exprs[expr_idx] {
        Expr::Import { path, span, .. } => {
            out.push((*path, *span));
        }
        Expr::Let { fields, body, .. } => {
            if let Some(v) = fields.value {
                collect_imports_from_expr(v, arenas, out);
            }
            if let Some(b) = body {
                collect_imports_from_expr(*b, arenas, out);
            }
        }
        Expr::Binding { fields, .. } => {
            if let Some(v) = fields.value {
                collect_imports_from_expr(v, arenas, out);
            }
        }
        Expr::Block { stmts, tail, .. } => {
            for &stmt_expr in stmts {
                collect_imports_from_expr(stmt_expr, arenas, out);
            }
            if let Some(t) = tail {
                collect_imports_from_expr(*t, arenas, out);
            }
        }
        Expr::Annotated { inner, .. } => {
            collect_imports_from_expr(*inner, arenas, out);
        }
        _ => {}
    }
}

/// Builds a module graph starting from `root`.
///
/// Reads, lexes, and parses each discovered module. The parsed modules are
/// returned alongside the graph via the `parsed_modules` output parameter
/// to avoid double-parsing.
///
/// # Errors
///
/// Returns [`ResolveError`] on I/O failures, unresolved imports, or cycles.
pub fn build_module_graph<S: BuildHasher>(
    root: &Path,
    config: &ResolverConfig,
    interner: &mut Interner,
    source_db: &mut SourceDb,
    diags: &mut DiagnosticBag,
    parsed_modules: &mut HashMap<ModuleId, ParsedModule, S>,
    source_override: Option<&str>,
) -> Result<ModuleGraph, ResolveError> {
    let mut graph = ModuleGraph::new();

    let canonical_root = canonicalize_path(root)?;
    let root_source = match source_override {
        Some(src) => src.to_owned(),
        None => read_source(&canonical_root)?,
    };
    let root_file_id = source_db.add(canonical_root.display().to_string(), &root_source);
    let root_id = graph.add_module(canonical_root, root_source, root_file_id);

    let mut work_queue: VecDeque<ModuleId> = VecDeque::new();
    work_queue.push_back(root_id);
    let mut visited: HashSet<ModuleId> = HashSet::new();
    let _inserted = visited.insert(root_id);

    while let Some(current_id) = work_queue.pop_front() {
        let node = graph.get(current_id);
        let source = node.source.clone();
        let file_id = node.file_id;
        let current_path = node.path.clone();

        let lexed = lex(&source, file_id, interner, diags);
        let parsed = parse(&lexed.tokens, file_id, diags, interner);

        let import_paths = collect_import_paths(&parsed);

        let _prev = parsed_modules.insert(current_id, parsed);

        let mut ctx = ImportProcessCtx {
            config,
            interner,
            source_db,
            diags,
            visited: &mut visited,
            work_queue: &mut work_queue,
        };

        for (sym, span) in import_paths {
            process_import(
                &mut graph,
                &mut ctx,
                current_id,
                sym,
                span,
                file_id,
                &current_path,
            );
        }
    }

    Ok(graph)
}

struct ImportProcessCtx<'a> {
    config: &'a ResolverConfig,
    interner: &'a Interner,
    source_db: &'a mut SourceDb,
    diags: &'a mut DiagnosticBag,
    visited: &'a mut HashSet<ModuleId>,
    work_queue: &'a mut VecDeque<ModuleId>,
}

fn process_import(
    graph: &mut ModuleGraph,
    ctx: &mut ImportProcessCtx<'_>,
    current_id: ModuleId,
    sym: Symbol,
    span: Span,
    file_id: FileId,
    current_path: &Path,
) {
    let full = ctx.interner.resolve(sym);
    let raw = full
        .strip_prefix('"')
        .and_then(|s| s.strip_suffix('"'))
        .unwrap_or(full);
    let specifier = match parse_specifier(raw) {
        Ok(s) => s,
        Err(e) => {
            let _d = ctx.diags.report(&e, span, file_id);
            return;
        }
    };

    // Built-in modules (e.g. musi:ffi) skip filesystem resolution.
    if specifier.scheme == crate::specifier::ImportScheme::Musi
        && crate::builtin::is_builtin_module(&specifier.module_path)
    {
        let sentinel = PathBuf::from(format!("<musi:{}>", specifier.module_path));
        let target_id = if let Some(existing) = graph.lookup(&sentinel) {
            existing
        } else {
            let builtin_file_id = ctx
                .source_db
                .add(sentinel.display().to_string(), "");
            graph.add_builtin_module(sentinel, builtin_file_id)
        };
        graph.add_edge(current_id, target_id, sym);
        return;
    }

    let resolved_path = match resolve_import(&specifier, current_path, ctx.config) {
        Ok(p) => p,
        Err(e) => {
            let _d = ctx.diags.report(&e, span, file_id);
            return;
        }
    };

    let canonical = match canonicalize_path(&resolved_path) {
        Ok(c) => c,
        Err(e) => {
            let _d = ctx.diags.report(&e, span, file_id);
            return;
        }
    };

    let target_id = if let Some(existing) = graph.lookup(&canonical) {
        existing
    } else {
        let target_source = match read_source(&canonical) {
            Ok(s) => s,
            Err(e) => {
                let _d = ctx.diags.report(&e, span, file_id);
                return;
            }
        };
        let target_file_id = ctx
            .source_db
            .add(canonical.display().to_string(), &target_source);
        graph.add_module(canonical, target_source, target_file_id)
    };

    graph.add_edge(current_id, target_id, sym);

    if ctx.visited.insert(target_id) {
        ctx.work_queue.push_back(target_id);
    }
}

fn canonicalize_path(path: &Path) -> Result<PathBuf, ResolveError> {
    fs::canonicalize(path).map_err(|_| ResolveError::ModuleNotFound {
        path: Box::from(path.display().to_string()),
    })
}

fn read_source(path: &Path) -> Result<String, ResolveError> {
    fs::read_to_string(path).map_err(|e| ResolveError::Io {
        path: Box::from(path.display().to_string()),
        source: e,
    })
}
