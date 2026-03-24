use std::collections::HashMap;
use std::error::Error;
use std::fmt;
use std::path::{Path, PathBuf};

use music_found::Symbol;

use crate::def::DefId;

/// Opaque identifier for a module in the dependency graph.
#[derive(Clone, Copy, PartialEq, Eq, Hash)]
pub struct ModuleId(u32);

impl ModuleId {
    /// Returns the underlying raw index.
    #[must_use]
    pub const fn raw(self) -> u32 {
        self.0
    }

    /// Returns the index as `usize` for Vec indexing.
    ///
    /// # Panics
    ///
    /// Panics if `u32` does not fit in `usize` (impossible on >=32-bit platforms).
    #[must_use]
    fn idx(self) -> usize {
        usize::try_from(self.0).expect("u32 always fits in usize")
    }
}

impl fmt::Debug for ModuleId {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "ModuleId({})", self.0)
    }
}

/// The public names exported by a resolved module.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ModuleExports {
    pub exports: HashMap<Symbol, DefId>,
}

impl ModuleExports {
    #[must_use]
    pub fn new() -> Self {
        Self {
            exports: HashMap::new(),
        }
    }
}

impl Default for ModuleExports {
    fn default() -> Self {
        Self::new()
    }
}

/// Whether a module is currently being loaded or has finished resolving.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ModuleState {
    /// Module file entered but not yet finished resolving (cycle detection).
    Loading,
    /// Module fully resolved with its public exports.
    Loaded(ModuleExports),
}

/// Per-module metadata stored in the graph.
#[derive(Debug)]
pub struct ModuleNode {
    pub path: PathBuf,
    pub state: ModuleState,
    /// Modules this one imports (outgoing dependency edges).
    pub deps: Vec<ModuleId>,
}

/// Dependency graph tracking all modules and their relationships.
///
/// Modules are identified by `ModuleId` and form a directed acyclic graph
/// rooted at the project entrypoint. The graph enforces that no cycles exist
/// (F#-style strict ban on circular dependencies).
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

    /// Register a module by its canonical path, returning its `ModuleId`.
    /// If the module is already registered, returns the existing id.
    ///
    /// # Panics
    ///
    /// Panics if the graph already contains `u32::MAX` modules.
    pub fn add_module(&mut self, path: PathBuf) -> ModuleId {
        if let Some(&id) = self.path_to_id.get(&path) {
            return id;
        }
        let raw = u32::try_from(self.nodes.len())
            .expect("module graph overflow: exceeded u32::MAX modules");
        let id = ModuleId(raw);
        self.nodes.push(ModuleNode {
            path: path.clone(),
            state: ModuleState::Loading,
            deps: Vec::new(),
        });
        let _prev = self.path_to_id.insert(path, id);
        id
    }

    /// Record a dependency: `from` imports `to`.
    pub fn add_edge(&mut self, from: ModuleId, to: ModuleId) {
        let node = &mut self.nodes[from.idx()];
        if !node.deps.contains(&to) {
            node.deps.push(to);
        }
    }

    /// Check whether a module is in the `Loading` state (cycle detection).
    #[must_use]
    pub fn is_loading(&self, id: ModuleId) -> bool {
        matches!(self.nodes[id.idx()].state, ModuleState::Loading)
    }

    /// Check whether a module has been fully loaded.
    #[must_use]
    pub fn is_loaded(&self, id: ModuleId) -> bool {
        matches!(self.nodes[id.idx()].state, ModuleState::Loaded(_))
    }

    /// Mark a module as currently being loaded (for cycle detection).
    pub fn mark_loading(&mut self, id: ModuleId) {
        self.nodes[id.idx()].state = ModuleState::Loading;
    }

    /// Mark a module as fully loaded with its resolved exports.
    pub fn mark_loaded(&mut self, id: ModuleId, exports: ModuleExports) {
        self.nodes[id.idx()].state = ModuleState::Loaded(exports);
    }

    /// Return the exports for a fully loaded module.
    #[must_use]
    pub fn get_exports(&self, id: ModuleId) -> Option<&ModuleExports> {
        match &self.nodes[id.idx()].state {
            ModuleState::Loaded(exports) => Some(exports),
            ModuleState::Loading => None,
        }
    }

    /// Set the exports for a module (convenience for `mark_loaded`).
    pub fn set_exports(&mut self, id: ModuleId, exports: ModuleExports) {
        self.mark_loaded(id, exports);
    }

    /// Look up a module id by its canonical path.
    #[must_use]
    pub fn lookup(&self, path: &Path) -> Option<ModuleId> {
        self.path_to_id.get(path).copied()
    }

    /// Return the path for a module.
    #[must_use]
    pub fn path(&self, id: ModuleId) -> &Path {
        &self.nodes[id.idx()].path
    }

    /// Return the dependency list for a module.
    #[must_use]
    pub fn deps(&self, id: ModuleId) -> &[ModuleId] {
        &self.nodes[id.idx()].deps
    }

    /// Number of modules in the graph.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.nodes.len()
    }

    /// Whether the graph is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.nodes.is_empty()
    }

    /// Topological sort using Kahn's algorithm.
    ///
    /// Returns modules in dependency order (dependencies before dependants).
    /// If a cycle exists, returns `Err` with the ids of modules involved.
    ///
    /// # Errors
    ///
    /// Returns `CycleError` if the graph contains a cycle.
    ///
    /// # Panics
    ///
    /// Panics if internal index conversion fails (unreachable on >=32-bit platforms).
    pub fn topo_sort(&self) -> Result<Vec<ModuleId>, CycleError> {
        let n = self.nodes.len();
        let mut in_degree = vec![0u32; n];

        for node in &self.nodes {
            for &dep in &node.deps {
                in_degree[dep.idx()] += 1;
            }
        }

        let mut queue: Vec<ModuleId> = in_degree
            .iter()
            .enumerate()
            .filter(|(_, deg)| **deg == 0)
            .map(|(i, _)| ModuleId(u32::try_from(i).expect("already bounded by add_module")))
            .collect();

        let mut order = Vec::with_capacity(n);

        while let Some(id) = queue.pop() {
            order.push(id);
            for &dep in &self.nodes[id.idx()].deps {
                in_degree[dep.idx()] -= 1;
                if in_degree[dep.idx()] == 0 {
                    queue.push(dep);
                }
            }
        }

        if order.len() == n {
            order.reverse();
            Ok(order)
        } else {
            let cycle_ids: Vec<ModuleId> = (0..n)
                .filter(|i| {
                    !order.contains(&ModuleId(
                        u32::try_from(*i).expect("already bounded by add_module"),
                    ))
                })
                .map(|i| ModuleId(u32::try_from(i).expect("already bounded by add_module")))
                .collect();
            Err(CycleError { modules: cycle_ids })
        }
    }

    /// Find the cycle path starting from a given module using DFS.
    /// Returns the chain of module paths forming the cycle.
    #[must_use]
    pub fn find_cycle_chain(&self, start: ModuleId) -> Vec<PathBuf> {
        let mut visited = vec![false; self.nodes.len()];
        let mut on_stack = vec![false; self.nodes.len()];
        let mut path = Vec::new();

        if self.dfs_cycle(start, &mut visited, &mut on_stack, &mut path) {
            path.push(self.nodes[start.idx()].path.clone());
            path.reverse();
            path
        } else {
            Vec::new()
        }
    }

    fn dfs_cycle(
        &self,
        current: ModuleId,
        visited: &mut [bool],
        on_stack: &mut [bool],
        path: &mut Vec<PathBuf>,
    ) -> bool {
        let ci = current.idx();
        visited[ci] = true;
        on_stack[ci] = true;

        for &dep in &self.nodes[ci].deps {
            let di = dep.idx();
            if !visited[di] {
                if self.dfs_cycle(dep, visited, on_stack, path) {
                    path.push(self.nodes[ci].path.clone());
                    return true;
                }
            } else if on_stack[di] {
                path.push(self.nodes[di].path.clone());
                path.push(self.nodes[ci].path.clone());
                return true;
            }
        }

        on_stack[ci] = false;
        false
    }
}

impl Default for ModuleGraph {
    fn default() -> Self {
        Self::new()
    }
}

/// Returned by `topo_sort` when the graph contains a cycle.
#[derive(Debug, Clone)]
pub struct CycleError {
    /// Module ids that participate in one or more cycles.
    pub modules: Vec<ModuleId>,
}

impl fmt::Display for CycleError {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(
            f,
            "cyclic dependency detected among {} modules",
            self.modules.len()
        )
    }
}

impl Error for CycleError {}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
