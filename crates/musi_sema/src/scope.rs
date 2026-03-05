//! Lexical scope tree for name resolution.

use std::collections::HashMap;

use musi_shared::Symbol;

use crate::def::DefId;

// ---------------------------------------------------------------------------
// ScopeId
// ---------------------------------------------------------------------------

/// A handle to a single scope node in the [`ScopeTree`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

// ---------------------------------------------------------------------------
// Scope (private node)
// ---------------------------------------------------------------------------

struct Scope {
    parent: Option<ScopeId>,
    bindings: HashMap<Symbol, DefId>,
}

// ---------------------------------------------------------------------------
// ScopeTree
// ---------------------------------------------------------------------------

/// A tree of lexical scopes.
///
/// Each scope stores a map from name → [`DefId`] and a parent link.
/// Lookup walks the chain from inner to outer until a match is found.
pub struct ScopeTree {
    scopes: Vec<Scope>,
}

impl ScopeTree {
    /// Creates an empty scope tree.
    #[must_use]
    pub fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    /// Creates and returns the root (module-level) scope.
    #[must_use]
    pub fn push_root(&mut self) -> ScopeId {
        self.push_scope(None)
    }

    /// Creates and returns a child scope of `parent`.
    #[must_use]
    pub fn push_child(&mut self, parent: ScopeId) -> ScopeId {
        self.push_scope(Some(parent))
    }

    /// Defines `name → def_id` in `scope`.
    ///
    /// Returns the previous definition if the name was already bound in this
    /// exact scope (not a parent), so callers can diagnose shadowing.
    pub fn define(&mut self, scope: ScopeId, name: Symbol, def_id: DefId) -> Option<DefId> {
        let idx = usize::try_from(scope.0).expect("ScopeId in range");
        self.scopes
            .get_mut(idx)
            .expect("ScopeId is valid")
            .bindings
            .insert(name, def_id)
    }

    /// Looks up `name` starting from `scope` and walking towards the root.
    ///
    /// Returns the first [`DefId`] found, or `None` if the name is unbound.
    #[must_use]
    pub fn lookup(&self, scope: ScopeId, name: Symbol) -> Option<DefId> {
        let mut cur = scope;
        loop {
            let idx = usize::try_from(cur.0).expect("ScopeId in range");
            let s = self.scopes.get(idx).expect("ScopeId is valid");
            if let Some(&def) = s.bindings.get(&name) {
                return Some(def);
            }
            cur = s.parent?;
        }
    }

    /// Looks up `name` in `scope` only (not parents).
    #[must_use]
    pub fn lookup_local(&self, scope: ScopeId, name: Symbol) -> Option<DefId> {
        let idx = usize::try_from(scope.0).expect("ScopeId in range");
        self.scopes
            .get(idx)
            .expect("ScopeId is valid")
            .bindings
            .get(&name)
            .copied()
    }

    /// Returns all names visible from `scope` (this scope and all ancestors).
    #[must_use]
    pub fn visible_names(&self, scope: ScopeId) -> Vec<Symbol> {
        let mut names = Vec::new();
        let mut cur = scope;
        loop {
            let idx = usize::try_from(cur.0).expect("ScopeId in range");
            let s = self.scopes.get(idx).expect("ScopeId is valid");
            names.extend(s.bindings.keys().copied());
            match s.parent {
                Some(p) => cur = p,
                None => break,
            }
        }
        names
    }

    fn push_scope(&mut self, parent: Option<ScopeId>) -> ScopeId {
        let id = ScopeId(u32::try_from(self.scopes.len()).expect("scope count overflow"));
        self.scopes.push(Scope {
            parent,
            bindings: HashMap::new(),
        });
        id
    }
}

impl Default for ScopeTree {
    fn default() -> Self {
        Self::new()
    }
}
