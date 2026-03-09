//! Lexical scope tree for name resolution.

#[cfg(test)]
mod tests;

use std::collections::HashMap;

use music_shared::Symbol;

use crate::def::DefId;

/// A handle to a single scope node in the [`ScopeTree`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(u32);

struct Scope {
    parent: Option<ScopeId>,
    bindings: HashMap<Symbol, DefId>,
}

/// A tree of lexical scopes.
///
/// Each scope stores a map from name to [`DefId`] and a parent link.
/// Lookup walks the chain from inner to outer until a match is found.
pub struct ScopeTree {
    scopes: Vec<Scope>,
}

impl ScopeTree {
    /// Creates an empty scope tree.
    #[must_use]
    pub const fn new() -> Self {
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

    /// Defines `name -> def_id` in `scope`.
    ///
    /// Returns the previous definition if the name was already bound in this
    /// exact scope (not a parent), so callers can diagnose shadowing.
    ///
    /// # Panics
    ///
    /// Panics if `scope` is not a valid handle returned by this `ScopeTree`.
    pub fn define(&mut self, scope: ScopeId, name: Symbol, def_id: DefId) -> Option<DefId> {
        let idx = usize::try_from(scope.0).expect("ScopeId in range");
        self.scopes[idx].bindings.insert(name, def_id)
    }

    /// Looks up `name` starting from `scope` and walking towards the root.
    ///
    /// Returns the first [`DefId`] found, or `None` if the name is unbound.
    ///
    /// # Panics
    ///
    /// Panics if `scope` is not a valid handle returned by this `ScopeTree`.
    #[must_use]
    pub fn lookup(&self, scope: ScopeId, name: Symbol) -> Option<DefId> {
        let mut cur = scope;
        loop {
            let idx = usize::try_from(cur.0).expect("ScopeId in range");
            let s = &self.scopes[idx];
            if let Some(&def) = s.bindings.get(&name) {
                return Some(def);
            }
            cur = s.parent?;
        }
    }

    /// Looks up `name` in `scope` only (not parents).
    ///
    /// # Panics
    ///
    /// Panics if `scope` is not a valid handle returned by this `ScopeTree`.
    #[must_use]
    pub fn lookup_local(&self, scope: ScopeId, name: Symbol) -> Option<DefId> {
        let idx = usize::try_from(scope.0).expect("ScopeId in range");
        self.scopes[idx].bindings.get(&name).copied()
    }

    /// Returns all names visible from `scope` (this scope and all ancestors).
    ///
    /// # Panics
    ///
    /// Panics if `scope` is not a valid handle returned by this `ScopeTree`.
    #[must_use]
    pub fn visible_names(&self, scope: ScopeId) -> Vec<Symbol> {
        let mut names = Vec::new();
        let mut cur = scope;
        loop {
            let idx = usize::try_from(cur.0).expect("ScopeId in range");
            let s = &self.scopes[idx];
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
