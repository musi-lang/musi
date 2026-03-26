use std::collections::HashMap;

use music_shared::Symbol;

use crate::def::DefId;

/// Index into the `ScopeArena`.
pub type ScopeId = usize;

/// A lexical scope containing name bindings and a link to its parent.
#[derive(Debug, Clone)]
pub struct Scope {
    pub kind: ScopeKind,
    pub bindings: HashMap<Symbol, DefId>,
    pub parent: Option<ScopeId>,
}

/// What syntactic construct introduced this scope.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ScopeKind {
    Module,
    Function,
    Lambda,
    CaseArm,
    Comprehension,
    Block,
    TypeParams,
}

impl Scope {
    #[must_use]
    pub fn new(kind: ScopeKind, parent: Option<ScopeId>) -> Self {
        Self {
            kind,
            bindings: HashMap::new(),
            parent,
        }
    }

    /// Insert a binding into this scope, returning the previous `DefId` if the
    /// name was already bound.
    pub fn bind(&mut self, name: Symbol, def: DefId) -> Option<DefId> {
        self.bindings.insert(name, def)
    }

    /// Look up a name in this scope only (not parents).
    #[must_use]
    pub fn lookup(&self, name: Symbol) -> Option<DefId> {
        self.bindings.get(&name).copied()
    }
}

/// Arena of scopes forming lexical scope chains.
pub struct ScopeArena {
    scopes: Vec<Scope>,
}

impl ScopeArena {
    #[must_use]
    pub const fn new() -> Self {
        Self { scopes: Vec::new() }
    }

    /// Push a new scope and return its id.
    pub fn push(&mut self, kind: ScopeKind, parent: Option<ScopeId>) -> ScopeId {
        let id = self.scopes.len();
        self.scopes.push(Scope::new(kind, parent));
        id
    }

    /// Get a shared reference to a scope.
    ///
    /// # Panics
    ///
    /// Panics if `id` is out of bounds.
    #[must_use]
    pub fn get(&self, id: ScopeId) -> &Scope {
        self.scopes.get(id).expect("invalid ScopeId")
    }

    /// Get an exclusive reference to a scope.
    ///
    /// # Panics
    ///
    /// Panics if `id` is out of bounds.
    #[must_use]
    pub fn get_mut(&mut self, id: ScopeId) -> &mut Scope {
        self.scopes.get_mut(id).expect("invalid ScopeId")
    }

    /// Returns `true` if walking from `use_scope` up to `def_scope` crosses
    /// a lambda or function boundary (meaning the variable is captured).
    #[must_use]
    pub fn crosses_lambda_boundary(&self, def_scope: ScopeId, use_scope: ScopeId) -> bool {
        let mut current = use_scope;
        loop {
            if current == def_scope {
                return false;
            }
            let scope = self.get(current);
            if matches!(scope.kind, ScopeKind::Lambda | ScopeKind::Function) {
                return true;
            }
            match scope.parent {
                Some(parent) => current = parent,
                None => return false,
            }
        }
    }

    /// Walk the scope chain from `scope_id` upward, returning the first
    /// `DefId` matching `name`, or `None` if not found.
    #[must_use]
    pub fn resolve(&self, scope_id: ScopeId, name: Symbol) -> Option<DefId> {
        let mut current = scope_id;
        loop {
            let scope = self.get(current);
            if let Some(def) = scope.lookup(name) {
                return Some(def);
            }
            current = scope.parent?;
        }
    }
}

impl Default for ScopeArena {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
