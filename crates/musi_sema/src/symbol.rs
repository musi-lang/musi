use musi_ast::Ident;
use musi_basic::span::Span;

use crate::ty_repr::TyRepr;
use std::collections::HashMap;
use std::sync::Arc;
use std::sync::atomic::{AtomicU32, Ordering};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolId(pub u32);

impl SymbolId {
    #[must_use]
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    #[must_use]
    pub const fn as_u32(self) -> u32 {
        self.0
    }

    /// Returns symbol ID as `usize`.
    ///
    /// # Panics
    ///
    /// Panics if symbol ID cannot fit into `usize`.
    #[must_use]
    pub fn as_usize(self) -> usize {
        usize::try_from(self.0).expect("symbol ID overflow")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct ScopeId(pub u32);

impl ScopeId {
    #[must_use]
    pub const fn new(id: u32) -> Self {
        Self(id)
    }

    /// Returns scope ID as `usize`.
    ///
    /// # Panics
    ///
    /// Panics if the scope ID cannot fit into `usize`.
    #[must_use]
    pub fn as_usize(self) -> usize {
        usize::try_from(self.0).expect("scope ID overflow")
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum SymbolKind {
    Local,
    Param,
    Fn,
    Type,
    Field,
    Variant,
    Builtin,
}

#[derive(Debug, Clone)]
pub struct Symbol {
    pub name: Ident,
    pub kind: SymbolKind,
    pub ty: TyRepr,
    pub def_span: Span,
    pub scope_id: ScopeId,
    pub mutable: bool,
}

impl Symbol {
    #[must_use]
    pub const fn new(
        name: Ident,
        kind: SymbolKind,
        ty: TyRepr,
        def_span: Span,
        scope_id: ScopeId,
        mutable: bool,
    ) -> Self {
        Self {
            name,
            kind,
            ty,
            def_span,
            scope_id,
            mutable,
        }
    }
}

#[derive(Debug, Clone)]
pub struct Scope {
    pub parent: Option<ScopeId>,
    names: HashMap<Ident, SymbolId>,
}

impl Scope {
    #[must_use]
    pub fn new(parent: Option<ScopeId>) -> Self {
        Self {
            parent,
            names: HashMap::new(),
        }
    }

    pub fn insert(&mut self, name: Ident, symbol: SymbolId) -> Option<SymbolId> {
        self.names.insert(name, symbol)
    }

    #[must_use]
    pub fn get(&self, name: Ident) -> Option<SymbolId> {
        self.names.get(&name).copied()
    }
}

#[derive(Debug, Clone)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    scopes: Vec<Scope>,
    scope_id: ScopeId,
    next_symbol: Arc<AtomicU32>,
    next_scope: Arc<AtomicU32>,
    local_symbols: HashMap<SymbolId, Symbol>,
    local_scopes: HashMap<ScopeId, Scope>,
    modified_scopes: HashMap<ScopeId, Scope>,
}

impl Default for SymbolTable {
    fn default() -> Self {
        Self::new()
    }
}

impl SymbolTable {
    #[must_use]
    pub fn new() -> Self {
        let global = Scope::new(None);
        Self {
            symbols: vec![],
            scopes: vec![global],
            scope_id: ScopeId::new(0),
            next_symbol: Arc::new(AtomicU32::new(0)),
            next_scope: Arc::new(AtomicU32::new(1)),
            local_symbols: HashMap::new(),
            local_scopes: HashMap::new(),
            modified_scopes: HashMap::new(),
        }
    }

    #[must_use]
    pub fn fork(&self) -> Self {
        Self {
            symbols: self.all_symbols(),
            scopes: self.all_scopes(),
            scope_id: self.scope_id,
            next_symbol: Arc::clone(&self.next_symbol),
            next_scope: Arc::clone(&self.next_scope),
            local_symbols: HashMap::new(),
            local_scopes: HashMap::new(),
            modified_scopes: HashMap::new(),
        }
    }

    fn all_symbols(&self) -> Vec<Symbol> {
        let mut all = self.symbols.clone();
        for (id, sym) in &self.local_symbols {
            let idx = id.as_usize();
            if idx >= all.len() {
                all.resize(idx + 1, sym.clone());
            }
            all[idx] = sym.clone();
        }
        all
    }

    fn all_scopes(&self) -> Vec<Scope> {
        let mut all = self.scopes.clone();
        for (id, scope) in &self.local_scopes {
            let idx = id.as_usize();
            if idx >= all.len() {
                all.resize(idx + 1, scope.clone());
            }
            all[idx] = scope.clone();
        }
        for (id, scope) in &self.modified_scopes {
            let idx = id.as_usize();
            if idx >= all.len() {
                all.resize(idx + 1, scope.clone());
            }
            all[idx] = scope.clone();
        }
        all
    }

    #[must_use]
    pub const fn scope_id(&self) -> ScopeId {
        self.scope_id
    }

    #[must_use]
    /// Returns depth of current scope.
    ///
    /// # Panics
    ///
    /// Panics ifcurrent scope is global scope.
    pub fn scope_depth(&self) -> usize {
        let mut depth = 1;
        let mut curr = self.get_scope(self.scope_id).expect("missing scope");
        while let Some(parent) = curr.parent {
            depth += 1;
            curr = self.get_scope(parent).expect("missing scope");
        }
        depth
    }

    /// Pushes new scope.
    ///
    /// # Panics
    ///
    /// Panics if there are more than `u32::MAX` scopes.
    pub fn push_scope(&mut self) -> ScopeId {
        let id = ScopeId::new(self.next_scope.fetch_add(1, Ordering::Relaxed));
        let scope = Scope::new(Some(self.scope_id));
        let _ = self.local_scopes.insert(id, scope);
        self.scope_id = id;
        id
    }

    pub const fn reenter(&mut self, id: ScopeId) {
        self.scope_id = id;
    }

    /// Pops current scope.
    ///
    /// # Panics
    ///
    /// Panics if attempting to pop global scope.
    pub fn pop_scope(&mut self) {
        let scope = self.get_scope(self.scope_id).expect("missing scope");
        self.scope_id = scope.parent.expect("cannot pop global scope");
    }

    fn get_scope(&self, id: ScopeId) -> Option<&Scope> {
        self.local_scopes
            .get(&id)
            .or_else(|| self.modified_scopes.get(&id))
            .or_else(|| self.scopes.get(id.as_usize()))
    }

    fn get_scope_mut(&mut self, id: ScopeId) -> Option<&mut Scope> {
        if self.local_scopes.contains_key(&id) {
            return self.local_scopes.get_mut(&id);
        }
        if self.modified_scopes.contains_key(&id) {
            return self.modified_scopes.get_mut(&id);
        }
        if let Some(scope) = self.scopes.get(id.as_usize()) {
            let cloned = scope.clone();
            return Some(self.modified_scopes.entry(id).or_insert(cloned));
        }
        None
    }

    /// Defines new symbol in current scope.
    ///
    /// # Errors
    ///
    /// Returns previous `SymbolId` if symbol with same name already exists.
    ///
    /// # Panics
    ///
    /// Panics if there are more than `u32::MAX` symbols.
    pub fn define(
        &mut self,
        name: Ident,
        kind: SymbolKind,
        ty: TyRepr,
        span: Span,
        mutable: bool,
    ) -> Result<SymbolId, SymbolId> {
        let id = SymbolId::new(self.next_symbol.fetch_add(1, Ordering::Relaxed));
        let symbol = Symbol::new(name, kind, ty, span, self.scope_id, mutable);
        let _ = self.local_symbols.insert(id, symbol);

        let scope = self.get_scope_mut(self.scope_id).expect("missing scope");
        scope.names.insert(name, id).map_or(Ok(id), Err)
    }

    pub fn merge(&mut self, other: Self) {
        for (id, sym) in other.local_symbols {
            let idx = id.as_usize();
            if idx >= self.symbols.len() {
                self.symbols.resize(idx + 1, sym.clone());
            }
            self.symbols[idx] = sym;
        }
        for (id, scope) in other.local_scopes {
            let idx = id.as_usize();
            if idx >= self.scopes.len() {
                self.scopes.resize(idx + 1, scope.clone());
            }
            self.scopes[idx] = scope;
        }
        for (id, scope) in other.modified_scopes {
            self.scopes[id.as_usize()] = scope;
        }
    }

    #[must_use]
    pub fn lookup(&self, name: Ident) -> Option<SymbolId> {
        let mut scope_id = Some(self.scope_id);
        while let Some(sid) = scope_id {
            let scope = self.get_scope(sid)?;
            if let Some(symbol) = scope.get(name) {
                return Some(symbol);
            }
            scope_id = scope.parent;
        }
        None
    }

    #[must_use]
    pub fn lookup_local(&self, name: Ident) -> Option<SymbolId> {
        self.scopes[self.scope_id.as_usize()].get(name)
    }

    #[must_use]
    pub fn get(&self, id: SymbolId) -> Option<&Symbol> {
        self.local_symbols
            .get(&id)
            .or_else(|| self.symbols.get(id.as_usize()))
    }

    #[must_use]
    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut Symbol> {
        if let Some(symbol) = self.local_symbols.get_mut(&id) {
            return Some(symbol);
        }
        self.symbols.get_mut(id.as_usize())
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.symbols.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.symbols.is_empty()
    }
}

#[cfg(test)]
mod tests;
