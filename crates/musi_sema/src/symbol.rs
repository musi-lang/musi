use musi_ast::Ident;
use musi_basic::span::Span;
use std::collections::HashMap;

use crate::ty_repr::TyRepr;

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

#[derive(Debug)]
pub struct SymbolTable {
    symbols: Vec<Symbol>,
    scopes: Vec<Scope>,
    scope_id: ScopeId,
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
        }
    }

    #[must_use]
    pub const fn scope_id(&self) -> ScopeId {
        self.scope_id
    }

    #[must_use]
    pub fn scope_depth(&self) -> usize {
        let mut depth = 1;
        let mut curr = &self.scopes[self.scope_id.as_usize()];
        while let Some(parent) = curr.parent {
            depth += 1;
            curr = &self.scopes[parent.as_usize()];
        }
        depth
    }

    /// Pushes new scope.
    ///
    /// # Panics
    ///
    /// Panics if there are more than `u32::MAX` scopes.
    pub fn push_scope(&mut self) -> ScopeId {
        let id = ScopeId::new(u32::try_from(self.scopes.len()).expect("scope overflow"));
        let scope = Scope::new(Some(self.scope_id));
        self.scopes.push(scope);
        self.scope_id = id;
        id
    }

    /// Pops current scope.
    ///
    /// # Panics
    ///
    /// Panics if attempting to pop global scope.
    pub fn pop_scope(&mut self) {
        let scope = &self.scopes[self.scope_id.as_usize()];
        self.scope_id = scope.parent.expect("cannot pop global scope");
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
        let id = self.next_symbol_id();
        let symbol = Symbol::new(name, kind, ty, span, self.scope_id, mutable);
        self.symbols.push(symbol);

        let scope = &mut self.scopes[self.scope_id.as_usize()];
        scope.names.insert(name, id).map_or(Ok(id), Err)
    }

    #[must_use]
    pub fn lookup(&self, name: Ident) -> Option<SymbolId> {
        let mut scope_id = Some(self.scope_id);
        while let Some(sid) = scope_id {
            let scope = &self.scopes[sid.as_usize()];
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
        self.symbols.get(id.as_usize())
    }

    #[must_use]
    pub fn get_mut(&mut self, id: SymbolId) -> Option<&mut Symbol> {
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

    fn next_symbol_id(&self) -> SymbolId {
        let len = u32::try_from(self.symbols.len()).expect("symbol overflow");
        SymbolId::new(len)
    }
}

#[cfg(test)]
mod tests;
