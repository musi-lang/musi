use std::collections::HashMap;

use musi_core::{MusiError, MusiResult, Span, Symbol};

use crate::ty::TyId;

#[derive(Debug, Clone)]
pub struct ValueEntry {
    pub ty: TyId,
    pub mutable: bool,
    pub span: Span,
}

#[derive(Debug, Default)]
struct Scope {
    values: HashMap<Symbol, ValueEntry>,
    tys: HashMap<Symbol, TyId>,
}

#[derive(Debug, Default)]
pub struct TyEnv {
    scopes: Vec<Scope>,
}

impl TyEnv {
    #[must_use]
    pub fn new() -> Self {
        Self {
            scopes: vec![Scope::default()],
        }
    }

    pub fn enter_scope(&mut self) {
        self.scopes.push(Scope::default());
    }

    /// # Errors
    /// Returns error if attempting to exit global scope.
    pub fn exit_scope(&mut self, span: Span) -> MusiResult<()> {
        if self.scopes.len() > 1 {
            let _ = self.scopes.pop();
            Ok(())
        } else {
            Err(MusiError::new("cannot exit global scope", span))
        }
    }

    /// # Panics
    /// Panics if environment has no scopes (invariant violation).
    pub fn bind_value(&mut self, name: Symbol, ty: TyId, mutable: bool, span: Span) {
        let scope = self.current_scope_mut();
        let _ = scope.values.insert(name, ValueEntry { ty, mutable, span });
    }

    /// # Panics
    /// Panics if environment has no scopes (invariant violation).
    pub fn bind_ty(&mut self, name: Symbol, ty: TyId) {
        let scope = self.current_scope_mut();
        let _ = scope.tys.insert(name, ty);
    }

    fn current_scope_mut(&mut self) -> &mut Scope {
        self.scopes
            .last_mut()
            .expect("TyEnv must always have at least one scope")
    }

    #[must_use]
    pub fn lookup_value(&self, name: Symbol) -> Option<&ValueEntry> {
        for scope in self.scopes.iter().rev() {
            if let Some(entry) = scope.values.get(&name) {
                return Some(entry);
            }
        }
        None
    }

    #[must_use]
    pub fn lookup_ty(&self, name: Symbol) -> Option<TyId> {
        for scope in self.scopes.iter().rev() {
            if let Some(&ty) = scope.tys.get(&name) {
                return Some(ty);
            }
        }
        None
    }
}

#[cfg(test)]
mod tests;
