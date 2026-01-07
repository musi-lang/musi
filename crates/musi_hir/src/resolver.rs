use std::collections::HashMap;

use musi_core::{Interner, MusiResult, Symbol};

use crate::{DefId, DefKind, DefTable, errors};

#[derive(Debug)]
pub struct Scope {
    bindings: HashMap<u32, DefId>,
    parent: Option<usize>,
}

impl Scope {
    fn new(parent: Option<usize>) -> Self {
        Self {
            bindings: HashMap::new(),
            parent,
        }
    }
}

pub struct Resolver<'a> {
    interner: &'a Interner,
    scopes: Vec<Scope>,
    current: usize,
}

impl<'a> Resolver<'a> {
    #[must_use]
    pub fn new(interner: &'a Interner) -> Self {
        let mut resolver = Self {
            interner,
            scopes: vec![],
            current: 0,
        };
        resolver.scopes.push(Scope::new(None));
        resolver
    }

    pub fn push_scope(&mut self) {
        let new_scope = Scope::new(Some(self.current));
        self.current = self.scopes.len();
        self.scopes.push(new_scope);
    }

    pub fn pop_scope(&mut self) {
        if let Some(parent) = self.scopes[self.current].parent {
            self.current = parent;
        }
    }

    pub fn define(&mut self, defs: &mut DefTable, kind: DefKind, name: Symbol) -> DefId {
        let def_id = defs.insert(kind, name, name.span);
        let _ = self.scopes[self.current].bindings.insert(name.id, def_id);
        def_id
    }

    /// # Errors
    /// Returns error if the identifier is not found in any scope.
    pub fn resolve(&self, name: Symbol) -> MusiResult<DefId> {
        let mut scope_idx = self.current;
        loop {
            if let Some(&def_id) = self.scopes[scope_idx].bindings.get(&name.id) {
                return Ok(def_id);
            }
            match self.scopes[scope_idx].parent {
                Some(parent) => scope_idx = parent,
                None => return Err(errors::undefined_identifier(name, self.interner)),
            }
        }
    }
}
