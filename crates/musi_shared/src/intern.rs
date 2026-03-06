//! Arena-backed string interning.

use std::collections::HashMap;
use std::sync::Arc;

/// A compact, copyable handle to an interned string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(pub u32);

/// Deduplicating string storage.
///
/// Each unique string is stored exactly once and assigned a [`Symbol`] handle.
/// Both the lookup map and the index vec share the same [`Arc<str>`] allocation.
#[derive(Debug, Default)]
pub struct Interner {
    map: HashMap<Arc<str>, Symbol>,
    strings: Vec<Arc<str>>,
}

impl Interner {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(sym_count: usize) -> Self {
        Self {
            map: HashMap::with_capacity(sym_count),
            strings: Vec::with_capacity(sym_count),
        }
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Interns `s` and returns its [`Symbol`].
    ///
    /// # Panics
    ///
    /// Panics if the number of interned strings exceeds `u32::MAX`.
    #[must_use]
    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(&sym) = self.map.get(s) {
            return sym;
        }
        let sym = Symbol(u32::try_from(self.strings.len()).expect("symbol table overflow"));
        let arc: Arc<str> = s.into();
        self.strings.push(Arc::clone(&arc));
        let _prev = self.map.insert(arc, sym);
        sym
    }

    /// Resolves a [`Symbol`] back to its string.
    ///
    /// # Panics
    ///
    /// Panics if `sym` did not originate from this interner.
    #[must_use]
    pub fn resolve(&self, sym: Symbol) -> &str {
        let idx = usize::try_from(sym.0).expect("symbol index out of range");
        self.strings
            .get(idx)
            .expect("symbol not found in this interner")
    }
}

#[cfg(test)]
mod tests;
