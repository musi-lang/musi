//! Arena-backed string interning.

use std::collections::HashMap;
use std::sync::Arc;

/// A compact, copyable handle to an interned string.
///
/// Two symbols are equal if and only if their source strings are equal.
/// Ordering reflects insertion order into the [`Interner`].
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(pub u32);

/// Deduplicating string storage.
///
/// Each unique string is stored exactly once and assigned a [`Symbol`] handle.
/// Resolving a symbol back to its string is an O(1) index lookup.
///
/// String data is heap-allocated once per unique string; both the lookup map
/// and the index vec share the same [`Arc<str>`] allocation.
#[derive(Debug, Default)]
pub struct Interner {
    map: HashMap<Arc<str>, Symbol>,
    strings: Vec<Arc<str>>,
}

impl Interner {
    /// Creates an empty interner.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Creates an interner pre-allocated for at least `sym_count` unique strings.
    #[must_use]
    pub fn with_capacity(sym_count: usize) -> Self {
        Self {
            map: HashMap::with_capacity(sym_count),
            strings: Vec::with_capacity(sym_count),
        }
    }

    /// Returns the number of unique strings interned so far.
    #[must_use]
    pub fn len(&self) -> usize {
        self.strings.len()
    }

    /// Returns `true` if no strings have been interned.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Interns a string, returning its [`Symbol`].
    ///
    /// If the string has already been interned, the existing symbol is returned.
    /// Otherwise a new symbol is allocated and the string is stored.
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

    /// Resolves a [`Symbol`] back to its interned string.
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
