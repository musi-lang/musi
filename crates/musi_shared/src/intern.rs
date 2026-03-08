//! String interning via `intaglio`.

use intaglio::{Symbol as IntaglioSymbol, SymbolTable};

/// A compact, copyable handle to an interned string.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(pub u32);

/// Deduplicating string storage backed by [`intaglio::SymbolTable`].
#[derive(Debug, Default)]
pub struct Interner(SymbolTable);

impl Interner {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    #[must_use]
    pub fn with_capacity(sym_count: usize) -> Self {
        Self(SymbolTable::with_capacity(sym_count))
    }

    #[must_use]
    pub fn len(&self) -> usize {
        self.0.len()
    }

    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.0.is_empty()
    }

    /// Interns `s` and returns its [`Symbol`].
    ///
    /// # Panics
    ///
    /// Panics if the number of interned strings exceeds `u32::MAX`.
    #[must_use]
    pub fn intern(&mut self, s: &str) -> Symbol {
        let sym = self
            .0
            .intern(String::from(s))
            .expect("symbol table overflow");
        Symbol(sym.id())
    }

    /// Returns the [`Symbol`] for `s` if already interned, without mutating.
    #[must_use]
    pub fn get(&self, s: &str) -> Option<Symbol> {
        self.0.check_interned(s).map(|sym| Symbol(sym.id()))
    }

    /// Resolves a [`Symbol`] back to its string.
    ///
    /// # Panics
    ///
    /// Panics if `sym` did not originate from this interner.
    #[must_use]
    pub fn resolve(&self, sym: Symbol) -> &str {
        self.0
            .get(IntaglioSymbol::new(sym.0))
            .expect("symbol not found in this interner")
    }

    /// Attempts to resolve a [`Symbol`], returning `None` if invalid.
    #[must_use]
    pub fn try_resolve(&self, sym: Symbol) -> Option<&str> {
        self.0.get(IntaglioSymbol::new(sym.0))
    }
}

#[cfg(test)]
mod tests;
