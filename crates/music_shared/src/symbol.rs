use std::fmt;

/// Interned string identifier. 4 bytes, Copy.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Symbol(u32);

impl Symbol {
    /// Return the raw `u32` index.
    #[must_use]
    pub const fn raw(self) -> u32 {
        self.0
    }

    /// Create a synthetic symbol from a raw index.
    ///
    /// Use sentinel values (e.g. `u32::MAX - n`) to avoid collisions with
    /// symbols produced by the interner.
    #[must_use]
    pub const fn synthetic(id: u32) -> Self {
        Self(id)
    }
}

impl fmt::Display for Symbol {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "#{}", self.0)
    }
}

/// String interner. Maps strings to [`Symbol`] indices.
#[derive(Debug)]
pub struct Interner {
    table: intaglio::SymbolTable,
}

impl Interner {
    /// Create an empty interner.
    #[must_use]
    pub fn new() -> Self {
        Self {
            table: intaglio::SymbolTable::with_capacity(0),
        }
    }

    /// Intern a string, returning its symbol. Equal strings always yield the
    /// same symbol.
    ///
    /// # Panics
    ///
    /// Panics if the symbol table exceeds `u32::MAX` entries.
    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(sym) = self.table.check_interned(s) {
            return Symbol(sym.id());
        }
        let intaglio_sym = self
            .table
            .intern(String::from(s))
            .expect("symbol table overflow");
        Symbol(intaglio_sym.id())
    }

    /// Resolve a symbol back to its string.
    ///
    /// # Panics
    ///
    /// Panics if `sym` was not produced by this interner.
    #[must_use]
    pub fn resolve(&self, sym: Symbol) -> &str {
        self.table
            .get(intaglio::Symbol::new(sym.0))
            .expect("symbol not found in interner")
    }

    /// Number of interned strings.
    #[must_use]
    pub fn len(&self) -> usize {
        self.table.len()
    }

    /// Whether the interner contains no strings.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.table.is_empty()
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
