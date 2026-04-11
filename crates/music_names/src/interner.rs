use std::collections::HashMap;

use crate::Symbol;

#[derive(Debug, Default)]
pub struct Interner {
    map: HashMap<Box<str>, Symbol>,
    strings: Vec<Box<str>>,
}

impl Interner {
    #[must_use]
    pub fn new() -> Self {
        Self {
            map: HashMap::new(),
            strings: Vec::new(),
        }
    }

    /// Interns `s`, returning a stable [`Symbol`] for its spelling.
    ///
    /// If the string has already been interned, this returns the existing symbol.
    ///
    /// # Panics
    /// Panics if the interner exceeds `u32::MAX` unique strings.
    pub fn intern(&mut self, s: &str) -> Symbol {
        if let Some(sym) = self.map.get(s).copied() {
            return sym;
        }
        let raw = u32::try_from(self.strings.len()).expect("interner overflow: exceeded u32::MAX");
        let sym = Symbol::from_raw(raw);
        let boxed: Box<str> = s.into();
        self.strings.push(boxed.clone());
        let _prev = self.map.insert(boxed, sym);
        sym
    }

    #[must_use]
    pub fn try_resolve(&self, sym: Symbol) -> Option<&str> {
        let index = usize::try_from(sym.raw()).ok()?;
        self.strings.get(index).map(Box::as_ref)
    }

    /// Resolves `sym` back to its original spelling.
    ///
    /// # Panics
    /// Panics if `sym` was not allocated by this interner.
    #[must_use]
    pub fn resolve(&self, sym: Symbol) -> &str {
        self.try_resolve(sym)
            .expect("unknown symbol: not allocated by this interner")
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
