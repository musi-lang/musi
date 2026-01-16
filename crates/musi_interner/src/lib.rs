//! String interning for memory efficiency

#![warn(missing_docs)]

/// String interner for compact string storage
pub struct Interner {
    strings: Vec<String>,
}

/// Interned string identifier
#[derive(Copy, Clone, Debug, Eq, PartialEq, Hash)]
pub struct Interned(pub(crate) u32);

impl Interner {
    /// Create new empty interner
    #[must_use]
    pub const fn new() -> Self {
        Self { strings: vec![] }
    }

    /// Intern string and return compact identifier
    ///
    /// # Panics
    /// If `Interner` would exceed `u32::MAX` strings
    pub fn intern(&mut self, s: &str) -> Interned {
        let id = self
            .strings
            .iter()
            .position(|existing| existing == s)
            .unwrap_or_else(|| {
                self.strings.push(s.to_owned());
                self.strings.len() - 1
            });

        Interned(id.try_into().expect("interner overflow: too many strings"))
    }

    /// Get string slice from interned identifier
    ///
    /// # Panics
    /// If `Interned` ID exceeds `usize::MAX`
    #[must_use]
    pub fn resolve(&self, id: Interned) -> &str {
        self.strings
            .get(usize::try_from(id.0).expect("invalid interned ID"))
            .map_or("", String::as_str)
    }

    /// Check if interner contains no strings
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }

    /// Get number of interned strings
    #[must_use]
    pub const fn len(&self) -> usize {
        self.strings.len()
    }
}

impl Default for Interner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests;
