use std::collections::HashMap;

/// String interner for deduplicating strings.
#[derive(Debug, Default)]
pub struct Interner {
    table: HashMap<String, u32>,
    strings: Vec<String>,
}

impl Interner {
    /// Creates new empty interner.
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Interns string and returns unique ID.
    ///
    /// # Panics
    ///
    /// Panics if number of strings exceeds `u32::MAX`.
    pub fn intern(&mut self, text: &str) -> u32 {
        if let Some(&id) = self.table.get(text) {
            return id;
        }
        let id = u32::try_from(self.strings.len()).expect("interner overflow");
        let owned = text.to_owned();
        self.strings.push(owned.clone());
        let _ = self.table.insert(owned, id);
        id
    }

    /// Returns string slice for given ID.
    #[must_use]
    pub fn lookup(&self, id: u32) -> Option<&str> {
        self.strings
            .get(usize::try_from(id).ok()?)
            .map(String::as_str)
    }

    /// Returns string slice or placeholder for given ID.
    #[must_use]
    pub fn resolve(&self, id: u32) -> &str {
        self.lookup(id).unwrap_or("<unknown>")
    }

    /// Clears all interned strings.
    pub fn clear(&mut self) {
        self.table.clear();
        self.strings.clear();
    }

    /// Returns number of interned strings.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.strings.len()
    }

    /// Returns `true`` if interner is empty.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }
}
