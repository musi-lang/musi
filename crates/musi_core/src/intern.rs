use std::collections::HashMap;

#[derive(Debug, Default)]
/// String interner for deduplicating strings.
pub struct Interner {
    table: HashMap<String, u32>,
    strings: Vec<String>,
}

impl Interner {
    #[must_use]
    /// Creates new empty interner.
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
    pub fn lookup(&self, id: u32) -> Option<&str> {
        self.strings
            .get(usize::try_from(id).ok()?)
            .map(String::as_str)
    }

    #[must_use]
    /// Returns string slice or placeholder for given ID.
    pub fn resolve(&self, id: u32) -> &str {
        self.lookup(id).unwrap_or("<unknown>")
    }

    /// Clears all interned strings.
    pub fn clear(&mut self) {
        self.table.clear();
        self.strings.clear();
    }

    #[must_use]
    /// Returns number of interned strings.
    pub const fn len(&self) -> usize {
        self.strings.len()
    }

    #[must_use]
    /// Returns `true` if interner is empty.
    pub const fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }
}
