use std::collections::HashMap;

use crate::types::Ident;

#[derive(Debug, Default)]
pub struct Interner {
    table: HashMap<String, u32>,
    strings: Vec<String>,
}

impl Interner {
    #[must_use]
    pub fn new() -> Self {
        Self::default()
    }

    /// Interns string and returns its unique ID.
    ///
    /// # Panics
    ///
    /// Panics if interner contains more than `u32::MAX` strings.
    pub fn intern(&mut self, text: &str) -> Ident {
        if let Some(&id) = self.table.get(text) {
            return id;
        }
        let id = u32::try_from(self.strings.len()).expect("interner overflow");
        let owned = text.to_owned();
        self.strings.push(owned.clone());
        let _: Option<Ident> = self.table.insert(owned, id);
        id
    }

    #[must_use]
    pub fn lookup(&self, id: Ident) -> Option<&str> {
        self.strings
            .get(usize::try_from(id).ok()?)
            .map(String::as_str)
    }

    #[must_use]
    pub fn resolve(&self, id: Ident) -> &str {
        self.lookup(id).unwrap_or("<unknown>")
    }

    pub fn clear(&mut self) {
        self.table.clear();
        self.strings.clear();
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.strings.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.strings.is_empty()
    }
}
