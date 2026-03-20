use std::collections::HashMap;

use crate::def::DefId;

pub struct LangItemRegistry {
    items: HashMap<Box<str>, DefId>,
}

impl LangItemRegistry {
    #[must_use]
    pub fn new() -> Self {
        Self {
            items: HashMap::new(),
        }
    }

    pub fn register(&mut self, name: &str, def: DefId) -> Option<DefId> {
        self.items.insert(Box::from(name), def)
    }

    #[must_use]
    pub fn get(&self, name: &str) -> Option<DefId> {
        self.items.get(name).copied()
    }
}

impl Default for LangItemRegistry {
    fn default() -> Self {
        Self::new()
    }
}
