use std::collections::HashMap;

#[derive(Debug, Default)]
pub struct Interner {
    table: HashMap<String, u32>,
    strings: Vec<String>,
}

impl Interner {
    pub fn new() -> Self {
        Self::default()
    }

    pub fn intern(&mut self, text: &str) -> u32 {
        if let Some(&id) = self.table.get(text) {
            return id;
        }
        let id = self.strings.len() as u32;
        let owned = text.to_string();
        self.strings.push(owned.clone());
        let _ = self.table.insert(owned, id);
        id
    }

    pub fn lookup(&self, id: u32) -> Option<&str> {
        self.strings.get(id as usize).map(String::as_str)
    }

    pub fn clear(&mut self) {
        self.table.clear();
        self.strings.clear();
    }

    pub const fn len(&self) -> usize {
        self.strings.len()
    }
}
