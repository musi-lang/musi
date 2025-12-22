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

    pub fn intern(&mut self, s: &str) -> u32 {
        if let Some(&id) = self.table.get(s) {
            return id;
        }
        let id = self.strings.len() as u32;
        let s_owned = s.to_string();
        self.strings.push(s_owned.clone());
        let _ = self.table.insert(s_owned, id);
        id
    }

    pub fn lookup(&self, id: u32) -> Option<&str> {
        self.strings.get(id as usize).map(String::as_str)
    }

    pub fn clear(&mut self) {
        self.table.clear();
        self.strings.clear();
    }

    pub const fn size(&self) -> usize {
        self.strings.len()
    }
}
