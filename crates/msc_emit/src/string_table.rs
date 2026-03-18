//! String table: interns all string data for the STRT section.

use std::collections::HashMap;

use msc_shared::{Interner, Symbol};

use crate::error::EmitError;

pub struct StringTable {
    entries: Vec<String>,
    index: HashMap<Symbol, u16>,
    str_index: HashMap<String, u16>,
}

impl StringTable {
    pub fn new() -> Self {
        Self {
            entries: vec![],
            index: HashMap::new(),
            str_index: HashMap::new(),
        }
    }

    pub fn intern(&mut self, sym: Symbol, interner: &Interner) -> Result<u16, EmitError> {
        if let Some(&idx) = self.index.get(&sym) {
            return Ok(idx);
        }
        let s = interner.resolve(sym).to_owned();
        if let Some(&idx) = self.str_index.get(&s) {
            let _ = self.index.insert(sym, idx);
            return Ok(idx);
        }
        let idx = u16::try_from(self.entries.len()).map_err(|_| EmitError::TooManyStrings)?;
        let _ = self.index.insert(sym, idx);
        let _ = self.str_index.insert(s.clone(), idx);
        self.entries.push(s);
        Ok(idx)
    }

    pub fn intern_str(&mut self, s: &str) -> Result<u16, EmitError> {
        if let Some(&idx) = self.str_index.get(s) {
            return Ok(idx);
        }
        let idx = u16::try_from(self.entries.len()).map_err(|_| EmitError::TooManyStrings)?;
        let _ = self.str_index.insert(s.to_owned(), idx);
        self.entries.push(s.to_owned());
        Ok(idx)
    }

    pub fn write_into(&self, buf: &mut Vec<u8>) {
        let count = self.entries.len() as u16;
        buf.extend_from_slice(&count.to_be_bytes());
        for entry in &self.entries {
            let bytes = entry.as_bytes();
            let len = bytes.len() as u16;
            buf.extend_from_slice(&len.to_be_bytes());
            buf.extend_from_slice(bytes);
        }
    }
}
