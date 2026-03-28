use std::collections::HashMap;

/// A constant value stored in the module's constant pool.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum ConstantEntry {
    Int(i64),
    Float(u64),
    Str(String),
    Tag(u16),
}

/// De-duplicating constant pool builder.
///
/// Each unique `ConstantEntry` is stored once, and repeated additions
/// return the same `u16` index.
pub struct ConstantPool {
    entries: Vec<ConstantEntry>,
    dedup: HashMap<ConstantEntry, u16>,
}

impl ConstantPool {
    #[must_use]
    pub fn new() -> Self {
        Self {
            entries: Vec::new(),
            dedup: HashMap::new(),
        }
    }

    /// Insert a constant and return its pool index. Returns the existing
    /// index if the entry was already present.
    ///
    /// # Panics
    ///
    /// Panics if the pool exceeds `u16::MAX` entries.
    pub fn add(&mut self, entry: ConstantEntry) -> u16 {
        if let Some(&idx) = self.dedup.get(&entry) {
            return idx;
        }
        let idx = u16::try_from(self.entries.len()).expect("constant pool overflow (>65535)");
        let _prev = self.dedup.insert(entry.clone(), idx);
        self.entries.push(entry);
        idx
    }

    #[must_use]
    pub fn entries(&self) -> &[ConstantEntry] {
        &self.entries
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.entries.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.entries.is_empty()
    }
}

impl Default for ConstantPool {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
