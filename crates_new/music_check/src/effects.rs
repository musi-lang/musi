use std::collections::BTreeSet;

use music_names::Symbol;

use super::SemTyId;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EffectKey {
    pub name: Symbol,
    pub arg: Option<SemTyId>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EffectRow {
    pub items: BTreeSet<EffectKey>,
    pub is_open: bool,
}

impl EffectRow {
    #[must_use]
    pub fn empty() -> Self {
        Self {
            items: BTreeSet::new(),
            is_open: false,
        }
    }

    #[must_use]
    pub fn is_pure(&self) -> bool {
        self.items.is_empty() && !self.is_open
    }

    pub fn add(&mut self, key: EffectKey) {
        let _ = self.items.insert(key);
    }

    pub fn remove_by_name(&mut self, name: Symbol) {
        self.items.retain(|k| k.name != name);
    }

    pub fn union_with(&mut self, other: &EffectRow) {
        self.items.extend(other.items.iter().cloned());
        self.is_open |= other.is_open;
    }
}
