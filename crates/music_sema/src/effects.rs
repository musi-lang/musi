use std::collections::BTreeSet;

use music_hir::HirTyId;

#[derive(Debug, Clone, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EffectKey {
    pub name: Box<str>,
    pub arg: Option<HirTyId>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EffectRow {
    pub items: BTreeSet<EffectKey>,
    pub open: Option<Box<str>>,
}

impl EffectRow {
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            items: BTreeSet::new(),
            open: None,
        }
    }

    #[must_use]
    pub fn is_pure(&self) -> bool {
        self.items.is_empty() && self.open.is_none()
    }

    pub fn add(&mut self, key: EffectKey) {
        let _did_insert = self.items.insert(key);
    }

    pub fn union_with(&mut self, other: &Self) {
        self.items.extend(other.items.iter().cloned());
        if self.open.is_none() {
            self.open.clone_from(&other.open);
        }
    }

    pub fn remove_by_name(&mut self, name: &str) {
        self.items.retain(|key| key.name.as_ref() != name);
    }
}
