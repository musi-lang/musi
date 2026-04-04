use std::collections::BTreeSet;

use music_hir::HirTyId;
use music_names::Symbol;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct EffectKey {
    pub name: Symbol,
    pub arg: Option<HirTyId>,
}

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct EffectRow {
    pub items: BTreeSet<EffectKey>,
    pub open: Option<Symbol>,
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
        self.items.extend(other.items.iter().copied());
        if self.open.is_none() {
            self.open = other.open;
        }
    }

    pub fn remove_by_name(&mut self, name: Symbol) {
        self.items.retain(|key| key.name != name);
    }
}
