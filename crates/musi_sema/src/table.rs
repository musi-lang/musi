use crate::ty::TyId;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum VarValue {
    Unbound,
    Link(u32),
    Bound(TyId),
}

#[derive(Debug, Default)]
pub struct UnificationTable {
    values: Vec<VarValue>,
}

impl UnificationTable {
    #[must_use]
    pub const fn new() -> Self {
        Self { values: Vec::new() }
    }

    /// # Panics
    /// Panics if table contains more than `u32::MAX` keys.
    pub fn new_key(&mut self) -> u32 {
        let key = u32::try_from(self.values.len()).expect("table overflow");
        self.values.push(VarValue::Unbound);
        key
    }

    fn get_root(&mut self, key: u32) -> u32 {
        let idx = Self::idx(key);
        match self.values[idx] {
            VarValue::Link(parent) => {
                let root = self.get_root(parent);
                self.values[idx] = VarValue::Link(root);
                root
            }
            _ => key,
        }
    }

    /// # Panics
    /// Panics if key is out of bounds.
    pub fn probe(&mut self, key: u32) -> Option<TyId> {
        let root = self.get_root(key);
        match self.values[Self::idx(root)] {
            VarValue::Bound(ty) => Some(ty),
            _ => None,
        }
    }

    /// # Panics
    /// Panics if either key is out of bounds.
    pub fn unify_var_var(&mut self, key1: u32, key2: u32) {
        let root1 = self.get_root(key1);
        let root2 = self.get_root(key2);

        if root1 != root2 {
            self.values[Self::idx(root1)] = VarValue::Link(root2);
        }
    }

    /// # Panics
    /// Panics if key is out of bounds.
    pub fn unify_var_ty(&mut self, key: u32, ty: TyId) {
        let root = self.get_root(key);
        self.values[Self::idx(root)] = VarValue::Bound(ty);
    }

    fn idx(key: u32) -> usize {
        usize::try_from(key).expect("invalid key index")
    }
}
