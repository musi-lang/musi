use std::iter::{Enumerate, Map};
use std::slice;

use crate::Idx;

/// Typed arena allocator. All nodes allocated here, referenced by `Idx<T>`.
///
/// Values are stored contiguously in a `Vec<T>` and addressed via lightweight
/// `Idx<T>` handles. The arena never frees individual entries; all memory is
/// released when the arena is dropped.
#[derive(Debug)]
pub struct Arena<T> {
    data: Vec<T>,
}

/// Iterator over `(Idx<T>, &T)` pairs in an arena.
pub type ArenaIter<'a, T> =
    Map<Enumerate<slice::Iter<'a, T>>, fn((usize, &'a T)) -> (Idx<T>, &'a T)>;

/// Iterator over `(Idx<T>, &mut T)` pairs in an arena.
pub type ArenaIterMut<'a, T> =
    Map<Enumerate<slice::IterMut<'a, T>>, fn((usize, &'a mut T)) -> (Idx<T>, &'a mut T)>;

impl<T> Arena<T> {
    /// Creates an empty arena.
    #[must_use]
    pub const fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Creates an empty arena with pre-allocated capacity for `capacity` items.
    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// Allocates `value` in the arena and returns its typed index.
    ///
    /// # Panics
    ///
    /// Panics if the arena already contains `u32::MAX` entries.
    pub fn alloc(&mut self, value: T) -> Idx<T> {
        let raw =
            u32::try_from(self.data.len()).expect("arena overflow: exceeded u32::MAX entries");
        self.data.push(value);
        Idx::new(raw)
    }

    /// Returns a shared reference to the value at `idx`.
    ///
    /// # Panics
    ///
    /// Panics if `idx` does not refer to a valid slot in this arena.
    #[must_use]
    pub fn get(&self, idx: Idx<T>) -> &T {
        self.data
            .get(usize::try_from(idx.raw()).expect("u32 always fits in usize"))
            .expect("Idx out of bounds: used with wrong arena")
    }

    /// Returns an exclusive reference to the value at `idx`.
    ///
    /// # Panics
    ///
    /// Panics if `idx` does not refer to a valid slot in this arena.
    #[must_use]
    pub fn get_mut(&mut self, idx: Idx<T>) -> &mut T {
        self.data
            .get_mut(usize::try_from(idx.raw()).expect("u32 always fits in usize"))
            .expect("Idx out of bounds: used with wrong arena")
    }

    /// Returns the number of values stored in the arena.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` if the arena contains no values.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    /// Iterates over all `(Idx<T>, &T)` pairs in insertion order.
    ///
    /// # Panics
    ///
    /// Panics if the arena contains more than `u32::MAX` entries (unreachable
    /// because `alloc` enforces this limit).
    pub fn iter(&self) -> ArenaIter<'_, T> {
        fn map_ref<T>((i, val): (usize, &T)) -> (Idx<T>, &T) {
            let raw = u32::try_from(i).expect("arena iteration: index exceeds u32::MAX");
            (Idx::new(raw), val)
        }
        self.data.iter().enumerate().map(map_ref)
    }

    /// Iterates over all `(Idx<T>, &mut T)` pairs in insertion order.
    ///
    /// # Panics
    ///
    /// Panics if the arena contains more than `u32::MAX` entries (unreachable
    /// because `alloc` enforces this limit).
    pub fn iter_mut(&mut self) -> ArenaIterMut<'_, T> {
        fn map_mut<T>((i, val): (usize, &mut T)) -> (Idx<T>, &mut T) {
            let raw = u32::try_from(i).expect("arena iteration: index exceeds u32::MAX");
            (Idx::new(raw), val)
        }
        self.data.iter_mut().enumerate().map(map_mut)
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'a, T> IntoIterator for &'a Arena<T> {
    type Item = (Idx<T>, &'a T);
    type IntoIter = ArenaIter<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'a, T> IntoIterator for &'a mut Arena<T> {
    type Item = (Idx<T>, &'a mut T);
    type IntoIter = ArenaIterMut<'a, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

#[cfg(test)]
mod tests;
