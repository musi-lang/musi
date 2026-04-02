use core::ops::{Index, IndexMut};
use core::slice;
use std::iter::{Enumerate, Map};

use crate::Idx;

const _: () = assert!(size_of::<usize>() >= size_of::<u32>());

#[allow(clippy::as_conversions)]
const fn widen(raw: u32) -> usize {
    raw as usize
}

#[derive(Debug)]
pub struct Arena<T> {
    data: Vec<T>,
}

pub type ArenaIter<'arena, T> =
    Map<Enumerate<slice::Iter<'arena, T>>, fn((usize, &'arena T)) -> (Idx<T>, &'arena T)>;

pub type ArenaIterMut<'arena, T> = Map<
    Enumerate<slice::IterMut<'arena, T>>,
    fn((usize, &'arena mut T)) -> (Idx<T>, &'arena mut T),
>;

impl<T> Arena<T> {
    #[must_use]
    pub const fn new() -> Self {
        Self { data: Vec::new() }
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// Allocates `value` into the arena and returns its stable index.
    ///
    /// # Panics
    /// Panics if the arena exceeds `u32::MAX` entries.
    pub fn alloc(&mut self, value: T) -> Idx<T> {
        let raw =
            u32::try_from(self.data.len()).expect("arena overflow: exceeded u32::MAX entries");
        self.data.push(value);
        Idx::new(raw)
    }

    /// Returns a shared reference to the value stored at `idx`.
    ///
    /// # Panics
    /// Panics if `idx` is out of bounds for this arena (for example, an index from a
    /// different arena type or instance).
    #[must_use]
    pub fn get(&self, idx: Idx<T>) -> &T {
        self.data
            .get(widen(idx.raw()))
            .expect("Idx out of bounds: used with wrong arena")
    }

    /// Returns a mutable reference to the value stored at `idx`.
    ///
    /// # Panics
    /// Panics if `idx` is out of bounds for this arena (for example, an index from a
    /// different arena type or instance).
    #[must_use]
    pub fn get_mut(&mut self, idx: Idx<T>) -> &mut T {
        self.data
            .get_mut(widen(idx.raw()))
            .expect("Idx out of bounds: used with wrong arena")
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.data.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.data.is_empty()
    }

    pub fn iter(&self) -> ArenaIter<'_, T> {
        #[allow(clippy::cast_possible_truncation, clippy::as_conversions)]
        const fn map_ref<T>((i, value): (usize, &T)) -> (Idx<T>, &T) {
            (Idx::from_raw(i as u32), value)
        }

        self.data.iter().enumerate().map(map_ref)
    }

    pub fn iter_mut(&mut self) -> ArenaIterMut<'_, T> {
        #[allow(clippy::cast_possible_truncation, clippy::as_conversions)]
        const fn map_ref<T>((i, value): (usize, &mut T)) -> (Idx<T>, &mut T) {
            (Idx::from_raw(i as u32), value)
        }

        self.data.iter_mut().enumerate().map(map_ref)
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<'arena, T> IntoIterator for &'arena Arena<T> {
    type Item = (Idx<T>, &'arena T);
    type IntoIter = ArenaIter<'arena, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter()
    }
}

impl<'arena, T> IntoIterator for &'arena mut Arena<T> {
    type Item = (Idx<T>, &'arena mut T);
    type IntoIter = ArenaIterMut<'arena, T>;

    fn into_iter(self) -> Self::IntoIter {
        self.iter_mut()
    }
}

impl<T> Index<Idx<T>> for Arena<T> {
    type Output = T;

    fn index(&self, index: Idx<T>) -> &Self::Output {
        self.get(index)
    }
}

impl<T> IndexMut<Idx<T>> for Arena<T> {
    fn index_mut(&mut self, index: Idx<T>) -> &mut Self::Output {
        self.get_mut(index)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
