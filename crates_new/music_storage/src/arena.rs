use std::iter::{Enumerate, Map};
use std::ops::{Index, IndexMut};
use std::slice;

use crate::Idx;

const _: () = assert!(size_of::<usize>() >= size_of::<u32>());

#[allow(clippy::as_conversions)]
const fn widen(v: u32) -> usize {
    v as usize
}

/// Typed arena allocator. All nodes allocated here, referenced by `Idx<T>`.
#[derive(Debug)]
pub struct Arena<T> {
    data: Vec<T>,
}

/// Iterator over `(Idx<T>, &T)` pairs in an arena.
pub type ArenaIter<'storage, T> =
    Map<Enumerate<slice::Iter<'storage, T>>, fn((usize, &'storage T)) -> (Idx<T>, &'storage T)>;

/// Iterator over `(Idx<T>, &mut T)` pairs in an arena.
pub type ArenaIterMut<'storage, T> = Map<
    Enumerate<slice::IterMut<'storage, T>>,
    fn((usize, &'storage mut T)) -> (Idx<T>, &'storage mut T),
>;

impl<T> Arena<T> {
    #[must_use]
    pub const fn new() -> Self {
        Self { data: vec![] }
    }

    #[must_use]
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            data: Vec::with_capacity(capacity),
        }
    }

    /// # Panics
    ///
    /// Panics if the arena already contains `u32::MAX` entries.
    pub fn alloc(&mut self, value: T) -> Idx<T> {
        let raw =
            u32::try_from(self.data.len()).expect("arena overflow: exceeded u32::MAX entries");
        self.data.push(value);
        Idx::new(raw)
    }

    /// # Panics
    ///
    /// Panics if `idx` does not refer to a valid slot in this arena.
    #[must_use]
    pub fn get(&self, idx: Idx<T>) -> &T {
        self.data
            .get(widen(idx.raw()))
            .expect("Idx out of bounds: used with wrong arena")
    }

    /// # Panics
    ///
    /// Panics if `idx` does not refer to a valid slot in this arena.
    #[must_use]
    pub fn get_mut(&mut self, idx: Idx<T>) -> &mut T {
        self.data
            .get_mut(widen(idx.raw()))
            .expect("Idx out of bounds: used with wrong arena")
    }

    /// # Safety
    ///
    /// `idx` must have been returned by a previous `alloc` call on this arena.
    #[inline]
    #[must_use]
    #[allow(unsafe_code)]
    pub unsafe fn get_unchecked(&self, idx: Idx<T>) -> &T {
        unsafe { self.data.get_unchecked(widen(idx.raw())) }
    }

    /// # Safety
    ///
    /// `idx` must have been returned by a previous `alloc` call on this arena.
    #[inline]
    #[must_use]
    #[allow(unsafe_code)]
    pub unsafe fn get_mut_unchecked(&mut self, idx: Idx<T>) -> &mut T {
        unsafe { self.data.get_unchecked_mut(widen(idx.raw())) }
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
        const fn map_ref<T>((i, val): (usize, &T)) -> (Idx<T>, &T) {
            (Idx::new(i as u32), val)
        }
        self.data.iter().enumerate().map(map_ref)
    }

    pub fn iter_mut(&mut self) -> ArenaIterMut<'_, T> {
        #[allow(clippy::cast_possible_truncation, clippy::as_conversions)]
        const fn map_mut<T>((i, val): (usize, &mut T)) -> (Idx<T>, &mut T) {
            (Idx::new(i as u32), val)
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

impl<T> Index<Idx<T>> for Arena<T> {
    type Output = T;

    fn index(&self, idx: Idx<T>) -> &T {
        self.get(idx)
    }
}

impl<T> IndexMut<Idx<T>> for Arena<T> {
    fn index_mut(&mut self, idx: Idx<T>) -> &mut T {
        self.get_mut(idx)
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
