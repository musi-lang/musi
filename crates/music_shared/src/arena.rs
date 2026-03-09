//! Typed arena with index-based access.
//!
//! [`Arena<T>`] is a bump-allocator backed by a `Vec<T>`.  Items are addressed
//! by [`Idx<T>`], a typed `u32` wrapper that is `Copy` and carries the element
//! type as a phantom parameter so indices into different arenas cannot be mixed.
//!
//! [`IdxRange<T>`] represents a contiguous half-open range `[start, end)` of
//! consecutively allocated items.

#[cfg(test)]
mod tests;

use std::{
    fmt,
    hash::{Hash, Hasher},
    marker::PhantomData,
    ops::{Index, IndexMut, Range},
};

/// Vec-backed bump allocator. Only grows; individual items are never freed.
pub struct Arena<T> {
    data: Vec<T>,
}

impl<T> Arena<T> {
    /// Creates an empty arena.
    #[must_use]
    pub const fn new() -> Self {
        Self { data: Vec::new() }
    }

    /// Creates an empty arena with at least `cap` capacity pre-allocated.
    #[must_use]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            data: Vec::with_capacity(cap),
        }
    }

    /// Stores `value` in the arena and returns its index.
    ///
    /// # Panics
    ///
    /// Panics if the arena contains `u32::MAX` or more items.
    #[must_use]
    pub fn alloc(&mut self, value: T) -> Idx<T> {
        let raw = u32::try_from(self.data.len()).expect("arena length must fit in u32");
        self.data.push(value);
        Idx::from_raw(raw)
    }

    /// Stores all items yielded by `iter` consecutively and returns the
    /// half-open [`IdxRange<T>`] that covers them.
    ///
    /// # Panics
    ///
    /// Panics if the arena grows beyond `u32::MAX` items.
    #[must_use]
    pub fn alloc_iter(&mut self, iter: impl IntoIterator<Item = T>) -> IdxRange<T> {
        let start = u32::try_from(self.data.len()).expect("arena length must fit in u32");
        self.data.extend(iter);
        let end = u32::try_from(self.data.len()).expect("arena length must fit in u32");
        IdxRange::new(start..end)
    }

    /// Returns the number of items stored in the arena.
    #[must_use]
    pub const fn len(&self) -> usize {
        self.data.len()
    }

    /// Returns `true` when no items have been allocated yet.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

impl<T> Index<Idx<T>> for Arena<T> {
    type Output = T;

    fn index(&self, idx: Idx<T>) -> &T {
        let i = usize::try_from(idx.raw).expect("platform must be >= 32-bit");
        &self.data[i]
    }
}

impl<T> IndexMut<Idx<T>> for Arena<T> {
    fn index_mut(&mut self, idx: Idx<T>) -> &mut T {
        let i = usize::try_from(idx.raw).expect("platform must be >= 32-bit");
        &mut self.data[i]
    }
}

impl<T> Index<IdxRange<T>> for Arena<T> {
    type Output = [T];

    fn index(&self, range: IdxRange<T>) -> &[T] {
        let start = usize::try_from(range.start).expect("platform must be >= 32-bit");
        let end = usize::try_from(range.end).expect("platform must be >= 32-bit");
        &self.data[start..end]
    }
}

/// Typed index into an [`Arena<T>`].
///
/// Invariant in `T` via `PhantomData<fn() -> T>` so that indices for different
/// arenas cannot be accidentally interchanged.
pub struct Idx<T> {
    raw: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Idx<T> {
    /// Constructs an `Idx` from a raw `u32`.  Only `Arena` should call this.
    #[must_use]
    pub const fn from_raw(raw: u32) -> Self {
        Self {
            raw,
            _marker: PhantomData,
        }
    }

    /// Returns the underlying `u32`, useful for serialisation.
    #[must_use]
    pub const fn raw(self) -> u32 {
        self.raw
    }
}

impl<T> Copy for Idx<T> {}
impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}
impl<T> Eq for Idx<T> {}
impl<T> Hash for Idx<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}
impl<T> fmt::Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "Idx({})", self.raw)
    }
}

/// Contiguous half-open range `[start, end)` of items in an [`Arena<T>`].
///
/// Produced by [`Arena::alloc_iter`].  `Copy` and iterable as `Idx<T>`.
pub struct IdxRange<T> {
    start: u32,
    end: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> IdxRange<T> {
    /// Constructs an `IdxRange` from a half-open `u32` range.
    #[must_use]
    pub const fn new(range: Range<u32>) -> Self {
        Self {
            start: range.start,
            end: range.end,
            _marker: PhantomData,
        }
    }

    /// Returns the number of items in the range.
    ///
    /// # Panics
    ///
    /// Panics on platforms where `usize` is narrower than 32 bits.
    #[must_use]
    pub fn len(&self) -> usize {
        usize::try_from(self.end - self.start).expect("platform must be >= 32-bit")
    }

    /// Returns `true` when the range covers no items.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }
}

impl<T> Copy for IdxRange<T> {}
impl<T> Clone for IdxRange<T> {
    fn clone(&self) -> Self {
        *self
    }
}
impl<T> PartialEq for IdxRange<T> {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.end == other.end
    }
}
impl<T> Eq for IdxRange<T> {}
impl<T> fmt::Debug for IdxRange<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "IdxRange({}..{})", self.start, self.end)
    }
}

/// Iterator over the indices in an [`IdxRange<T>`].
pub struct IdxRangeIter<T> {
    current: u32,
    end: u32,
    _marker: PhantomData<fn() -> T>,
}

impl<T> Iterator for IdxRangeIter<T> {
    type Item = Idx<T>;

    fn next(&mut self) -> Option<Idx<T>> {
        if self.current < self.end {
            let idx = Idx::from_raw(self.current);
            self.current += 1;
            Some(idx)
        } else {
            None
        }
    }

    fn size_hint(&self) -> (usize, Option<usize>) {
        let remaining =
            usize::try_from(self.end - self.current).expect("platform must be >= 32-bit");
        (remaining, Some(remaining))
    }
}

impl<T> ExactSizeIterator for IdxRangeIter<T> {}

impl<T> IntoIterator for IdxRange<T> {
    type Item = Idx<T>;
    type IntoIter = IdxRangeIter<T>;

    fn into_iter(self) -> IdxRangeIter<T> {
        IdxRangeIter {
            current: self.start,
            end: self.end,
            _marker: PhantomData,
        }
    }
}
