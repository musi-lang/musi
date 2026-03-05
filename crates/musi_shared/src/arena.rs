//! A typed arena with strongly-typed indices, plus a slice arena for batch allocation.

use core::cmp::Ordering;
use core::fmt;
use core::hash::{Hash, Hasher};
use core::marker::PhantomData;

// ── Idx<T> ──────────────────────────────────────────────────────────────────

/// A strongly-typed index into an [`Arena<T>`].
///
/// The phantom type parameter prevents mixing indices from different arenas.
/// Uses `PhantomData<fn() -> T>` so that `Idx<T>` is `Send + Sync` unconditionally
/// (an index is just a number, it does not own or reference a `T`).
pub struct Idx<T> {
    raw: u32,
    _phantom: PhantomData<fn() -> T>,
}

impl<T> Idx<T> {
    pub(crate) fn new(raw: u32) -> Self {
        Self {
            raw,
            _phantom: PhantomData,
        }
    }

    pub(crate) fn as_usize(self) -> usize {
        usize::try_from(self.raw).expect("arena index requires 32-bit+ usize")
    }
}

impl<T> Clone for Idx<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Idx<T> {}

impl<T> PartialEq for Idx<T> {
    fn eq(&self, other: &Self) -> bool {
        self.raw == other.raw
    }
}

impl<T> Eq for Idx<T> {}

impl<T> PartialOrd for Idx<T> {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for Idx<T> {
    fn cmp(&self, other: &Self) -> Ordering {
        self.raw.cmp(&other.raw)
    }
}

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

// ── Arena<T> ─────────────────────────────────────────────────────────────────

/// A typed bump-allocation pool.
///
/// Items are allocated sequentially and addressed by [`Idx<T>`] handles.
pub struct Arena<T> {
    items: Vec<T>,
}

impl<T> Arena<T> {
    /// Creates an empty arena.
    #[must_use]
    pub const fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Creates an arena pre-allocated for at least `cap` items.
    #[must_use]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            items: Vec::with_capacity(cap),
        }
    }

    /// Allocates a value in the arena and returns its index.
    ///
    /// # Panics
    ///
    /// Panics if the arena already contains `u32::MAX` items.
    #[must_use]
    pub fn alloc(&mut self, value: T) -> Idx<T> {
        let raw = u32::try_from(self.items.len()).expect("arena overflow");
        self.items.push(value);
        Idx::new(raw)
    }

    /// Returns a shared reference to the item at `idx`.
    #[must_use]
    pub fn get(&self, idx: Idx<T>) -> &T {
        &self.items[idx.as_usize()]
    }

    /// Returns an exclusive reference to the item at `idx`.
    pub fn get_mut(&mut self, idx: Idx<T>) -> &mut T {
        &mut self.items[idx.as_usize()]
    }

    /// Returns an iterator over all stored items in allocation order.
    pub fn iter(&self) -> impl Iterator<Item = &T> {
        self.items.iter()
    }

    /// Returns an iterator over `(Idx<T>, &T)` pairs in allocation order.
    pub fn iter_idx(&self) -> impl Iterator<Item = (Idx<T>, &T)> {
        self.items.iter().enumerate().map(|(i, v)| {
            let raw = u32::try_from(i).expect("arena index in range");
            (Idx::new(raw), v)
        })
    }

    /// Returns the number of items in the arena.
    #[must_use]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Returns `true` if the arena contains no items.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl<T> Default for Arena<T> {
    fn default() -> Self {
        Self::new()
    }
}

// ── Slice<T> ─────────────────────────────────────────────────────────────────

/// A compact, copyable handle to a contiguous range of items in a [`SliceArena<T>`].
///
/// `Slice<T>` encodes a start index and length. Like [`Idx<T>`], it uses
/// `PhantomData<fn() -> T>` for `Send + Sync` without `T: Send + Sync` bounds.
pub struct Slice<T> {
    start: u32,
    len: u32,
    _phantom: PhantomData<fn() -> T>,
}

impl<T> Slice<T> {
    /// Creates the canonical empty slice (`start = 0, len = 0`).
    ///
    /// `SliceArena::get_slice(Slice::empty())` always returns `&[]`.
    #[must_use]
    pub const fn empty() -> Self {
        Self {
            start: 0,
            len: 0,
            _phantom: PhantomData,
        }
    }

    /// Returns the number of items this slice covers.
    #[must_use]
    pub fn len(&self) -> usize {
        usize::try_from(self.len).expect("slice length fits in usize")
    }

    /// Returns `true` if the slice covers zero items.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.len == 0
    }
}

impl<T> Clone for Slice<T> {
    fn clone(&self) -> Self {
        *self
    }
}

impl<T> Copy for Slice<T> {}

impl<T> PartialEq for Slice<T> {
    fn eq(&self, other: &Self) -> bool {
        self.start == other.start && self.len == other.len
    }
}

impl<T> Eq for Slice<T> {}

impl<T> Hash for Slice<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.start.hash(state);
        self.len.hash(state);
    }
}

impl<T> fmt::Debug for Slice<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let end = self.start.saturating_add(self.len);
        write!(f, "Slice({}..{})", self.start, end)
    }
}

// ── SliceArena<T> ────────────────────────────────────────────────────────────

/// An arena that stores items contiguously and hands out [`Slice<T>`] handles.
///
/// Unlike [`Arena<T>`] (which allocates one item at a time), `SliceArena<T>`
/// accepts batches of items via [`alloc_slice`](SliceArena::alloc_slice) and
/// returns a `Slice<T>` that is `Copy` — no heap allocation to share the handle.
///
/// This is the intended backing store for multi-element fields in flat AST nodes,
/// replacing `Vec<Param>`, `Vec<TyParam>`, etc.
pub struct SliceArena<T> {
    items: Vec<T>,
}

impl<T> SliceArena<T> {
    /// Creates an empty `SliceArena`.
    #[must_use]
    pub const fn new() -> Self {
        Self { items: Vec::new() }
    }

    /// Creates a `SliceArena` pre-allocated for at least `cap` items.
    #[must_use]
    pub fn with_capacity(cap: usize) -> Self {
        Self {
            items: Vec::with_capacity(cap),
        }
    }

    /// Appends all items from `iter` and returns a [`Slice<T>`] handle.
    ///
    /// # Panics
    ///
    /// Panics if the total number of stored items would exceed `u32::MAX`.
    #[must_use]
    pub fn alloc_slice(&mut self, iter: impl IntoIterator<Item = T>) -> Slice<T> {
        let start = u32::try_from(self.items.len()).expect("slice arena overflow");
        self.items.extend(iter);
        let end = u32::try_from(self.items.len()).expect("slice arena overflow");
        let len = end - start;
        Slice {
            start,
            len,
            _phantom: PhantomData,
        }
    }

    /// Returns a shared reference to the items covered by `slice`.
    #[must_use]
    pub fn get_slice(&self, slice: Slice<T>) -> &[T] {
        let start = usize::try_from(slice.start).expect("slice start in range");
        let len = usize::try_from(slice.len).expect("slice len in range");
        assert!(
            start + len <= self.items.len(),
            "slice [{start}..{}] out of bounds (arena len={})",
            start + len,
            self.items.len()
        );
        &self.items[start..start + len]
    }

    /// Returns a mutable reference to the items covered by `slice`.
    pub fn get_slice_mut(&mut self, slice: Slice<T>) -> &mut [T] {
        let start = usize::try_from(slice.start).expect("slice start in range");
        let len = usize::try_from(slice.len).expect("slice len in range");
        assert!(
            start + len <= self.items.len(),
            "slice [{start}..{}] out of bounds (arena len={})",
            start + len,
            self.items.len()
        );
        &mut self.items[start..start + len]
    }

    /// Returns the total number of items stored across all slices.
    #[must_use]
    pub fn len(&self) -> usize {
        self.items.len()
    }

    /// Returns `true` if no items have been allocated.
    #[must_use]
    pub fn is_empty(&self) -> bool {
        self.items.is_empty()
    }
}

impl<T> Default for SliceArena<T> {
    fn default() -> Self {
        Self::new()
    }
}

// ── Tests ────────────────────────────────────────────────────────────────────

#[cfg(test)]
mod tests;
