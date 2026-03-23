use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use std::fmt;
use std::marker::PhantomData;

/// Typed index into an `Arena<T>`. 4 bytes, `Copy`, type-safe.
///
/// Two `Idx` values with different type parameters are incompatible at the type
/// level, preventing accidental cross-arena lookups.
///
/// Trait impls are manual (not derived) so that `Idx<T>` is unconditionally
/// `Copy + Eq + Hash + Ord` regardless of `T`. Edition 2024 derives would
/// propagate bounds from `PhantomData<fn() -> T>` to `T`.
pub struct Idx<T> {
    raw: u32,
    _phantom: PhantomData<fn() -> T>,
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

impl<T> Hash for Idx<T> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        self.raw.hash(state);
    }
}

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

impl<T> Idx<T> {
    /// Creates an `Idx` from a raw `u32` value.
    ///
    /// Intended for deserialization and interop. The caller is responsible for
    /// ensuring the raw value refers to a valid arena slot.
    #[must_use]
    pub const fn from_raw(raw: u32) -> Self {
        Self {
            raw,
            _phantom: PhantomData::<fn() -> T>,
        }
    }

    /// Returns the underlying raw `u32` index.
    #[must_use]
    pub const fn raw(self) -> u32 {
        self.raw
    }

    /// Creates an `Idx` from a raw value. Only the arena should call this.
    #[must_use]
    pub(crate) const fn new(raw: u32) -> Self {
        Self {
            raw,
            _phantom: PhantomData::<fn() -> T>,
        }
    }
}

impl<T> fmt::Debug for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        f.debug_struct("Idx").field("raw", &self.raw).finish()
    }
}

impl<T> fmt::Display for Idx<T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.raw)
    }
}

#[cfg(test)]
mod tests;
