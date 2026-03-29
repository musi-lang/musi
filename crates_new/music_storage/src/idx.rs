use core::cmp::Ordering;
use core::hash::{Hash, Hasher};
use std::fmt;
use std::marker::PhantomData;

/// Typed index into an `Arena<T>`. 4 bytes, `Copy`, type-safe.
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
    #[must_use]
    pub const fn from_raw(raw: u32) -> Self {
        Self {
            raw,
            _phantom: PhantomData::<fn() -> T>,
        }
    }

    #[must_use]
    pub const fn raw(self) -> u32 {
        self.raw
    }

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
#[allow(clippy::unwrap_used)]
mod tests;
