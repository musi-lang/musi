use std::fmt;
use std::marker::PhantomData;

/// Typed index into an `Arena<T>`. 4 bytes, `Copy`, type-safe.
///
/// Two `Idx` values with different type parameters are incompatible at the type
/// level, preventing accidental cross-arena lookups.
#[derive(Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Idx<T> {
    raw: u32,
    _phantom: PhantomData<fn() -> T>,
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
