use core::marker::PhantomData;
use core::ops::Range;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SliceRange<T> {
    start: u32,
    end: u32,
    marker: PhantomData<fn() -> T>,
}

impl<T> SliceRange<T> {
    pub const EMPTY: Self = Self {
        start: 0,
        end: 0,
        marker: PhantomData::<fn() -> T>,
    };

    #[must_use]
    pub const fn new(start: u32, end: u32) -> Self {
        Self {
            start,
            end,
            marker: PhantomData::<fn() -> T>,
        }
    }

    #[must_use]
    pub const fn len(self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    #[must_use]
    pub const fn is_empty(self) -> bool {
        self.start == self.end
    }

    #[must_use]
    pub fn as_range(self) -> Range<usize> {
        let start = usize::try_from(self.start).unwrap_or(usize::MAX);
        let end = usize::try_from(self.end).unwrap_or(usize::MAX);
        start..end
    }
}

#[derive(Debug, Default)]
pub struct SliceArena<T> {
    data: Vec<T>,
}

impl<T> SliceArena<T> {
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

    pub fn alloc_from_slice(&mut self, values: &[T]) -> SliceRange<T>
    where
        T: Clone,
    {
        self.alloc_from_iter(values.iter().cloned())
    }

    /// Appends `values` to the arena and returns the contiguous range covering the
    /// newly-allocated items.
    ///
    /// # Panics
    /// Panics if the arena exceeds `u32::MAX` items.
    pub fn alloc_from_iter<I>(&mut self, values: I) -> SliceRange<T>
    where
        I: IntoIterator<Item = T>,
    {
        let start =
            u32::try_from(self.data.len()).expect("slice arena overflow: exceeded u32::MAX items");
        self.data.extend(values);
        let end =
            u32::try_from(self.data.len()).expect("slice arena overflow: exceeded u32::MAX items");
        SliceRange::new(start, end)
    }

    #[must_use]
    pub fn get(&self, range: SliceRange<T>) -> &[T] {
        self.data.get(range.as_range()).unwrap_or_default()
    }

    #[must_use]
    pub const fn len(&self) -> usize {
        self.data.len()
    }

    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.data.is_empty()
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
