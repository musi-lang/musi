use std::fmt;

/// A byte-offset range within a source file.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Default)]
pub struct Span {
    pub start: u32,
    pub end: u32,
}

impl Span {
    /// Sentinel span for compiler-generated nodes.
    pub const DUMMY: Self = Self { start: 0, end: 0 };

    /// Create a new span from start (inclusive) to end (exclusive).
    #[must_use]
    pub fn new(start: u32, end: u32) -> Self {
        debug_assert!(
            start <= end,
            "Span::new called with start ({start}) > end ({end})"
        );
        Self { start, end }
    }

    /// Byte length of the span.
    #[must_use]
    pub const fn len(&self) -> u32 {
        self.end.saturating_sub(self.start)
    }

    /// Whether the span covers zero bytes.
    #[must_use]
    pub const fn is_empty(&self) -> bool {
        self.start == self.end
    }

    /// Whether `offset` falls within `[start, end)`.
    #[must_use]
    pub const fn contains(&self, offset: u32) -> bool {
        offset >= self.start && offset < self.end
    }

    /// Smallest span covering both `self` and `other`.
    #[must_use]
    pub const fn merge(self, other: Self) -> Self {
        Self {
            start: if self.start < other.start {
                self.start
            } else {
                other.start
            },
            end: if self.end > other.end {
                self.end
            } else {
                other.end
            },
        }
    }

    /// Span from `self.start` to `end.end`.
    #[must_use]
    pub const fn to(self, end: Self) -> Self {
        Self {
            start: self.start,
            end: end.end,
        }
    }
}

impl fmt::Display for Span {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}..{}", self.start, self.end)
    }
}

/// A value paired with its source location.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct Spanned<T> {
    pub kind: T,
    pub span: Span,
}

impl<T> Spanned<T> {
    /// Wrap a value with its span.
    #[must_use]
    pub const fn new(kind: T, span: Span) -> Self {
        Self { kind, span }
    }

    /// Transform the inner value, preserving the span.
    #[must_use]
    pub fn map<U, F: FnOnce(T) -> U>(self, f: F) -> Spanned<U> {
        Spanned {
            kind: f(self.kind),
            span: self.span,
        }
    }

    /// Borrow the inner value as a reference, preserving the span.
    #[must_use]
    pub const fn as_ref(&self) -> Spanned<&T> {
        Spanned {
            kind: &self.kind,
            span: self.span,
        }
    }

    /// Create a spanned value at `Span::DUMMY`.
    #[must_use]
    pub const fn dummy(kind: T) -> Self {
        Self {
            kind,
            span: Span::DUMMY,
        }
    }
}

#[cfg(test)]
#[allow(clippy::unwrap_used, clippy::panic)]
mod tests;
