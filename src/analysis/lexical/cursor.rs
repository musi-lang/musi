use std::sync::Arc;

use crate::core::source::SourceFile;

pub struct Cursor {
    pub source: Arc<SourceFile>,
    pub position: usize,
    pub line: usize,
}

impl Cursor {
    #[inline]
    pub fn advance(&mut self) -> Option<u8> {
        let current = self.peek()?;

        self.position = self.position.saturating_add(1);

        self.line = if current == b'\n' {
            self.line.saturating_add(1)
        } else {
            self.line
        };
        Some(current)
    }

    #[inline]
    pub fn advance_by(&mut self, mut remaining: usize) {
        while remaining > 0 && self.advance().is_some() {
            remaining = remaining.saturating_sub(1);
        }
    }

    #[inline]
    pub fn is_at_end(&self) -> bool {
        self.position >= self.source.content.len()
    }

    #[inline]
    pub fn matches_pair(&self, expected: u8, next: u8) -> bool {
        if self.position >= self.source.content.len().saturating_sub(1) {
            return false;
        }

        matches!(self.source.content.get(self.position..=self.position.saturating_add(1)),
            Some(&[first, second]) if first == expected && second == next)
    }

    #[inline]
    pub fn matches_triplet(&self, expected: u8, next: u8, last: u8) -> bool {
        matches!(self.source.content.get(self.position..=self.position.saturating_add(2)),
            Some(&[first, second, third]) if first == expected && second == next && third == last)
    }

    #[inline]
    pub fn peek(&self) -> Option<u8> {
        self.source.content.get(self.position).copied()
    }

    #[inline]
    pub fn peek_next(&self) -> Option<u8> {
        self.source
            .content
            .get(self.position.saturating_add(1))
            .copied()
    }

    #[inline]
    pub fn remaining(&self) -> usize {
        self.source.content.len().saturating_sub(self.position)
    }

    #[inline]
    pub fn slice_from(&self, start: usize) -> &[u8] {
        if start > self.position {
            return &[];
        }

        self.source.content.get(start..self.position).unwrap_or(&[])
    }
}
