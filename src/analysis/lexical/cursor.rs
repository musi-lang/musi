use std::sync::Arc;

use crate::core::source::NamedSource;

pub struct Cursor {
    pub source: Arc<NamedSource>,
    pub offset: usize,
    pub line: usize,
}

impl Cursor {
    #[inline]
    pub fn advance(&mut self) -> Option<u8> {
        let current_byte = self.current_byte()?;

        self.offset = self.offset.saturating_add(1);

        self.line = if current_byte == b'\n' {
            self.line.saturating_add(1)
        } else {
            self.line
        };
        Some(current_byte)
    }

    #[inline]
    pub fn advance_by(&mut self, mut remaining_steps: usize) {
        while remaining_steps > 0 && self.advance().is_some() {
            remaining_steps = remaining_steps.saturating_sub(1);
        }
    }

    #[inline]
    pub fn is_at_end(&self) -> bool {
        self.offset >= self.source.bytes.len()
    }

    #[inline]
    pub fn matches_byte_pair(&self, expected: u8, next: u8) -> bool {
        if self.offset >= self.source.bytes.len().saturating_sub(1) {
            return false;
        }

        matches!(self.source.bytes.get(self.offset..=self.offset.saturating_add(1)),
            Some(&[first, second]) if first == expected && second == next)
    }

    #[inline]
    pub fn matches_byte_triplet(&self, expected: u8, next: u8, last: u8) -> bool {
        matches!(self.source.bytes.get(self.offset..=self.offset.saturating_add(2)),
            Some(&[first, second, third]) if first == expected && second == next && third == last)
    }

    #[inline]
    pub fn current_byte(&self) -> Option<u8> {
        self.source.bytes.get(self.offset).copied()
    }

    #[inline]
    pub fn next_byte(&self) -> Option<u8> {
        self.source
            .bytes
            .get(self.offset.checked_add(1).unwrap_or(self.offset))
            .copied()
    }

    #[inline]
    pub fn remaining_bytes(&self) -> usize {
        self.source.bytes.len().saturating_sub(self.offset)
    }

    #[inline]
    pub fn byte_slice_from(&self, start: usize) -> &[u8] {
        if start > self.offset {
            return &[];
        }

        self.source.bytes.get(start..self.offset).unwrap_or(&[])
    }
}
