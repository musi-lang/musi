use std::sync::Arc;

use crate::core::source::Source;

pub struct Cursor {
    pub source: Arc<Source>,
    pub byte_offset: usize,
    pub line_number: usize,
}

impl Cursor {
    #[inline]
    pub fn advance(&mut self) -> Option<u8> {
        let current = self.current_byte()?;

        self.byte_offset = self.byte_offset.saturating_add(1);

        self.line_number = if current == b'\n' {
            self.line_number.saturating_add(1)
        } else {
            self.line_number
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
        self.byte_offset >= self.source.bytes.len()
    }

    #[inline]
    pub fn matches_byte_pair(&self, expected: u8, next: u8) -> bool {
        if self.byte_offset >= self.source.bytes.len().saturating_sub(1) {
            return false;
        }

        matches!(self.source.bytes.get(self.byte_offset..=self.byte_offset.saturating_add(1)),
            Some(&[first, second]) if first == expected && second == next)
    }

    #[inline]
    pub fn matches_byte_triplet(&self, expected: u8, next: u8, last: u8) -> bool {
        matches!(self.source.bytes.get(self.byte_offset..=self.byte_offset.saturating_add(2)),
            Some(&[first, second, third]) if first == expected && second == next && third == last)
    }

    #[inline]
    pub fn current_byte(&self) -> Option<u8> {
        self.source.bytes.get(self.byte_offset).copied()
    }

    #[inline]
    pub fn next_byte(&self) -> Option<u8> {
        self.source
            .bytes
            .get(self.byte_offset.checked_add(1).unwrap_or(self.byte_offset))
            .copied()
    }

    #[inline]
    pub fn remaining_bytes(&self) -> usize {
        self.source.bytes.len().saturating_sub(self.byte_offset)
    }

    #[inline]
    pub fn byte_slice_from(&self, start: usize) -> &[u8] {
        if start > self.byte_offset {
            return &[];
        }

        self.source
            .bytes
            .get(start..self.byte_offset)
            .unwrap_or(&[])
    }
}
