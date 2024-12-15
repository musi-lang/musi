use std::sync::Arc;

use crate::core::source::Source;

pub struct Cursor {
    pub source: Arc<Source>,
    pub position: usize,
    pub current_line: usize,
}

impl Cursor {
    #[inline]
    pub fn peek(&self) -> Option<u8> {
        self.source.content.get(self.position).copied()
    }

    #[inline]
    pub fn peek_next(&self) -> Option<u8> {
        self.source
            .content
            .get(self.position.checked_add(1).unwrap_or(self.position))
            .copied()
    }

    #[inline]
    pub fn advance(&mut self) -> Option<u8> {
        let current = self.peek()?;
        self.position = self.position.checked_add(1).unwrap_or(self.position);

        if current == b'\n' {
            self.current_line = self
                .current_line
                .checked_add(1)
                .unwrap_or(self.current_line);
        }

        Some(current)
    }

    #[inline]
    pub fn advance_by(&mut self, count: usize) {
        for _ in 0..count {
            if self.advance().is_none() {
                break;
            }
        }
    }

    #[inline]
    pub fn match_2byte(&self, first: u8, second: u8) -> bool {
        matches!(self.source.content.get(self.position..=self.position.saturating_add(1)),
            Some(&[current, next]) if current == first && next == second)
    }

    #[inline]
    pub fn match_3byte(&self, first: u8, second: u8, third: u8) -> bool {
        matches!(self.source.content.get(self.position..=self.position.saturating_add(2)),
            Some(&[current, next, last]) if current == first && next == second && last == third)
    }

    #[inline]
    pub fn slice_from(&self, start: usize) -> &[u8] {
        if (start <= self.position) && (self.position <= self.source.content.len()) {
            self.source.content.get(start..self.position).unwrap_or(&[])
        } else {
            &[]
        }
    }
}
