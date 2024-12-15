use std::sync::Arc;

use crate::core::source::Source;

pub(super) struct Cursor {
    pub(super) source: Arc<Source>,
    pub position: usize,
    pub(super) current_line: usize,
}

impl Cursor {
    pub(super) const fn new(source: Arc<Source>) -> Self {
        Self {
            source,
            position: 0,
            current_line: 0,
        }
    }

    #[inline]
    pub(super) fn peek(&self) -> Option<u8> {
        self.source.content.get(self.position).copied()
    }

    #[inline]
    pub(super) fn peek_next(&self) -> Option<u8> {
        self.source.content.get(self.position + 1).copied()
    }

    #[inline]
    pub(super) fn advance(&mut self) -> Option<u8> {
        let current = self.peek()?;
        self.position += 1;

        if current == b'\n' {
            self.current_line += 1;
        }

        Some(current)
    }

    #[inline]
    pub(super) fn advance_by(&mut self, count: usize) {
        for _ in 0..count {
            if self.advance().is_none() {
                break;
            }
        }
    }

    #[inline]
    pub(super) fn match_2byte(&self, first: u8, second: u8) -> bool {
        matches!(self.source.content.get(self.position..=self.position + 1),
            Some([current, next]) if *current == first && *next == second)
    }

    #[inline]
    pub(super) fn match_3byte(&self, first: u8, second: u8, third: u8) -> bool {
        matches!(self.source.content.get(self.position..=self.position + 2),
            Some([current, next, last]) if *current == first && *next == second && *last == third)
    }

    #[inline]
    pub(super) fn slice_from(&self, start: usize) -> &[u8] {
        if (start <= self.position) && (self.position <= self.source.content.len()) {
            &self.source.content[start..self.position]
        } else {
            &[]
        }
    }
}
