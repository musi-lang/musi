use crate::value::GcRef;

use super::object::HeapObject;

pub(super) const IMMIX_LINE_BYTES: usize = 128;
const IMMIX_BLOCK_BYTES: usize = 32 * 1024;
pub(super) const IMMIX_LINES_PER_BLOCK: usize = IMMIX_BLOCK_BYTES / IMMIX_LINE_BYTES;
pub(super) const IMMIX_CARD_LINES: usize = 8;
pub(super) const IMMIX_CARDS_PER_BLOCK: usize = IMMIX_LINES_PER_BLOCK / IMMIX_CARD_LINES;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum HeapSpace {
    Young,
    Mature,
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum HeapAllocation {
    Immix {
        space: HeapSpace,
        block: usize,
        start_line: usize,
        line_count: usize,
    },
    Large {
        space: HeapSpace,
    },
}

#[derive(Debug, Clone)]
pub(super) struct HeapSlot {
    pub(super) generation: u32,
    pub(super) space: HeapSpace,
    pub(super) survive_count: u8,
    pub(super) object: Option<HeapObject>,
    pub(super) allocation: HeapAllocation,
    pub(super) bytes: usize,
    pub(super) mark_epoch: u32,
}

impl HeapSlot {
    pub(super) const fn live_with_bytes(
        generation: u32,
        space: HeapSpace,
        object: HeapObject,
        allocation: HeapAllocation,
        bytes: usize,
    ) -> Self {
        Self {
            generation,
            space,
            survive_count: 0,
            object: Some(object),
            allocation,
            bytes,
            mark_epoch: 0,
        }
    }

    pub(super) fn line_count(&self) -> usize {
        self.bytes.div_ceil(IMMIX_LINE_BYTES).max(1)
    }

    pub(super) const fn is_live_generation(&self, reference: GcRef) -> bool {
        self.generation == reference.generation() && self.object.is_some()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LineState {
    Free,
    Allocated,
    Marked,
}

#[derive(Debug, Clone)]
pub(super) struct ImmixBlock {
    pub(super) space: HeapSpace,
    lines: Vec<LineState>,
    cursor: usize,
}

impl ImmixBlock {
    #[must_use]
    pub(super) fn new(space: HeapSpace) -> Self {
        Self {
            space,
            lines: vec![LineState::Free; IMMIX_LINES_PER_BLOCK],
            cursor: 0,
        }
    }

    pub(super) fn is_free(&self) -> bool {
        self.lines.iter().all(|line| *line == LineState::Free)
    }

    pub(super) fn live_lines(&self) -> usize {
        self.lines
            .iter()
            .filter(|line| matches!(line, LineState::Allocated | LineState::Marked))
            .count()
    }

    pub(super) fn free_lines(&self) -> usize {
        self.lines
            .iter()
            .filter(|line| **line == LineState::Free)
            .count()
    }

    pub(super) fn reserve_lines(&mut self, line_count: usize) -> Option<usize> {
        if line_count > self.lines.len() {
            return None;
        }
        let limit = self.lines.len().saturating_sub(line_count);
        if let Some(start) = self.reserve_lines_in(line_count, self.cursor, limit) {
            return Some(start);
        }
        if self.cursor > 0 {
            let wrapped_limit = self.cursor.saturating_sub(1).min(limit);
            if let Some(start) = self.reserve_lines_in(line_count, 0, wrapped_limit) {
                return Some(start);
            }
        }
        None
    }

    fn reserve_lines_in(&mut self, line_count: usize, start: usize, limit: usize) -> Option<usize> {
        if start > limit {
            return None;
        }
        for start_line in start..=limit {
            if self.lines[start_line..start_line + line_count]
                .iter()
                .all(|line| *line == LineState::Free)
            {
                for line in &mut self.lines[start_line..start_line + line_count] {
                    *line = LineState::Allocated;
                }
                self.cursor = start_line.saturating_add(line_count).min(self.lines.len());
                return Some(start_line);
            }
        }
        None
    }

    pub(super) fn mark_lines(&mut self, start_line: usize, line_count: usize) {
        let end = start_line.saturating_add(line_count).min(self.lines.len());
        for line in &mut self.lines[start_line..end] {
            *line = LineState::Marked;
        }
    }

    pub(super) fn release_lines(&mut self, start_line: usize, line_count: usize) {
        let end = start_line.saturating_add(line_count).min(self.lines.len());
        for line in &mut self.lines[start_line..end] {
            *line = LineState::Free;
        }
        self.cursor = self.cursor.min(start_line);
    }

    pub(super) fn finish_collection(&mut self) {
        for line in &mut self.lines {
            *line = match *line {
                LineState::Marked => LineState::Allocated,
                LineState::Allocated | LineState::Free => *line,
            };
        }
    }
}

#[cfg(test)]
mod success {
    use super::{HeapSpace, IMMIX_LINES_PER_BLOCK, ImmixBlock};

    #[test]
    fn full_block_reservation_does_not_probe_past_end() {
        let mut block = ImmixBlock::new(HeapSpace::Young);

        assert_eq!(block.reserve_lines(IMMIX_LINES_PER_BLOCK), Some(0));
        assert_eq!(block.reserve_lines(1), None);
        assert_eq!(block.reserve_lines(2), None);
    }

    #[test]
    fn wrapped_reservation_does_not_probe_past_end() {
        let mut block = ImmixBlock::new(HeapSpace::Young);

        assert_eq!(block.reserve_lines(IMMIX_LINES_PER_BLOCK - 1), Some(0));
        assert_eq!(block.reserve_lines(2), None);
    }
}

#[cfg(test)]
mod failure {}
