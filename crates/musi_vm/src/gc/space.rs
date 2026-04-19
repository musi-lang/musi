use crate::value::GcRef;

use super::object::HeapObject;

pub(super) const IMMIX_LINE_BYTES: usize = 128;
const IMMIX_BLOCK_BYTES: usize = 32 * 1024;
pub(super) const IMMIX_LINES_PER_BLOCK: usize = IMMIX_BLOCK_BYTES / IMMIX_LINE_BYTES;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(super) enum HeapAllocation {
    Immix {
        block: usize,
        start_line: usize,
        line_count: usize,
    },
    Large,
}

#[derive(Debug, Clone)]
pub(super) struct HeapSlot {
    pub(super) generation: u32,
    pub(super) object: Option<HeapObject>,
    pub(super) allocation: HeapAllocation,
    pub(super) bytes: usize,
    pub(super) is_marked: bool,
    pub(super) is_pinned: bool,
}

impl HeapSlot {
    pub(super) fn live(generation: u32, object: HeapObject, allocation: HeapAllocation) -> Self {
        let bytes = object.bytes();
        Self {
            generation,
            object: Some(object),
            allocation,
            bytes,
            is_marked: false,
            is_pinned: false,
        }
    }

    pub(super) fn line_count(&self) -> usize {
        self.bytes.div_ceil(IMMIX_LINE_BYTES).max(1)
    }

    pub(super) const fn is_live_generation(&self, reference: GcRef) -> bool {
        self.generation == reference.generation && self.object.is_some()
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
enum LineState {
    Free,
    Used,
}

#[derive(Debug, Clone)]
pub(super) struct ImmixBlock {
    lines: Vec<LineState>,
}

impl Default for ImmixBlock {
    fn default() -> Self {
        Self {
            lines: vec![LineState::Free; IMMIX_LINES_PER_BLOCK],
        }
    }
}

impl ImmixBlock {
    pub(super) fn is_free(&self) -> bool {
        self.lines.iter().all(|line| *line == LineState::Free)
    }

    pub(super) fn reserve_lines(&mut self, line_count: usize) -> Option<usize> {
        if line_count > self.lines.len() {
            return None;
        }
        let limit = self.lines.len().saturating_sub(line_count);
        for start in 0..=limit {
            if self.lines[start..start + line_count]
                .iter()
                .all(|line| *line == LineState::Free)
            {
                for line in &mut self.lines[start..start + line_count] {
                    *line = LineState::Used;
                }
                return Some(start);
            }
        }
        None
    }
}
