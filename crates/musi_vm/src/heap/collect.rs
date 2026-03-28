use super::layout::{is_large, BlockState, CellSlot, LargeEntry, INITIAL_GC_THRESHOLD, LARGE_FLAG, LINES_PER_BLOCK, CELLS_PER_LINE};
use super::Heap;
use crate::value::Value;

impl Heap {
    pub fn collect_minor(&mut self, roots: &[Value]) {
        for &val in roots {
            self.mark_value_minor(val);
        }
        let remembered: Vec<usize> = self.gc.remembered_set.clone();
        for &addr in &remembered {
            self.mark_children_minor(addr);
        }
        self.sweep_lines();
        self.gc.remembered_set.clear();
        self.gc.minor_cycles += 1;
    }

    pub fn collect_major(&mut self, roots: &[Value]) {
        self.clear_all_marks();
        for &val in roots {
            self.mark_value(val);
        }
        self.sweep_lines();
        self.gc.minor_cycles = 0;
    }

    fn clear_all_marks(&mut self) {
        for block in &mut self.blocks {
            block.meta.line_marks = [0; LINES_PER_BLOCK];
            for cell in &mut block.cells {
                cell.set_marked(false);
            }
        }
        for entry in self.large.objects.iter_mut().flatten() {
            entry.set_marked(false);
        }
    }

    fn sweep_lines(&mut self) {
        for block in &mut self.blocks {
            let mut all_clear = true;
            let mut all_marked = true;
            block.meta.hole_count = 0;
            for line in 1..LINES_PER_BLOCK {
                if block.meta.line_marks[line] == 0 {
                    all_marked = false;
                    block.meta.hole_count += 1;
                    for cell_offset in 0..CELLS_PER_LINE {
                        let cell_idx = (line - 1) * CELLS_PER_LINE + cell_offset;
                        if cell_idx < block.cells.len() && !block.cells[cell_idx].is_pinned() {
                            block.cells[cell_idx].obj = None;
                            block.cells[cell_idx].set_marked(false);
                        }
                    }
                } else {
                    all_clear = false;
                }
            }
            block.meta.state = if all_clear {
                BlockState::Free
            } else if all_marked {
                BlockState::Full
            } else {
                BlockState::Recyclable
            };
        }

        for (idx, slot) in self.large.objects.iter_mut().enumerate() {
            if let Some(entry) = slot {
                if !entry.is_marked() && !entry.is_pinned() {
                    *slot = None;
                    self.large.free.push(idx);
                }
            }
        }

        self.rebuild_block_lists();
    }

    fn rebuild_block_lists(&mut self) {
        self.free_blocks.clear();
        self.recyclable_blocks.clear();
        for (idx, block) in self.blocks.iter().enumerate() {
            if idx == self.bump.block {
                continue;
            }
            match block.meta.state {
                BlockState::Free => self.free_blocks.push(idx),
                BlockState::Recyclable => self.recyclable_blocks.push(idx),
                BlockState::Full => {}
            }
        }
    }

    #[must_use]
    pub const fn should_collect(&self) -> bool {
        self.gc.allocation_count >= self.gc.threshold
    }

    #[must_use]
    pub const fn should_major(&self) -> bool {
        self.gc.minor_cycles >= self.gc.major_interval
    }

    pub fn reset_threshold(&mut self) {
        let live = self.live_count();
        self.gc.threshold = (live * 2).max(INITIAL_GC_THRESHOLD);
        self.gc.allocation_count = 0;
    }

    pub fn pin(&mut self, addr: usize) {
        if is_large(addr) {
            let idx = addr & !LARGE_FLAG;
            if let Some(entry) = self.large.objects.get_mut(idx).and_then(|s| s.as_mut()) {
                entry.set_pinned(true);
            }
        } else if let Some(cell) = self.get_cell_mut(addr) {
            cell.set_pinned(true);
        }
    }

    pub fn unpin(&mut self, addr: usize) {
        if is_large(addr) {
            let idx = addr & !LARGE_FLAG;
            if let Some(entry) = self.large.objects.get_mut(idx).and_then(|s| s.as_mut()) {
                entry.set_pinned(false);
            }
        } else if let Some(cell) = self.get_cell_mut(addr) {
            cell.set_pinned(false);
        }
    }

    pub fn remember(&mut self, addr: usize) {
        if !self.gc.remembered_set.contains(&addr) {
            self.gc.remembered_set.push(addr);
        }
    }

    #[must_use]
    pub fn is_marked(&self, addr: usize) -> bool {
        if is_large(addr) {
            let idx = addr & !LARGE_FLAG;
            return self
                .large
                .objects
                .get(idx)
                .and_then(|s| s.as_ref())
                .is_some_and(LargeEntry::is_marked);
        }
        self.get_cell(addr).is_some_and(CellSlot::is_marked)
    }

    #[must_use]
    pub fn live_count(&self) -> usize {
        let block_live: usize = self
            .blocks
            .iter()
            .map(|b| b.cells.iter().filter(|c| c.obj.is_some()).count())
            .sum();
        let large_live = self.large.objects.iter().filter(|s| s.is_some()).count();
        block_live + large_live
    }
}
