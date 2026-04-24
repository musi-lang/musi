use super::super::space::{
    HeapAllocation, HeapSpace, IMMIX_CARDS_PER_BLOCK, IMMIX_LINE_BYTES, IMMIX_LINES_PER_BLOCK,
    ImmixBlock,
};
use super::RuntimeHeap;

impl RuntimeHeap {
    pub(in crate::gc) fn live_object_count(&self) -> usize {
        self.slots
            .iter()
            .filter(|slot| slot.object.is_some())
            .count()
    }

    pub(in crate::gc) fn allocate_in_space(
        &mut self,
        space: HeapSpace,
        bytes: usize,
    ) -> HeapAllocation {
        let line_count = bytes.div_ceil(IMMIX_LINE_BYTES).max(1);
        self.allocate_lines_excluding(space, line_count, &[])
    }

    pub(in crate::gc) fn allocate_lines_excluding(
        &mut self,
        space: HeapSpace,
        line_count: usize,
        excluded_blocks: &[usize],
    ) -> HeapAllocation {
        if line_count > IMMIX_LINES_PER_BLOCK {
            return HeapAllocation::Large { space };
        }
        let current_block = match space {
            HeapSpace::Young => self.current_young_block,
            HeapSpace::Mature => self.current_mature_block,
        };
        if let Some(block_index) = current_block
            && !excluded_blocks.contains(&block_index)
            && let Some(block) = self.blocks.get_mut(block_index)
            && block.space == space
            && let Some(start_line) = block.reserve_lines(line_count)
        {
            return HeapAllocation::Immix {
                space,
                block: block_index,
                start_line,
                line_count,
            };
        }
        if space == HeapSpace::Young && current_block.is_some() && excluded_blocks.is_empty() {
            return self.allocate_new_block(space, line_count);
        }
        for (block_index, block) in self.blocks.iter_mut().enumerate() {
            if excluded_blocks.contains(&block_index) {
                continue;
            }
            if block.space != space {
                continue;
            }
            if let Some(start_line) = block.reserve_lines(line_count) {
                match space {
                    HeapSpace::Young => self.current_young_block = Some(block_index),
                    HeapSpace::Mature => self.current_mature_block = Some(block_index),
                }
                return HeapAllocation::Immix {
                    space,
                    block: block_index,
                    start_line,
                    line_count,
                };
            }
        }
        self.allocate_new_block(space, line_count)
    }

    fn allocate_new_block(&mut self, space: HeapSpace, line_count: usize) -> HeapAllocation {
        let block_index = self.blocks.len();
        let mut block = ImmixBlock::new(space);
        let start_line = block.reserve_lines(line_count).unwrap_or_default();
        self.blocks.push(block);
        if space == HeapSpace::Mature {
            self.mature_card_table.push([false; IMMIX_CARDS_PER_BLOCK]);
        }
        match space {
            HeapSpace::Young => self.current_young_block = Some(block_index),
            HeapSpace::Mature => self.current_mature_block = Some(block_index),
        }
        HeapAllocation::Immix {
            space,
            block: block_index,
            start_line,
            line_count,
        }
    }

    pub(in crate::gc) fn mark_allocation(&mut self, allocation: HeapAllocation) {
        let HeapAllocation::Immix {
            space: _,
            block,
            start_line,
            line_count,
        } = allocation
        else {
            return;
        };
        if let Some(block) = self.blocks.get_mut(block) {
            block.mark_lines(start_line, line_count);
        }
    }

    pub(in crate::gc) fn release_allocation(&mut self, allocation: HeapAllocation) {
        let HeapAllocation::Immix {
            space: _,
            block,
            start_line,
            line_count,
        } = allocation
        else {
            return;
        };
        if let Some(block) = self.blocks.get_mut(block) {
            block.release_lines(start_line, line_count);
        }
    }

    pub(in crate::gc) fn finish_line_sweep(&mut self) {
        for block in &mut self.blocks {
            block.finish_collection();
        }
        if self
            .current_young_block
            .is_some_and(|block| self.blocks.get(block).is_none_or(ImmixBlock::is_free))
        {
            self.current_young_block = None;
        }
        if self
            .current_mature_block
            .is_some_and(|block| self.blocks.get(block).is_none_or(ImmixBlock::is_free))
        {
            self.current_mature_block = None;
        }
    }

    pub(in crate::gc) fn fragmented_blocks(&self) -> Vec<usize> {
        self.blocks
            .iter()
            .enumerate()
            .filter_map(|(index, block)| {
                let live = block.live_lines();
                let free = block.free_lines();
                (live > 0 && free > live).then_some(index)
            })
            .collect()
    }

    pub(in crate::gc) fn free_blocks(&self) -> usize {
        self.blocks.iter().filter(|block| block.is_free()).count()
    }
}
