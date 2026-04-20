use crate::types::VmResult;
use crate::value::GcRef;

use super::super::space::{HeapAllocation, HeapSpace, IMMIX_CARD_LINES, IMMIX_CARDS_PER_BLOCK};
use super::RuntimeHeap;

impl RuntimeHeap {
    pub(crate) fn mark_write_barrier(&mut self, reference: GcRef) -> VmResult {
        let (space, allocation) = {
            let slot = self.slot(reference)?;
            (slot.space, slot.allocation)
        };
        if space != HeapSpace::Mature {
            return Ok(());
        }
        self.mark_allocation_cards(allocation, reference.slot());
        Ok(())
    }

    fn mark_allocation_cards(&mut self, allocation: HeapAllocation, slot_index: usize) {
        match allocation {
            HeapAllocation::Immix {
                space: HeapSpace::Mature,
                block,
                start_line,
                line_count,
            } => {
                let Some(card_index) = self.mature_card_index(block) else {
                    return;
                };
                if let Some(cards) = self.mature_card_table.get_mut(card_index) {
                    let first = start_line / IMMIX_CARD_LINES;
                    let end_line = start_line.saturating_add(line_count).saturating_sub(1);
                    let last = end_line / IMMIX_CARD_LINES;
                    for card in cards
                        .iter_mut()
                        .take(last.min(IMMIX_CARDS_PER_BLOCK.saturating_sub(1)) + 1)
                        .skip(first)
                    {
                        *card = true;
                    }
                }
            }
            HeapAllocation::Large {
                space: HeapSpace::Mature,
            } => {
                if !self.remembered_large_slots.contains(&slot_index) {
                    self.remembered_large_slots.push(slot_index);
                }
            }
            HeapAllocation::Immix {
                space: HeapSpace::Young,
                ..
            }
            | HeapAllocation::Large {
                space: HeapSpace::Young,
            } => {}
        }
    }

    pub(in crate::gc) fn mature_card_index(&self, block_index: usize) -> Option<usize> {
        let block = self.blocks.get(block_index)?;
        if block.space != HeapSpace::Mature {
            return None;
        }
        let mature_before = self.blocks[..block_index]
            .iter()
            .filter(|candidate| candidate.space == HeapSpace::Mature)
            .count();
        Some(mature_before)
    }
}
