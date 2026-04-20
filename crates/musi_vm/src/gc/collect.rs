use crate::value::GcRef;
#[cfg(test)]
use crate::value::Value;
use smallvec::SmallVec;

use super::heap::{
    HeapCollectionStats, LARGE_PROMOTE_BYTES, PROMOTE_SURVIVE_THRESHOLD, RuntimeHeap,
};
use super::space::{HeapAllocation, HeapSpace, IMMIX_CARD_LINES, IMMIX_CARDS_PER_BLOCK};

impl RuntimeHeap {
    #[cfg(test)]
    pub(crate) fn collect_from_roots<'a>(
        &mut self,
        roots: impl IntoIterator<Item = &'a Value>,
    ) -> HeapCollectionStats {
        let refs = roots
            .into_iter()
            .filter_map(Value::gc_ref)
            .collect::<Vec<_>>();
        self.collect_major_from_refs(refs)
    }

    pub(crate) fn collect_minor_from_refs(
        &mut self,
        roots: impl IntoIterator<Item = GcRef>,
    ) -> HeapCollectionStats {
        self.clear_marks();
        for reference in roots {
            self.mark_ref_young(reference);
        }
        self.mark_from_dirty_mature_cards();
        let remembered = self.collect_remembered_survivor_cards();
        let stats = self.finish_minor_collection();
        self.install_remembered_cards(remembered);
        stats
    }

    pub(crate) fn collect_major_from_refs(
        &mut self,
        roots: impl IntoIterator<Item = GcRef>,
    ) -> HeapCollectionStats {
        self.clear_marks();
        for reference in roots {
            self.mark_ref_all(reference);
        }
        let stats = self.finish_major_collection();
        self.clear_mature_cards();
        self.remembered_large_slots.clear();
        stats
    }

    fn finish_minor_collection(&mut self) -> HeapCollectionStats {
        let before_bytes = self.allocated_bytes;
        let before_objects = self.live_object_count();
        let mut reclaimed_objects = 0;
        for slot_index in 0..self.slots.len() {
            let Some(slot) = self.slots.get(slot_index) else {
                continue;
            };
            if slot.space != HeapSpace::Young {
                continue;
            }
            let should_reclaim = slot.object.is_some() && !slot.is_marked && !slot.is_pinned;
            if should_reclaim {
                reclaimed_objects += self.reclaim_slot(slot_index);
                continue;
            }
            let should_promote =
                slot.object.is_some() && slot.is_marked && Self::should_promote(slot);
            if should_promote {
                self.promote_slot(slot_index);
            } else if let Some(slot) = self.slots.get_mut(slot_index)
                && slot.object.is_some()
                && slot.space == HeapSpace::Young
            {
                slot.survive_count = slot.survive_count.saturating_add(1);
                slot.is_marked = false;
            }
        }
        self.finish_line_sweep();
        let free_blocks = self.free_blocks();
        HeapCollectionStats {
            before_bytes,
            after_bytes: self.allocated_bytes,
            before_objects,
            after_objects: self.live_object_count(),
            reclaimed_bytes: before_bytes.saturating_sub(self.allocated_bytes),
            reclaimed_objects,
            free_blocks,
            evacuated_objects: 0,
        }
    }

    fn finish_major_collection(&mut self) -> HeapCollectionStats {
        let before_bytes = self.allocated_bytes;
        let before_objects = self.live_object_count();
        let mut reclaimed_objects = 0;
        for slot_index in 0..self.slots.len() {
            let should_reclaim = self.slots[slot_index].object.is_some()
                && !self.slots[slot_index].is_marked
                && !self.slots[slot_index].is_pinned;
            if !should_reclaim {
                if let Some(slot) = self.slots.get_mut(slot_index) {
                    slot.is_marked = false;
                }
                continue;
            }
            reclaimed_objects += self.reclaim_slot(slot_index);
        }
        for slot in &mut self.slots {
            slot.is_marked = false;
        }
        self.finish_line_sweep();
        let evacuated_objects = if reclaimed_objects == 0 {
            0
        } else {
            self.evacuate_fragmented_blocks()
        };
        let free_blocks = self.free_blocks();
        HeapCollectionStats {
            before_bytes,
            after_bytes: self.allocated_bytes,
            before_objects,
            after_objects: self.live_object_count(),
            reclaimed_bytes: before_bytes.saturating_sub(self.allocated_bytes),
            reclaimed_objects,
            free_blocks,
            evacuated_objects,
        }
    }

    fn clear_marks(&mut self) {
        for slot in &mut self.slots {
            slot.is_marked = false;
        }
    }

    fn mark_ref_young(&mut self, reference: GcRef) {
        let (allocation, children) = {
            let Ok(slot) = self.slot_mut(reference) else {
                return;
            };
            if slot.space != HeapSpace::Young || slot.is_marked {
                return;
            }
            slot.is_marked = true;
            let allocation = slot.allocation;
            let mut children = SmallVec::<[GcRef; 8]>::new();
            if let Some(object) = slot.object.as_ref() {
                object.visit_children(|child| {
                    if let Some(reference) = child.gc_ref() {
                        children.push(reference);
                    }
                });
            }
            (allocation, children)
        };
        self.mark_allocation(allocation);
        for child in children {
            self.mark_ref_young(child);
        }
    }

    fn mark_ref_all(&mut self, reference: GcRef) {
        let (allocation, children) = {
            let Ok(slot) = self.slot_mut(reference) else {
                return;
            };
            if slot.is_marked {
                return;
            }
            slot.is_marked = true;
            let allocation = slot.allocation;
            let mut children = SmallVec::<[GcRef; 8]>::new();
            if let Some(object) = slot.object.as_ref() {
                object.visit_children(|child| {
                    if let Some(reference) = child.gc_ref() {
                        children.push(reference);
                    }
                });
            }
            (allocation, children)
        };
        self.mark_allocation(allocation);
        for child in children {
            self.mark_ref_all(child);
        }
    }

    fn mark_from_dirty_mature_cards(&mut self) {
        let marked_slots = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(slot_index, slot)| {
                (slot.space == HeapSpace::Mature
                    && slot.object.is_some()
                    && self.slot_in_dirty_card(slot))
                .then_some(slot_index)
            })
            .collect::<Vec<_>>();
        for slot_index in marked_slots {
            if let Some(slot) = self.slots.get(slot_index)
                && let Some(object) = slot.object.as_ref()
            {
                let mut young_children = SmallVec::<[GcRef; 8]>::new();
                object.visit_children(|child| {
                    if let Some(reference) = child.gc_ref() {
                        young_children.push(reference);
                    }
                });
                for child in young_children {
                    self.mark_ref_young(child);
                }
            }
        }
        let remembered_large = self.remembered_large_slots.clone();
        for slot_index in remembered_large {
            let Some(slot) = self.slots.get(slot_index) else {
                continue;
            };
            if slot.object.is_none() || slot.space != HeapSpace::Mature {
                continue;
            }
            if let Some(object) = slot.object.as_ref() {
                let mut young_children = SmallVec::<[GcRef; 8]>::new();
                object.visit_children(|child| {
                    if let Some(reference) = child.gc_ref() {
                        young_children.push(reference);
                    }
                });
                for child in young_children {
                    self.mark_ref_young(child);
                }
            }
        }
    }

    fn slot_in_dirty_card(&self, slot: &super::space::HeapSlot) -> bool {
        let HeapAllocation::Immix {
            space: HeapSpace::Mature,
            block,
            start_line,
            line_count,
        } = slot.allocation
        else {
            return false;
        };
        let Some(card_index) = self.mature_card_index(block) else {
            return false;
        };
        let Some(cards) = self.mature_card_table.get(card_index) else {
            return false;
        };
        let first = start_line / IMMIX_CARD_LINES;
        let end_line = start_line.saturating_add(line_count).saturating_sub(1);
        let last = end_line / IMMIX_CARD_LINES;
        (first..=last.min(IMMIX_CARDS_PER_BLOCK.saturating_sub(1))).any(|card| cards[card])
    }

    fn collect_remembered_survivor_cards(&self) -> Vec<[bool; IMMIX_CARDS_PER_BLOCK]> {
        let mut remembered = vec![[false; IMMIX_CARDS_PER_BLOCK]; self.mature_card_table.len()];
        for slot in &self.slots {
            if slot.space != HeapSpace::Mature || slot.object.is_none() {
                continue;
            }
            let HeapAllocation::Immix {
                space: HeapSpace::Mature,
                block,
                start_line,
                line_count,
            } = slot.allocation
            else {
                continue;
            };
            let Some(object) = slot.object.as_ref() else {
                continue;
            };
            let has_young = has_young_child(object, self);
            if !has_young {
                continue;
            }
            let Some(card_index) = self.mature_card_index(block) else {
                continue;
            };
            let Some(cards) = remembered.get_mut(card_index) else {
                continue;
            };
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
        remembered
    }

    fn install_remembered_cards(&mut self, remembered: Vec<[bool; IMMIX_CARDS_PER_BLOCK]>) {
        self.mature_card_table = remembered;
        self.remembered_large_slots = self
            .slots
            .iter()
            .enumerate()
            .filter_map(|(slot_index, slot)| {
                let HeapAllocation::Large {
                    space: HeapSpace::Mature,
                } = slot.allocation
                else {
                    return None;
                };
                let object = slot.object.as_ref()?;
                has_young_child(object, self).then_some(slot_index)
            })
            .collect();
    }

    fn clear_mature_cards(&mut self) {
        for cards in &mut self.mature_card_table {
            *cards = [false; IMMIX_CARDS_PER_BLOCK];
        }
    }

    fn reclaim_slot(&mut self, slot_index: usize) -> usize {
        if let Some(object) = self.slots[slot_index].object.as_mut() {
            object.break_edges();
        }
        self.release_allocation(self.slots[slot_index].allocation);
        let bytes = self.slots[slot_index].bytes;
        if self.slots[slot_index].space == HeapSpace::Young {
            self.young_allocated_bytes = self.young_allocated_bytes.saturating_sub(bytes);
        }
        self.slots[slot_index].object = None;
        self.slots[slot_index].generation = self.slots[slot_index].generation.wrapping_add(1);
        self.slots[slot_index].survive_count = 0;
        self.slots[slot_index].is_marked = false;
        self.slots[slot_index].is_pinned = false;
        self.allocated_bytes = self.allocated_bytes.saturating_sub(bytes);
        self.free_slots.push(slot_index);
        1
    }

    const fn should_promote(slot: &super::space::HeapSlot) -> bool {
        slot.is_pinned
            || slot.bytes >= LARGE_PROMOTE_BYTES
            || slot.survive_count >= PROMOTE_SURVIVE_THRESHOLD
    }

    fn promote_slot(&mut self, slot_index: usize) {
        let Some(slot) = self.slots.get(slot_index) else {
            return;
        };
        if slot.space != HeapSpace::Young || slot.object.is_none() {
            return;
        }
        let bytes = slot.bytes;
        let line_count = slot.line_count();
        let old_allocation = slot.allocation;
        let new_allocation = if line_count > super::space::IMMIX_LINES_PER_BLOCK {
            HeapAllocation::Large {
                space: HeapSpace::Mature,
            }
        } else {
            self.allocate_lines_excluding(HeapSpace::Mature, line_count, &[])
        };
        self.release_allocation(old_allocation);
        if let Some(slot) = self.slots.get_mut(slot_index) {
            slot.space = HeapSpace::Mature;
            slot.survive_count = 0;
            slot.is_marked = false;
            slot.allocation = new_allocation;
        }
        self.young_allocated_bytes = self.young_allocated_bytes.saturating_sub(bytes);
    }

    fn evacuate_fragmented_blocks(&mut self) -> usize {
        let candidates = self.fragmented_blocks();
        if candidates.is_empty() {
            return 0;
        }
        let live_slots = self
            .slots
            .iter()
            .enumerate()
            .filter(|(_, slot)| {
                slot.object.is_some()
                    && !slot.is_pinned
                    && matches!(
                        slot.allocation,
                        HeapAllocation::Immix { block, .. } if candidates.contains(&block)
                    )
            })
            .map(|(slot_index, slot)| (slot_index, slot.allocation, slot.line_count()))
            .collect::<Vec<_>>();
        let mut evacuated = 0;
        for (slot_index, old_allocation, line_count) in live_slots {
            let target_space = self
                .slots
                .get(slot_index)
                .map_or(HeapSpace::Mature, |slot| slot.space);
            let allocation = self.allocate_lines_excluding(target_space, line_count, &candidates);
            if allocation != old_allocation {
                evacuated += 1;
            }
            self.release_allocation(old_allocation);
            if let Some(slot) = self.slots.get_mut(slot_index) {
                slot.allocation = allocation;
            }
        }
        evacuated
    }
}

fn has_young_child(object: &super::object::HeapObject, heap: &RuntimeHeap) -> bool {
    let mut has_young = false;
    object.visit_children(|value| {
        if has_young {
            return;
        }
        let Some(reference) = value.gc_ref() else {
            return;
        };
        let Ok(slot) = heap.slot(reference) else {
            return;
        };
        if slot.space == HeapSpace::Young {
            has_young = true;
        }
    });
    has_young
}
