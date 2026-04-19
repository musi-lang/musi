use crate::value::{GcRef, Value};

use super::heap::{HeapCollectionStats, RuntimeHeap};
use super::object::HeapObject;

impl RuntimeHeap {
    pub(crate) fn collect_from_roots<'a>(
        &mut self,
        roots: impl IntoIterator<Item = &'a Value>,
    ) -> HeapCollectionStats {
        for root in roots {
            self.mark_value(root);
        }

        let before_bytes = self.allocated_bytes;
        let before_objects = self.live_object_count();
        let mut reclaimed_objects = 0;
        for slot_index in 0..self.slots.len() {
            let should_reclaim = self.slots[slot_index].object.is_some()
                && !self.slots[slot_index].is_marked
                && !self.slots[slot_index].is_pinned;
            if !should_reclaim {
                continue;
            }
            if let Some(object) = self.slots[slot_index].object.as_mut() {
                object.break_edges();
            }
            let bytes = self.slots[slot_index].bytes;
            self.slots[slot_index].object = None;
            self.slots[slot_index].generation = self.slots[slot_index].generation.wrapping_add(1);
            self.slots[slot_index].is_marked = false;
            self.slots[slot_index].is_pinned = false;
            self.allocated_bytes = self.allocated_bytes.saturating_sub(bytes);
            self.free_slots.push(slot_index);
            reclaimed_objects += 1;
        }
        for slot in &mut self.slots {
            slot.is_marked = false;
        }
        let evacuated_objects = self.compact_live_allocations();
        let free_blocks = self.blocks.iter().filter(|block| block.is_free()).count();
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

    fn mark_value(&mut self, source: &Value) {
        let Some(reference) = source.gc_ref() else {
            return;
        };
        self.mark_ref(reference);
    }

    fn mark_ref(&mut self, reference: GcRef) {
        let Ok(slot) = self.slot_mut(reference) else {
            return;
        };
        if slot.is_marked {
            return;
        }
        slot.is_marked = true;
        let children = slot
            .object
            .as_ref()
            .map_or_else(Vec::new, HeapObject::children);
        for child in children {
            self.mark_value(&child);
        }
    }

    pub(super) fn compact_live_allocations(&mut self) -> usize {
        let live_slots = self
            .slots
            .iter()
            .enumerate()
            .filter(|(_, slot)| slot.object.is_some())
            .map(|(slot_index, slot)| (slot_index, slot.allocation, slot.line_count()))
            .collect::<Vec<_>>();
        self.blocks.clear();
        let mut evacuated = 0;
        for (slot_index, old_allocation, line_count) in live_slots {
            let allocation = self.allocate_lines(line_count);
            if allocation != old_allocation {
                evacuated += 1;
            }
            if let Some(slot) = self.slots.get_mut(slot_index) {
                slot.allocation = allocation;
            }
        }
        evacuated
    }
}
