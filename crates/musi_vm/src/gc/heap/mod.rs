use std::cell::Cell;
use std::marker::PhantomData;
use std::sync::atomic::{AtomicU64, Ordering};

use crate::value::IsolateId;

use super::space::{HeapSlot, IMMIX_CARDS_PER_BLOCK, ImmixBlock};

mod access;
mod alloc;
mod barrier;
mod fast_seq;
mod immix;
mod sequence;
mod util;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapOptions {
    pub max_object_bytes: Option<usize>,
}

#[derive(Debug, Default)]
pub struct RuntimeHeap {
    pub(super) isolate: IsolateId,
    pub(super) slots: Vec<HeapSlot>,
    pub(super) free_slots: Vec<usize>,
    pub(super) blocks: Vec<ImmixBlock>,
    pub(super) current_young_block: Option<usize>,
    pub(super) current_mature_block: Option<usize>,
    pub(super) mature_card_table: Vec<[bool; IMMIX_CARDS_PER_BLOCK]>,
    pub(super) remembered_large_slots: Vec<usize>,
    pub(super) allocated_bytes: usize,
    pub(super) young_allocated_bytes: usize,
    pub(super) seq8_fast_slots: Vec<usize>,
    pub(super) seq8_fast_cursor: usize,
    pub(super) seq8_fast_generation: u32,
    pub(super) mark_epoch: u32,
    single_thread_marker: PhantomData<Cell<()>>,
}

pub(super) const YOUNG_TARGET_BYTES: usize = 64 * 1024;
pub(super) const LARGE_PROMOTE_BYTES: usize = 4096;
pub(super) const PROMOTE_SURVIVE_THRESHOLD: u8 = 2;

#[derive(Debug, Clone, Copy)]
pub struct SequenceGuard {
    pub slot: usize,
    pub generation: u32,
    pub layout_version: u32,
}

#[derive(Debug, Clone, Copy)]
pub struct Seq2x2ArgGuard {
    pub grid: SequenceGuard,
    pub row0: SequenceGuard,
    pub row1: SequenceGuard,
}

static NEXT_ISOLATE_ID: AtomicU64 = AtomicU64::new(1);

impl Default for IsolateId {
    fn default() -> Self {
        Self::new(NEXT_ISOLATE_ID.fetch_add(1, Ordering::Relaxed))
    }
}

impl RuntimeHeap {
    #[must_use]
    pub fn new() -> Self {
        Self {
            isolate: IsolateId::default(),
            slots: Vec::new(),
            free_slots: Vec::new(),
            blocks: Vec::new(),
            current_young_block: None,
            current_mature_block: None,
            mature_card_table: Vec::new(),
            remembered_large_slots: Vec::new(),
            allocated_bytes: 0,
            young_allocated_bytes: 0,
            seq8_fast_slots: Vec::new(),
            seq8_fast_cursor: 0,
            seq8_fast_generation: 0,
            mark_epoch: 0,
            single_thread_marker: PhantomData,
        }
    }

    #[must_use]
    pub(crate) const fn isolate(&self) -> IsolateId {
        self.isolate
    }

    #[must_use]
    pub(crate) const fn allocated_bytes(&self) -> usize {
        self.allocated_bytes
    }

    #[must_use]
    pub(crate) const fn should_collect_young(&self) -> bool {
        self.young_allocated_bytes >= YOUNG_TARGET_BYTES
    }
}

/// Runtime heap collection counters.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct HeapCollectionStats {
    /// Estimated live heap bytes before collection.
    pub before_bytes: usize,
    /// Estimated live heap bytes after collection.
    pub after_bytes: usize,
    /// Tracked heap objects before collection.
    pub before_objects: usize,
    /// Tracked heap objects after collection.
    pub after_objects: usize,
    /// Estimated bytes reclaimed by collection.
    pub reclaimed_bytes: usize,
    /// Heap objects reclaimed by collection.
    pub reclaimed_objects: usize,
    /// Immix blocks with no live lines after collection.
    pub free_blocks: usize,
    /// Live objects assigned new Immix line ranges during compaction.
    pub evacuated_objects: usize,
}
