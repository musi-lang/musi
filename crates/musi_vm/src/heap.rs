use std::ffi::c_void;
use std::mem::size_of;

use crate::effect::EffectHandler;
use crate::frame::CallFrame;
use crate::value::Value;

// ── HeapObject types ─────────────────────────────────────────────────────────

#[derive(Clone)]
pub struct Closure {
    pub method_idx: u16,
    pub upvalues: Vec<Value>,
}

#[derive(Clone)]
pub struct Continuation {
    pub frames: Vec<CallFrame>,
    pub resume_pc: usize,
    pub captured_handlers: Vec<EffectHandler>,
}

#[derive(Clone)]
pub struct VmArray {
    pub tag: Value,
    pub elements: Vec<Value>,
}

#[derive(Clone)]
pub struct VmSlice {
    pub source: usize,
    pub start: usize,
    pub end: usize,
}

#[derive(Clone)]
pub enum HeapObject {
    Closure(Closure),
    Continuation(Continuation),
    Array(VmArray),
    String(String),
    Slice(VmSlice),
    CPtr(*mut c_void),
    Cell(Value),
}

// ── Immix constants ──────────────────────────────────────────────────────────

const CELL_SIZE: usize = size_of::<HeapObject>();
const LINE_SIZE: usize = 128;
const CELLS_PER_LINE: usize = LINE_SIZE / CELL_SIZE;
const LINES_PER_BLOCK: usize = 256;
const BLOCK_LINES: usize = 255; // line 0 reserved for metadata
const BLOCK_CAPACITY: usize = BLOCK_LINES * CELLS_PER_LINE;
const INITIAL_GC_THRESHOLD: usize = 256;

const _: () = assert!(CELL_SIZE <= LINE_SIZE);

// ── Address encoding ─────────────────────────────────────────────────────────

const CELL_BITS: u32 = 16;
const LARGE_FLAG: usize = 1 << 47;

#[inline]
const fn pack_addr(block_idx: usize, cell_idx: usize) -> usize {
    (block_idx << CELL_BITS) | cell_idx
}

#[inline]
const fn unpack_addr(addr: usize) -> (usize, usize) {
    (addr >> CELL_BITS, addr & ((1 << CELL_BITS) - 1))
}

#[inline]
const fn is_large(addr: usize) -> bool {
    addr & LARGE_FLAG != 0
}

// ── Block types ──────────────────────────────────────────────────────────────

struct CellSlot {
    obj: Option<HeapObject>,
    mark: bool,
    pinned: bool,
}

#[derive(Clone, Copy, PartialEq, Eq)]
enum BlockState {
    Free,
    Recyclable,
    Full,
}

struct BlockMeta {
    line_marks: [u8; LINES_PER_BLOCK],
    state: BlockState,
    hole_count: u16,
}

struct Block {
    meta: BlockMeta,
    cells: Vec<CellSlot>,
}

impl Block {
    fn new() -> Self {
        let mut cells = Vec::with_capacity(BLOCK_CAPACITY);
        for _ in 0..BLOCK_CAPACITY {
            cells.push(CellSlot {
                obj: None,
                mark: false,
                pinned: false,
            });
        }
        Self {
            meta: BlockMeta {
                line_marks: [0; LINES_PER_BLOCK],
                state: BlockState::Free,
                hole_count: 0,
            },
            cells,
        }
    }

    fn write(&mut self, cell_idx: usize, obj: HeapObject) {
        self.cells[cell_idx] = CellSlot {
            obj: Some(obj),
            mark: false,
            pinned: false,
        };
    }

    fn get(&self, cell_idx: usize) -> Option<&HeapObject> {
        self.cells.get(cell_idx)?.obj.as_ref()
    }

    fn get_mut(&mut self, cell_idx: usize) -> Option<&mut HeapObject> {
        self.cells.get_mut(cell_idx)?.obj.as_mut()
    }
}

// ── Large object space ───────────────────────────────────────────────────────

struct LargeEntry {
    obj: HeapObject,
    mark: bool,
    pinned: bool,
}

struct LargeObjectSpace {
    objects: Vec<Option<LargeEntry>>,
    free: Vec<usize>,
}

impl LargeObjectSpace {
    const fn new() -> Self {
        Self {
            objects: Vec::new(),
            free: Vec::new(),
        }
    }

    #[cfg(test)]
    fn alloc(&mut self, obj: HeapObject) -> usize {
        let entry = LargeEntry {
            obj,
            mark: false,
            pinned: false,
        };
        if let Some(idx) = self.free.pop() {
            self.objects[idx] = Some(entry);
            idx | LARGE_FLAG
        } else {
            let idx = self.objects.len();
            self.objects.push(Some(entry));
            idx | LARGE_FLAG
        }
    }

    fn get(&self, addr: usize) -> Option<&HeapObject> {
        let idx = addr & !LARGE_FLAG;
        self.objects.get(idx)?.as_ref().map(|e| &e.obj)
    }

    fn get_mut(&mut self, addr: usize) -> Option<&mut HeapObject> {
        let idx = addr & !LARGE_FLAG;
        self.objects.get_mut(idx)?.as_mut().map(|e| &mut e.obj)
    }
}

// ── Bump state ───────────────────────────────────────────────────────────────

struct BumpState {
    block: usize,
    cursor: usize,
    limit: usize,
}

// ── GC state ─────────────────────────────────────────────────────────────────

struct GcState {
    remembered_set: Vec<usize>,
    allocation_count: usize,
    threshold: usize,
    minor_cycles: u32,
    major_interval: u32,
}

// ── Heap (Sticky Immix) ─────────────────────────────────────────────────────

pub struct Heap {
    blocks: Vec<Block>,
    free_blocks: Vec<usize>,
    recyclable_blocks: Vec<usize>,
    bump: BumpState,
    large: LargeObjectSpace,
    gc: GcState,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

impl Heap {
    #[must_use]
    pub fn new() -> Self {
        let mut heap = Self {
            blocks: Vec::new(),
            free_blocks: Vec::new(),
            recyclable_blocks: Vec::new(),
            bump: BumpState {
                block: 0,
                cursor: 0,
                limit: 0,
            },
            large: LargeObjectSpace::new(),
            gc: GcState {
                remembered_set: Vec::new(),
                allocation_count: 0,
                threshold: INITIAL_GC_THRESHOLD,
                minor_cycles: 0,
                major_interval: 8,
            },
        };
        // Allocate the first block
        let block_idx = heap.blocks.len();
        heap.blocks.push(Block::new());
        heap.bump.block = block_idx;
        heap.bump.cursor = 0;
        heap.bump.limit = BLOCK_CAPACITY;
        heap
    }

    // ── Allocation ───────────────────────────────────────────────────────────

    #[inline]
    fn alloc(&mut self, obj: HeapObject) -> usize {
        if self.bump.cursor < self.bump.limit {
            let cell_idx = self.bump.cursor;
            self.blocks[self.bump.block].write(cell_idx, obj);
            self.bump.cursor += 1;
            self.gc.allocation_count += 1;
            return pack_addr(self.bump.block, cell_idx);
        }
        self.alloc_slow(obj)
    }

    fn alloc_slow(&mut self, obj: HeapObject) -> usize {
        if let Some((start, end)) = self.find_next_hole(self.bump.block, self.bump.cursor) {
            self.bump.cursor = start;
            self.bump.limit = end;
            return self.alloc(obj);
        }
        self.acquire_next_block();
        self.alloc(obj)
    }

    fn acquire_next_block(&mut self) {
        if let Some(block_idx) = self.recyclable_blocks.pop() {
            self.bump.block = block_idx;
            if let Some((start, end)) = self.find_first_hole(block_idx) {
                self.bump.cursor = start;
                self.bump.limit = end;
            } else {
                // Recyclable block has no hole (shouldn't happen, but handle gracefully)
                self.acquire_next_block();
            }
        } else if let Some(block_idx) = self.free_blocks.pop() {
            self.bump.block = block_idx;
            self.bump.cursor = 0;
            self.bump.limit = BLOCK_CAPACITY;
        } else {
            let block_idx = self.blocks.len();
            self.blocks.push(Block::new());
            self.bump.block = block_idx;
            self.bump.cursor = 0;
            self.bump.limit = BLOCK_CAPACITY;
        }
    }

    // ── Hole finding ─────────────────────────────────────────────────────────

    fn find_next_hole(&self, block_idx: usize, after_cell: usize) -> Option<(usize, usize)> {
        let block = &self.blocks[block_idx];
        let after_line = if after_cell == 0 {
            0
        } else {
            after_cell / CELLS_PER_LINE
        };
        let mut line = after_line + 1;
        while line < LINES_PER_BLOCK {
            if block.meta.line_marks[line] == 0 {
                let start_line = line;
                let mut end_line = line + 1;
                while end_line < LINES_PER_BLOCK && block.meta.line_marks[end_line] == 0 {
                    end_line += 1;
                }
                // Skip first line of hole (conservative: prev object may spill)
                let start_cell = (start_line + 1) * CELLS_PER_LINE;
                let end_cell = end_line * CELLS_PER_LINE;
                if start_cell < end_cell && start_cell < BLOCK_CAPACITY {
                    let end_cell = end_cell.min(BLOCK_CAPACITY);
                    return Some((start_cell, end_cell));
                }
                line = end_line;
            } else {
                line += 1;
            }
        }
        None
    }

    fn find_first_hole(&self, block_idx: usize) -> Option<(usize, usize)> {
        // Start searching from line 1 (line 0 is reserved)
        let block = &self.blocks[block_idx];
        let mut line = 1;
        while line < LINES_PER_BLOCK {
            if block.meta.line_marks[line] == 0 {
                let start_line = line;
                let mut end_line = line + 1;
                while end_line < LINES_PER_BLOCK && block.meta.line_marks[end_line] == 0 {
                    end_line += 1;
                }
                // For first hole, skip first line only if start > 1
                // (conservative marking optimization)
                let start_cell = if start_line == 1 {
                    0 // First usable cells in block
                } else {
                    (start_line + 1) * CELLS_PER_LINE
                };
                let end_cell = (end_line * CELLS_PER_LINE).min(BLOCK_CAPACITY);
                if start_cell < end_cell {
                    return Some((start_cell, end_cell));
                }
                line = end_line;
            } else {
                line += 1;
            }
        }
        None
    }

    // ── Object access ────────────────────────────────────────────────────────

    #[must_use]
    pub fn get(&self, addr: usize) -> Option<&HeapObject> {
        if is_large(addr) {
            return self.large.get(addr);
        }
        let (block_idx, cell_idx) = unpack_addr(addr);
        self.blocks.get(block_idx)?.get(cell_idx)
    }

    #[must_use]
    pub fn get_mut(&mut self, addr: usize) -> Option<&mut HeapObject> {
        if is_large(addr) {
            return self.large.get_mut(addr);
        }
        let (block_idx, cell_idx) = unpack_addr(addr);
        self.blocks.get_mut(block_idx)?.get_mut(cell_idx)
    }

    // ── Cell access for GC ───────────────────────────────────────────────────

    fn get_cell(&self, addr: usize) -> Option<&CellSlot> {
        if is_large(addr) {
            return None; // Large objects handled separately
        }
        let (block_idx, cell_idx) = unpack_addr(addr);
        self.blocks.get(block_idx)?.cells.get(cell_idx)
    }

    fn get_cell_mut(&mut self, addr: usize) -> Option<&mut CellSlot> {
        if is_large(addr) {
            return None;
        }
        let (block_idx, cell_idx) = unpack_addr(addr);
        self.blocks.get_mut(block_idx)?.cells.get_mut(cell_idx)
    }

    // ── Allocation convenience methods ───────────────────────────────────────

    pub fn alloc_closure(&mut self, method_idx: u16, upvalues: Vec<Value>) -> usize {
        self.alloc(HeapObject::Closure(Closure {
            method_idx,
            upvalues,
        }))
    }

    pub fn alloc_continuation(
        &mut self,
        frames: Vec<CallFrame>,
        resume_pc: usize,
        captured_handlers: Vec<EffectHandler>,
    ) -> usize {
        self.alloc(HeapObject::Continuation(Continuation {
            frames,
            resume_pc,
            captured_handlers,
        }))
    }

    pub fn alloc_array(&mut self, tag: Value, elements: Vec<Value>) -> usize {
        self.alloc(HeapObject::Array(VmArray { tag, elements }))
    }

    pub fn alloc_string(&mut self, data: String) -> usize {
        self.alloc(HeapObject::String(data))
    }

    pub fn alloc_slice(&mut self, source: usize, start: usize, end: usize) -> usize {
        self.alloc(HeapObject::Slice(VmSlice { source, start, end }))
    }

    pub fn alloc_cptr(&mut self, ptr: *mut c_void) -> usize {
        self.alloc(HeapObject::CPtr(ptr))
    }

    pub fn alloc_cell(&mut self, value: Value) -> usize {
        self.alloc(HeapObject::Cell(value))
    }

    /// Allocate directly into the large object space (for objects exceeding
    /// `LARGE_OBJECT_THRESHOLD` or explicit large allocations).
    #[cfg(test)]
    pub(crate) fn alloc_large(&mut self, obj: HeapObject) -> usize {
        self.gc.allocation_count += 1;
        self.large.alloc(obj)
    }

    // ── GC: mark phase (major — full trace) ──────────────────────────────────

    pub fn mark_value(&mut self, value: Value) {
        if value.is_ptr() {
            self.mark_object(value.as_ptr_idx());
        }
    }

    pub fn mark_object(&mut self, addr: usize) {
        let mut worklist = vec![addr];
        while let Some(a) = worklist.pop() {
            if is_large(a) {
                let idx = a & !LARGE_FLAG;
                let Some(entry) = self.large.objects.get_mut(idx).and_then(|s| s.as_mut()) else {
                    continue;
                };
                if entry.mark {
                    continue;
                }
                entry.mark = true;
                Self::collect_children_into(&entry.obj, &mut worklist);
                continue;
            }

            let (block_idx, cell_idx) = unpack_addr(a);
            let Some(block) = self.blocks.get_mut(block_idx) else {
                continue;
            };
            let Some(cell) = block.cells.get_mut(cell_idx) else {
                continue;
            };
            let Some(obj) = &cell.obj else { continue };
            if cell.mark {
                continue;
            }
            cell.mark = true;
            let line = cell_idx / CELLS_PER_LINE + 1;
            if line < LINES_PER_BLOCK {
                block.meta.line_marks[line] = 1;
            }
            Self::collect_children_into(obj, &mut worklist);
        }
    }

    // ── GC: mark phase (minor — skip already-marked/old cells) ───────────────

    fn mark_value_minor(&mut self, value: Value) {
        if value.is_ptr() {
            self.mark_object_minor(value.as_ptr_idx());
        }
    }

    fn mark_object_minor(&mut self, addr: usize) {
        let mut worklist = vec![addr];
        while let Some(a) = worklist.pop() {
            if is_large(a) {
                let idx = a & !LARGE_FLAG;
                let Some(entry) = self.large.objects.get_mut(idx).and_then(|s| s.as_mut()) else {
                    continue;
                };
                if entry.mark {
                    continue;
                }
                entry.mark = true;
                Self::collect_children_into(&entry.obj, &mut worklist);
                continue;
            }

            let (block_idx, cell_idx) = unpack_addr(a);
            let Some(block) = self.blocks.get_mut(block_idx) else {
                continue;
            };
            let Some(cell) = block.cells.get_mut(cell_idx) else {
                continue;
            };
            let Some(obj) = &cell.obj else { continue };
            if cell.mark {
                continue;
            }
            cell.mark = true;
            let line = cell_idx / CELLS_PER_LINE + 1;
            if line < LINES_PER_BLOCK {
                block.meta.line_marks[line] = 1;
            }
            Self::collect_children_into(obj, &mut worklist);
        }
    }

    fn mark_children_minor(&mut self, addr: usize) {
        if is_large(addr) {
            let idx = addr & !LARGE_FLAG;
            let children = {
                let Some(entry) = self.large.objects.get(idx).and_then(|s| s.as_ref()) else {
                    return;
                };
                let mut children = Vec::new();
                Self::collect_children_into(&entry.obj, &mut children);
                children
            };
            for child in children {
                self.mark_object_minor(child);
            }
            return;
        }
        let children = {
            let (block_idx, cell_idx) = unpack_addr(addr);
            let Some(block) = self.blocks.get(block_idx) else {
                return;
            };
            let Some(cell) = block.cells.get(cell_idx) else {
                return;
            };
            let Some(obj) = &cell.obj else { return };
            let mut children = Vec::new();
            Self::collect_children_into(obj, &mut children);
            children
        };
        for child in children {
            self.mark_object_minor(child);
        }
    }

    fn collect_children_into(obj: &HeapObject, worklist: &mut Vec<usize>) {
        match obj {
            HeapObject::Closure(c) => {
                for v in &c.upvalues {
                    if v.is_ptr() {
                        worklist.push(v.as_ptr_idx());
                    }
                }
            }
            HeapObject::Array(a) => {
                if a.tag.is_ptr() {
                    worklist.push(a.tag.as_ptr_idx());
                }
                for v in &a.elements {
                    if v.is_ptr() {
                        worklist.push(v.as_ptr_idx());
                    }
                }
            }
            HeapObject::Continuation(c) => {
                for frame in &c.frames {
                    for v in frame.locals_iter() {
                        if v.is_ptr() {
                            worklist.push(v.as_ptr_idx());
                        }
                    }
                    for v in frame.stack_iter() {
                        if v.is_ptr() {
                            worklist.push(v.as_ptr_idx());
                        }
                    }
                }
            }
            HeapObject::Slice(s) => {
                worklist.push(s.source);
            }
            HeapObject::Cell(v) => {
                if v.is_ptr() {
                    worklist.push(v.as_ptr_idx());
                }
            }
            HeapObject::String(_) | HeapObject::CPtr(_) => {}
        }
    }

    // ── GC: collection entry points ──────────────────────────────────────────

    pub fn collect_minor(&mut self, roots: &[Value]) {
        // Mark from roots (skip already-marked/old cells)
        for &val in roots {
            self.mark_value_minor(val);
        }
        // Mark from remembered set
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
                cell.mark = false;
            }
        }
        for entry in self.large.objects.iter_mut().flatten() {
            entry.mark = false;
        }
    }

    // ── GC: sweep ────────────────────────────────────────────────────────────

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
                        if cell_idx < block.cells.len() && !block.cells[cell_idx].pinned {
                            block.cells[cell_idx].obj = None;
                            block.cells[cell_idx].mark = false;
                        }
                    }
                } else {
                    all_clear = false;
                    // Line marks persist (sticky) — only clear_all_marks resets them
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

        // Sweep large objects
        for (idx, slot) in self.large.objects.iter_mut().enumerate() {
            if let Some(entry) = slot {
                if !entry.mark && !entry.pinned {
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
            // Don't add the current bump block to free/recyclable lists
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

    // ── GC: threshold ────────────────────────────────────────────────────────

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

    // ── GC: pinning ──────────────────────────────────────────────────────────

    pub fn pin(&mut self, addr: usize) {
        if is_large(addr) {
            let idx = addr & !LARGE_FLAG;
            if let Some(entry) = self.large.objects.get_mut(idx).and_then(|s| s.as_mut()) {
                entry.pinned = true;
            }
        } else if let Some(cell) = self.get_cell_mut(addr) {
            cell.pinned = true;
        }
    }

    pub fn unpin(&mut self, addr: usize) {
        if is_large(addr) {
            let idx = addr & !LARGE_FLAG;
            if let Some(entry) = self.large.objects.get_mut(idx).and_then(|s| s.as_mut()) {
                entry.pinned = false;
            }
        } else if let Some(cell) = self.get_cell_mut(addr) {
            cell.pinned = false;
        }
    }

    // ── Write barrier support ────────────────────────────────────────────────

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
                .is_some_and(|e| e.mark);
        }
        self.get_cell(addr).is_some_and(|c| c.mark)
    }

    // ── Live count ───────────────────────────────────────────────────────────

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

#[cfg(test)]
mod tests;
