use std::ffi::c_void;

use music_il::format;

use super::layout::{
    is_large, pack_addr, unpack_addr, Block, BumpState, CellSlot, GcState, LargeObjectSpace,
    BLOCK_CAPACITY, CELLS_PER_LINE, INITIAL_GC_THRESHOLD, LARGE_FLAG, LINES_PER_BLOCK,
};
use super::{Closure, Continuation, Heap, HeapObject, VmArray, VmCPtr, VmCell, VmSlice, VmString};
use crate::effect::EffectHandler;
use crate::frame::CallFrame;
use crate::value::Value;

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
                worklist: Vec::new(),
                allocation_count: 0,
                threshold: INITIAL_GC_THRESHOLD,
                minor_cycles: 0,
                major_interval: 8,
            },
        };
        let block_idx = heap.blocks.len();
        heap.blocks.push(Block::new());
        heap.bump.block = block_idx;
        heap.bump.cursor = 0;
        heap.bump.limit = BLOCK_CAPACITY;
        heap
    }

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
        let block = &self.blocks[block_idx];
        let mut line = 1;
        while line < LINES_PER_BLOCK {
            if block.meta.line_marks[line] == 0 {
                let start_line = line;
                let mut end_line = line + 1;
                while end_line < LINES_PER_BLOCK && block.meta.line_marks[end_line] == 0 {
                    end_line += 1;
                }
                let start_cell = if start_line == 1 {
                    0
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

    #[must_use]
    pub unsafe fn get_unchecked(&self, addr: usize) -> &HeapObject {
        if is_large(addr) {
            let idx = addr & !LARGE_FLAG;
            let slot = unsafe { self.large.objects.get_unchecked(idx) };
            let entry = unsafe { slot.as_ref().unwrap_unchecked() };
            return entry.obj_ref();
        }
        let (block_idx, cell_idx) = unpack_addr(addr);
        let block = unsafe { self.blocks.get_unchecked(block_idx) };
        let cell = unsafe { block.cells.get_unchecked(cell_idx) };
        unsafe { cell.obj.as_ref().unwrap_unchecked() }
    }

    #[must_use]
    pub unsafe fn get_mut_unchecked(&mut self, addr: usize) -> &mut HeapObject {
        if is_large(addr) {
            let idx = addr & !LARGE_FLAG;
            let slot = unsafe { self.large.objects.get_unchecked_mut(idx) };
            let entry = unsafe { slot.as_mut().unwrap_unchecked() };
            return entry.obj_mut();
        }
        let (block_idx, cell_idx) = unpack_addr(addr);
        let block = unsafe { self.blocks.get_unchecked_mut(block_idx) };
        let cell = unsafe { block.cells.get_unchecked_mut(cell_idx) };
        unsafe { cell.obj.as_mut().unwrap_unchecked() }
    }

    pub(super) fn get_cell(&self, addr: usize) -> Option<&CellSlot> {
        if is_large(addr) {
            return None;
        }
        let (block_idx, cell_idx) = unpack_addr(addr);
        self.blocks.get(block_idx)?.cells.get(cell_idx)
    }

    pub(super) fn get_cell_mut(&mut self, addr: usize) -> Option<&mut CellSlot> {
        if is_large(addr) {
            return None;
        }
        let (block_idx, cell_idx) = unpack_addr(addr);
        self.blocks.get_mut(block_idx)?.cells.get_mut(cell_idx)
    }

    pub fn alloc_closure(&mut self, method_idx: u16, upvalues: Vec<Value>) -> usize {
        self.alloc_closure_t(format::INTERNAL_TYPE_CLOSURE, method_idx, upvalues)
    }

    pub fn alloc_closure_t(&mut self, type_id: u16, method_idx: u16, upvalues: Vec<Value>) -> usize {
        self.alloc(HeapObject::Closure(Closure {
            type_id,
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
        self.alloc_continuation_t(
            format::INTERNAL_TYPE_CONTINUATION,
            frames,
            resume_pc,
            captured_handlers,
        )
    }

    pub fn alloc_continuation_t(
        &mut self,
        type_id: u16,
        frames: Vec<CallFrame>,
        resume_pc: usize,
        captured_handlers: Vec<EffectHandler>,
    ) -> usize {
        self.alloc(HeapObject::Continuation(Continuation {
            type_id,
            frames,
            resume_pc,
            captured_handlers,
        }))
    }

    pub fn alloc_array(&mut self, tag: Value, elements: Vec<Value>) -> usize {
        self.alloc_array_t(format::INTERNAL_TYPE_ARRAY, tag, elements)
    }

    pub fn alloc_array_t(&mut self, type_id: u16, tag: Value, elements: Vec<Value>) -> usize {
        self.alloc(HeapObject::Array(VmArray {
            type_id,
            tag,
            elements,
        }))
    }

    pub fn alloc_string(&mut self, data: String) -> usize {
        self.alloc_string_t(format::BUILTIN_TYPE_STRING, data)
    }

    pub fn alloc_string_t(&mut self, type_id: u16, data: String) -> usize {
        self.alloc(HeapObject::String(VmString { type_id, data }))
    }

    pub fn alloc_slice(&mut self, source: usize, start: usize, end: usize) -> usize {
        self.alloc_slice_t(format::INTERNAL_TYPE_SLICE, source, start, end)
    }

    pub fn alloc_slice_t(
        &mut self,
        type_id: u16,
        source: usize,
        start: usize,
        end: usize,
    ) -> usize {
        self.alloc(HeapObject::Slice(VmSlice {
            type_id,
            source,
            start,
            end,
        }))
    }

    pub fn alloc_cptr(&mut self, ptr: *mut c_void) -> usize {
        self.alloc_cptr_t(format::INTERNAL_TYPE_CPTR, ptr)
    }

    pub fn alloc_cptr_t(&mut self, type_id: u16, ptr: *mut c_void) -> usize {
        self.alloc(HeapObject::CPtr(VmCPtr { type_id, ptr }))
    }

    pub fn alloc_cell(&mut self, value: Value) -> usize {
        self.alloc_cell_t(format::INTERNAL_TYPE_CELL, value)
    }

    pub fn alloc_cell_t(&mut self, type_id: u16, value: Value) -> usize {
        self.alloc(HeapObject::Cell(VmCell { type_id, value }))
    }

    #[cfg(test)]
    pub(crate) fn alloc_large(&mut self, obj: HeapObject) -> usize {
        self.gc.allocation_count += 1;
        self.large.alloc(obj)
    }
}
