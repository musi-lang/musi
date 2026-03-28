use std::mem::size_of;

use super::HeapObject;

pub(super) const CELL_SIZE: usize = size_of::<HeapObject>();
pub(super) const LINE_SIZE: usize = 128;
pub(super) const CELLS_PER_LINE: usize = LINE_SIZE / CELL_SIZE;
pub(super) const LINES_PER_BLOCK: usize = 256;
pub(super) const BLOCK_LINES: usize = 255;
pub(super) const BLOCK_CAPACITY: usize = BLOCK_LINES * CELLS_PER_LINE;
pub(super) const INITIAL_GC_THRESHOLD: usize = 256;

const _: () = assert!(CELL_SIZE <= LINE_SIZE);

pub(super) const CELL_BITS: u32 = 16;
pub(super) const LARGE_FLAG: usize = 1 << 47;

#[inline]
pub(super) const fn pack_addr(block_idx: usize, cell_idx: usize) -> usize {
    (block_idx << CELL_BITS) | cell_idx
}

#[inline]
pub(super) const fn unpack_addr(addr: usize) -> (usize, usize) {
    (addr >> CELL_BITS, addr & ((1 << CELL_BITS) - 1))
}

#[inline]
pub(super) const fn is_large(addr: usize) -> bool {
    addr & LARGE_FLAG != 0
}

pub(super) const MARK_FLAG: u8 = 1;
pub(super) const PIN_FLAG: u8 = 2;

pub(super) struct CellSlot {
    pub(super) obj: Option<HeapObject>,
    pub(super) flags: u8,
}

impl CellSlot {
    pub(super) const fn is_marked(&self) -> bool {
        self.flags & MARK_FLAG != 0
    }

    pub(super) const fn set_marked(&mut self, v: bool) {
        if v {
            self.flags |= MARK_FLAG;
        } else {
            self.flags &= !MARK_FLAG;
        }
    }

    pub(super) const fn is_pinned(&self) -> bool {
        self.flags & PIN_FLAG != 0
    }

    pub(super) const fn set_pinned(&mut self, v: bool) {
        if v {
            self.flags |= PIN_FLAG;
        } else {
            self.flags &= !PIN_FLAG;
        }
    }
}

#[derive(Clone, Copy, PartialEq, Eq)]
pub(super) enum BlockState {
    Free,
    Recyclable,
    Full,
}

pub(super) struct BlockMeta {
    pub(super) line_marks: [u8; LINES_PER_BLOCK],
    pub(super) state: BlockState,
    pub(super) hole_count: u16,
}

pub(super) struct Block {
    pub(super) meta: BlockMeta,
    pub(super) cells: Vec<CellSlot>,
}

impl Block {
    pub(super) fn new() -> Self {
        let mut cells = Vec::with_capacity(BLOCK_CAPACITY);
        for _ in 0..BLOCK_CAPACITY {
            cells.push(CellSlot {
                obj: None,
                flags: 0,
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

    pub(super) fn write(&mut self, cell_idx: usize, obj: HeapObject) {
        self.cells[cell_idx] = CellSlot {
            obj: Some(obj),
            flags: 0,
        };
    }

    pub(super) fn get(&self, cell_idx: usize) -> Option<&HeapObject> {
        self.cells.get(cell_idx)?.obj.as_ref()
    }

    pub(super) fn get_mut(&mut self, cell_idx: usize) -> Option<&mut HeapObject> {
        self.cells.get_mut(cell_idx)?.obj.as_mut()
    }
}

pub(super) struct LargeEntry {
    pub(super) obj: HeapObject,
    pub(super) flags: u8,
}

impl LargeEntry {
    pub(super) const fn is_marked(&self) -> bool {
        self.flags & MARK_FLAG != 0
    }

    pub(super) const fn set_marked(&mut self, v: bool) {
        if v {
            self.flags |= MARK_FLAG;
        } else {
            self.flags &= !MARK_FLAG;
        }
    }

    pub(super) const fn is_pinned(&self) -> bool {
        self.flags & PIN_FLAG != 0
    }

    pub(super) const fn set_pinned(&mut self, v: bool) {
        if v {
            self.flags |= PIN_FLAG;
        } else {
            self.flags &= !PIN_FLAG;
        }
    }

    pub(super) const fn obj_ref(&self) -> &HeapObject {
        &self.obj
    }

    pub(super) const fn obj_mut(&mut self) -> &mut HeapObject {
        &mut self.obj
    }
}

pub(super) struct LargeObjectSpace {
    pub(super) objects: Vec<Option<LargeEntry>>,
    pub(super) free: Vec<usize>,
}

impl LargeObjectSpace {
    pub(super) const fn new() -> Self {
        Self {
            objects: Vec::new(),
            free: Vec::new(),
        }
    }

    #[cfg(test)]
    pub(super) fn alloc(&mut self, obj: HeapObject) -> usize {
        let entry = LargeEntry { obj, flags: 0 };
        if let Some(idx) = self.free.pop() {
            self.objects[idx] = Some(entry);
            idx | LARGE_FLAG
        } else {
            let idx = self.objects.len();
            self.objects.push(Some(entry));
            idx | LARGE_FLAG
        }
    }

    pub(super) fn get(&self, addr: usize) -> Option<&HeapObject> {
        let idx = addr & !LARGE_FLAG;
        self.objects.get(idx)?.as_ref().map(|e| &e.obj)
    }

    pub(super) fn get_mut(&mut self, addr: usize) -> Option<&mut HeapObject> {
        let idx = addr & !LARGE_FLAG;
        self.objects.get_mut(idx)?.as_mut().map(|e| &mut e.obj)
    }
}

pub(super) struct BumpState {
    pub(super) block: usize,
    pub(super) cursor: usize,
    pub(super) limit: usize,
}

pub(super) struct GcState {
    pub(super) remembered_set: Vec<usize>,
    pub(super) worklist: Vec<usize>,
    pub(super) allocation_count: usize,
    pub(super) threshold: usize,
    pub(super) minor_cycles: u32,
    pub(super) major_interval: u32,
}
