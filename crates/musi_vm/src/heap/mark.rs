use super::layout::{is_large, unpack_addr, CELLS_PER_LINE, LARGE_FLAG, LINES_PER_BLOCK};
use super::{Heap, HeapObject};
use crate::value::Value;

impl Heap {
    pub fn mark_value(&mut self, value: Value) {
        if value.is_ptr() {
            self.mark_object(value.as_ptr_idx());
        }
    }

    pub fn mark_object(&mut self, addr: usize) {
        self.gc.worklist.clear();
        self.gc.worklist.push(addr);
        while let Some(a) = self.gc.worklist.pop() {
            if is_large(a) {
                let idx = a & !LARGE_FLAG;
                let Some(entry) = self.large.objects.get_mut(idx).and_then(|s| s.as_mut()) else {
                    continue;
                };
                if entry.is_marked() {
                    continue;
                }
                entry.set_marked(true);
                Self::collect_children_into(&entry.obj, &mut self.gc.worklist);
                continue;
            }

            let (block_idx, cell_idx) = unpack_addr(a);
            let Some(block) = self.blocks.get_mut(block_idx) else {
                continue;
            };
            let Some(cell) = block.cells.get_mut(cell_idx) else {
                continue;
            };
            if cell.obj.is_none() || cell.is_marked() {
                continue;
            }
            cell.set_marked(true);
            let line = cell_idx / CELLS_PER_LINE + 1;
            if line < LINES_PER_BLOCK {
                block.meta.line_marks[line] = 1;
            }
            let obj = unsafe { cell.obj.as_ref().unwrap_unchecked() };
            Self::collect_children_into(obj, &mut self.gc.worklist);
        }
    }

    pub(super) fn mark_value_minor(&mut self, value: Value) {
        if value.is_ptr() {
            self.mark_object_minor(value.as_ptr_idx());
        }
    }

    pub(super) fn mark_object_minor(&mut self, addr: usize) {
        self.gc.worklist.clear();
        self.gc.worklist.push(addr);
        while let Some(a) = self.gc.worklist.pop() {
            if is_large(a) {
                let idx = a & !LARGE_FLAG;
                let Some(entry) = self.large.objects.get_mut(idx).and_then(|s| s.as_mut()) else {
                    continue;
                };
                if entry.is_marked() {
                    continue;
                }
                entry.set_marked(true);
                Self::collect_children_into(&entry.obj, &mut self.gc.worklist);
                continue;
            }

            let (block_idx, cell_idx) = unpack_addr(a);
            let Some(block) = self.blocks.get_mut(block_idx) else {
                continue;
            };
            let Some(cell) = block.cells.get_mut(cell_idx) else {
                continue;
            };
            if cell.obj.is_none() || cell.is_marked() {
                continue;
            }
            cell.set_marked(true);
            let line = cell_idx / CELLS_PER_LINE + 1;
            if line < LINES_PER_BLOCK {
                block.meta.line_marks[line] = 1;
            }
            let obj = unsafe { cell.obj.as_ref().unwrap_unchecked() };
            Self::collect_children_into(obj, &mut self.gc.worklist);
        }
    }

    pub(super) fn mark_children_minor(&mut self, addr: usize) {
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
            HeapObject::Cell(cell) => {
                if cell.value.is_ptr() {
                    worklist.push(cell.value.as_ptr_idx());
                }
            }
            HeapObject::String(_) | HeapObject::CPtr(_) => {}
        }
    }
}
