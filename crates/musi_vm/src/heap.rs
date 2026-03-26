use std::ffi::c_void;

use crate::effect::EffectHandler;
use crate::frame::CallFrame;
use crate::value::Value;

pub struct Closure {
    pub method_idx: u16,
    pub upvalues: Vec<Value>,
}

pub struct Continuation {
    pub frames: Vec<CallFrame>,
    pub resume_pc: usize,
    pub captured_handlers: Vec<EffectHandler>,
}

pub struct VmArray {
    pub tag: Value,
    pub elements: Vec<Value>,
}

pub struct VmSlice {
    pub source: usize,
    pub start: usize,
    pub end: usize,
}

pub enum HeapObject {
    Closure(Closure),
    Continuation(Continuation),
    Array(VmArray),
    String(String),
    Slice(VmSlice),
    CPtr(*mut c_void),
    /// Single-element mutable box for captured mutable variables.
    Cell(Value),
}

struct HeapEntry {
    obj: HeapObject,
    marked: bool,
    pinned: bool,
}

pub struct Heap {
    objects: Vec<Option<HeapEntry>>,
    free_list: Vec<usize>,
    allocation_count: usize,
    gc_threshold: usize,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

const INITIAL_GC_THRESHOLD: usize = 256;

impl Heap {
    #[must_use]
    pub const fn new() -> Self {
        Self {
            objects: Vec::new(),
            free_list: Vec::new(),
            allocation_count: 0,
            gc_threshold: INITIAL_GC_THRESHOLD,
        }
    }

    #[must_use]
    pub fn get(&self, idx: usize) -> Option<&HeapObject> {
        self.objects.get(idx)?.as_ref().map(|e| &e.obj)
    }

    #[must_use]
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut HeapObject> {
        self.objects.get_mut(idx)?.as_mut().map(|e| &mut e.obj)
    }

    fn alloc(&mut self, obj: HeapObject) -> usize {
        self.allocation_count = self.allocation_count.wrapping_add(1);
        let entry = HeapEntry {
            obj,
            marked: false,
            pinned: false,
        };
        if let Some(idx) = self.free_list.pop() {
            self.objects[idx] = Some(entry);
            idx
        } else {
            let idx = self.objects.len();
            self.objects.push(Some(entry));
            idx
        }
    }

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

    // ── GC: mark phase ───────────────────────────────────────────────────────

    pub fn mark_value(&mut self, value: Value) {
        if value.is_ptr() {
            self.mark_object(value.as_ptr_idx());
        }
    }

    pub fn mark_object(&mut self, idx: usize) {
        let mut worklist = vec![idx];
        while let Some(i) = worklist.pop() {
            let Some(entry) = self.objects.get_mut(i).and_then(|slot| slot.as_mut()) else {
                continue;
            };
            if entry.marked {
                continue;
            }
            entry.marked = true;
            match &entry.obj {
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
    }

    // ── GC: sweep phase ──────────────────────────────────────────────────────

    pub fn sweep(&mut self) {
        for (idx, slot) in self.objects.iter_mut().enumerate() {
            if let Some(entry) = slot {
                if entry.marked {
                    entry.marked = false;
                } else if !entry.pinned {
                    *slot = None;
                    self.free_list.push(idx);
                }
            }
        }
    }

    // ── GC: threshold ────────────────────────────────────────────────────────

    #[must_use]
    pub const fn should_collect(&self) -> bool {
        self.allocation_count >= self.gc_threshold
    }

    pub fn reset_threshold(&mut self) {
        let live = self.objects.iter().filter(|s| s.is_some()).count();
        self.gc_threshold = (live * 2).max(INITIAL_GC_THRESHOLD);
        self.allocation_count = 0;
    }

    // ── GC: pinning ──────────────────────────────────────────────────────────

    pub fn pin(&mut self, idx: usize) {
        if let Some(entry) = self.objects.get_mut(idx).and_then(|s| s.as_mut()) {
            entry.pinned = true;
        }
    }

    pub fn unpin(&mut self, idx: usize) {
        if let Some(entry) = self.objects.get_mut(idx).and_then(|s| s.as_mut()) {
            entry.pinned = false;
        }
    }

    /// Number of live (non-freed) heap slots.
    #[must_use]
    pub fn live_count(&self) -> usize {
        self.objects.iter().filter(|s| s.is_some()).count()
    }
}

#[cfg(test)]
mod tests;
