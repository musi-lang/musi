use std::ffi::c_void;

use crate::effect::EffectHandler;
use crate::frame::CallFrame;
use crate::value::Value;

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
    /// Single-element mutable box for captured mutable variables.
    Cell(Value),
    /// Temporary forwarding pointer installed during minor GC promotion.
    Forwarded(usize),
}

struct HeapEntry {
    obj: HeapObject,
    marked: bool,
    pinned: bool,
}

// ── Nursery ──────────────────────────────────────────────────────────────────

const NURSERY_CAPACITY: usize = 256;
const NURSERY_FLAG: usize = 1 << 31;

#[must_use]
pub const fn is_nursery_idx(idx: usize) -> bool {
    idx & NURSERY_FLAG != 0
}

const fn nursery_local(idx: usize) -> usize {
    idx & !NURSERY_FLAG
}

const fn nursery_global(local: usize) -> usize {
    local | NURSERY_FLAG
}

struct Nursery {
    objects: Vec<Option<HeapEntry>>,
    next: usize,
    capacity: usize,
}

impl Nursery {
    fn new(capacity: usize) -> Self {
        let mut objects = Vec::with_capacity(capacity);
        objects.resize_with(capacity, || None);
        Self {
            objects,
            next: 0,
            capacity,
        }
    }

    const fn is_full(&self) -> bool {
        self.next >= self.capacity
    }

    fn clear(&mut self) {
        for slot in &mut self.objects {
            *slot = None;
        }
        self.next = 0;
    }
}

// ── Heap ─────────────────────────────────────────────────────────────────────

pub struct Heap {
    objects: Vec<Option<HeapEntry>>,
    free_list: Vec<usize>,
    allocation_count: usize,
    gc_threshold: usize,
    nursery: Nursery,
    remembered_set: Vec<usize>,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

const INITIAL_GC_THRESHOLD: usize = 256;

impl Heap {
    #[must_use]
    pub fn new() -> Self {
        Self {
            objects: Vec::new(),
            free_list: Vec::new(),
            allocation_count: 0,
            gc_threshold: INITIAL_GC_THRESHOLD,
            nursery: Nursery::new(NURSERY_CAPACITY),
            remembered_set: Vec::new(),
        }
    }

    #[cfg(test)]
    pub(crate) fn new_with_nursery_capacity(capacity: usize) -> Self {
        Self {
            objects: Vec::new(),
            free_list: Vec::new(),
            allocation_count: 0,
            gc_threshold: INITIAL_GC_THRESHOLD,
            nursery: Nursery::new(capacity),
            remembered_set: Vec::new(),
        }
    }

    #[must_use]
    pub fn get(&self, idx: usize) -> Option<&HeapObject> {
        if is_nursery_idx(idx) {
            let local = nursery_local(idx);
            self.nursery.objects.get(local)?.as_ref().map(|e| &e.obj)
        } else {
            self.objects.get(idx)?.as_ref().map(|e| &e.obj)
        }
    }

    #[must_use]
    pub fn get_mut(&mut self, idx: usize) -> Option<&mut HeapObject> {
        if is_nursery_idx(idx) {
            let local = nursery_local(idx);
            self.nursery
                .objects
                .get_mut(local)?
                .as_mut()
                .map(|e| &mut e.obj)
        } else {
            self.objects.get_mut(idx)?.as_mut().map(|e| &mut e.obj)
        }
    }

    fn alloc(&mut self, obj: HeapObject) -> usize {
        self.allocation_count = self.allocation_count.wrapping_add(1);
        if !self.nursery.is_full() {
            let local = self.nursery.next;
            self.nursery.next = self.nursery.next.wrapping_add(1);
            self.nursery.objects[local] = Some(HeapEntry {
                obj,
                marked: false,
                pinned: false,
            });
            return nursery_global(local);
        }
        self.alloc_mature(obj)
    }

    fn alloc_mature(&mut self, obj: HeapObject) -> usize {
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
            if is_nursery_idx(i) {
                continue;
            }
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
                HeapObject::String(_) | HeapObject::CPtr(_) | HeapObject::Forwarded(_) => {}
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
        if is_nursery_idx(idx) {
            let local = nursery_local(idx);
            if let Some(entry) = self.nursery.objects.get_mut(local).and_then(|s| s.as_mut()) {
                entry.pinned = true;
            }
        } else if let Some(entry) = self.objects.get_mut(idx).and_then(|s| s.as_mut()) {
            entry.pinned = true;
        }
    }

    pub fn unpin(&mut self, idx: usize) {
        if is_nursery_idx(idx) {
            let local = nursery_local(idx);
            if let Some(entry) = self.nursery.objects.get_mut(local).and_then(|s| s.as_mut()) {
                entry.pinned = false;
            }
        } else if let Some(entry) = self.objects.get_mut(idx).and_then(|s| s.as_mut()) {
            entry.pinned = false;
        }
    }

    /// Number of live (non-freed) heap slots (mature + nursery).
    #[must_use]
    pub fn live_count(&self) -> usize {
        let mature = self.objects.iter().filter(|s| s.is_some()).count();
        let nursery = self.nursery.objects.iter().filter(|s| s.is_some()).count();
        mature + nursery
    }

    // ── Nursery / generational GC ────────────────────────────────────────────

    #[must_use]
    pub const fn nursery_full(&self) -> bool {
        self.nursery.is_full()
    }

    pub fn remember(&mut self, mature_idx: usize) {
        if !self.remembered_set.contains(&mature_idx) {
            self.remembered_set.push(mature_idx);
        }
    }

    /// Fix a single value: if it points into the nursery and the nursery slot
    /// holds a `Forwarded(new_idx)`, return the updated pointer. Otherwise
    /// return the value unchanged.
    #[must_use]
    pub fn fix_value(&self, val: Value) -> Value {
        if !val.is_ptr() {
            return val;
        }
        let idx = val.as_ptr_idx();
        if !is_nursery_idx(idx) {
            return val;
        }
        let local = nursery_local(idx);
        if let Some(Some(entry)) = self.nursery.objects.get(local) {
            if let HeapObject::Forwarded(new_idx) = entry.obj {
                return Value::from_ptr(new_idx);
            }
        }
        val
    }

    /// Run a minor (nursery) collection. Roots are provided by the VM.
    ///
    /// 1. Mark nursery objects reachable from roots and remembered set.
    /// 2. Promote live nursery objects to the mature heap.
    /// 3. Install forwarding pointers.
    /// 4. Fix references in mature objects.
    /// 5. Clear nursery and remembered set.
    pub fn minor_collect(&mut self, roots: &[Value]) {
        // Phase 1: mark nursery objects reachable from roots
        let mut nursery_marked = vec![false; self.nursery.capacity];

        // Scan roots for nursery pointers
        let mut worklist: Vec<usize> = Vec::new();
        for &val in roots {
            if val.is_ptr() {
                let idx = val.as_ptr_idx();
                if is_nursery_idx(idx) {
                    let local = nursery_local(idx);
                    if local < self.nursery.capacity && !nursery_marked[local] {
                        nursery_marked[local] = true;
                        worklist.push(local);
                    }
                }
            }
        }

        // Scan remembered set: mature objects that may point into nursery
        for &mature_idx in &self.remembered_set.clone() {
            if let Some(entry) = self.objects.get(mature_idx).and_then(|s| s.as_ref()) {
                Self::scan_obj_for_nursery_ptrs(
                    &entry.obj,
                    &mut nursery_marked,
                    &mut worklist,
                    self.nursery.capacity,
                );
            }
        }

        // Transitively mark nursery-to-nursery references
        while let Some(local) = worklist.pop() {
            if let Some(Some(entry)) = self.nursery.objects.get(local) {
                let ptrs = Self::collect_ptr_indices(&entry.obj);
                for idx in ptrs {
                    if is_nursery_idx(idx) {
                        let l = nursery_local(idx);
                        if l < self.nursery.capacity && !nursery_marked[l] {
                            nursery_marked[l] = true;
                            worklist.push(l);
                        }
                    }
                }
            }
        }

        // Also mark pinned nursery objects
        for (local, marked) in nursery_marked
            .iter_mut()
            .enumerate()
            .take(self.nursery.capacity)
        {
            if let Some(entry) = &self.nursery.objects[local] {
                if entry.pinned && !*marked {
                    *marked = true;
                }
            }
        }

        // Phase 2: promote marked nursery objects to mature heap
        for (local, marked) in nursery_marked
            .iter()
            .enumerate()
            .take(self.nursery.capacity)
        {
            if !marked {
                continue;
            }
            let obj = match &self.nursery.objects[local] {
                Some(entry) => entry.obj.clone(),
                None => continue,
            };
            let new_mature_idx = self.alloc_mature(obj);

            // Install forwarding pointer
            self.nursery.objects[local] = Some(HeapEntry {
                obj: HeapObject::Forwarded(new_mature_idx),
                marked: false,
                pinned: false,
            });
        }

        // Phase 3: fix references in all mature objects
        self.fix_mature_references();

        // Nursery still contains forwarding pointers at this point.
        // The caller must call `fix_value` on VM roots, then `clear_nursery`.
    }

    /// Promote all live nursery objects unconditionally (called before major GC).
    pub fn promote_all_nursery(&mut self) {
        for local in 0..self.nursery.capacity {
            let obj = match &self.nursery.objects[local] {
                Some(entry) => entry.obj.clone(),
                None => continue,
            };
            if matches!(obj, HeapObject::Forwarded(_)) {
                continue;
            }
            let new_mature_idx = self.alloc_mature(obj);
            self.nursery.objects[local] = Some(HeapEntry {
                obj: HeapObject::Forwarded(new_mature_idx),
                marked: false,
                pinned: false,
            });
        }
        self.fix_mature_references();
        // Nursery still contains forwarding pointers at this point.
        // The caller must call `fix_value` on VM roots, then `clear_nursery`.
    }

    /// Clear the nursery and remembered set. Must be called after VM roots
    /// have been fixed via `fix_value`.
    pub fn clear_nursery(&mut self) {
        self.nursery.clear();
        self.remembered_set.clear();
    }

    fn scan_obj_for_nursery_ptrs(
        obj: &HeapObject,
        nursery_marked: &mut [bool],
        worklist: &mut Vec<usize>,
        capacity: usize,
    ) {
        for idx in Self::collect_ptr_indices(obj) {
            if is_nursery_idx(idx) {
                let local = nursery_local(idx);
                if local < capacity && !nursery_marked[local] {
                    nursery_marked[local] = true;
                    worklist.push(local);
                }
            }
        }
    }

    fn collect_ptr_indices(obj: &HeapObject) -> Vec<usize> {
        let mut ptrs = Vec::new();
        match obj {
            HeapObject::Closure(c) => {
                for v in &c.upvalues {
                    if v.is_ptr() {
                        ptrs.push(v.as_ptr_idx());
                    }
                }
            }
            HeapObject::Array(a) => {
                if a.tag.is_ptr() {
                    ptrs.push(a.tag.as_ptr_idx());
                }
                for v in &a.elements {
                    if v.is_ptr() {
                        ptrs.push(v.as_ptr_idx());
                    }
                }
            }
            HeapObject::Continuation(c) => {
                for frame in &c.frames {
                    for v in frame.locals_iter() {
                        if v.is_ptr() {
                            ptrs.push(v.as_ptr_idx());
                        }
                    }
                    for v in frame.stack_iter() {
                        if v.is_ptr() {
                            ptrs.push(v.as_ptr_idx());
                        }
                    }
                }
            }
            HeapObject::Slice(s) => {
                ptrs.push(s.source);
            }
            HeapObject::Cell(v) => {
                if v.is_ptr() {
                    ptrs.push(v.as_ptr_idx());
                }
            }
            HeapObject::String(_) | HeapObject::CPtr(_) | HeapObject::Forwarded(_) => {}
        }
        ptrs
    }

    /// After promoting nursery objects with forwarding pointers installed,
    /// scan all mature objects and update any references that point into the
    /// nursery to their new mature location.
    fn fix_mature_references(&mut self) {
        for slot in &mut self.objects {
            let Some(entry) = slot.as_mut() else {
                continue;
            };
            Self::fix_obj_references(&mut entry.obj, &self.nursery);
        }
    }

    fn fix_obj_references(obj: &mut HeapObject, nursery: &Nursery) {
        match obj {
            HeapObject::Closure(c) => {
                for v in &mut c.upvalues {
                    Self::fix_value_in_place(v, nursery);
                }
            }
            HeapObject::Array(a) => {
                Self::fix_value_in_place(&mut a.tag, nursery);
                for v in &mut a.elements {
                    Self::fix_value_in_place(v, nursery);
                }
            }
            HeapObject::Continuation(c) => {
                for frame in &mut c.frames {
                    for v in frame.locals_iter_mut() {
                        Self::fix_value_in_place(v, nursery);
                    }
                    for v in frame.stack_iter_mut() {
                        Self::fix_value_in_place(v, nursery);
                    }
                }
            }
            HeapObject::Slice(s) => {
                if is_nursery_idx(s.source) {
                    let local = nursery_local(s.source);
                    if let Some(Some(entry)) = nursery.objects.get(local) {
                        if let HeapObject::Forwarded(new_idx) = entry.obj {
                            s.source = new_idx;
                        }
                    }
                }
            }
            HeapObject::Cell(v) => {
                Self::fix_value_in_place(v, nursery);
            }
            HeapObject::String(_) | HeapObject::CPtr(_) | HeapObject::Forwarded(_) => {}
        }
    }

    fn fix_value_in_place(val: &mut Value, nursery: &Nursery) {
        if !val.is_ptr() {
            return;
        }
        let idx = val.as_ptr_idx();
        if !is_nursery_idx(idx) {
            return;
        }
        let local = nursery_local(idx);
        if let Some(Some(entry)) = nursery.objects.get(local) {
            if let HeapObject::Forwarded(new_idx) = entry.obj {
                *val = Value::from_ptr(new_idx);
            }
        }
    }
}

#[cfg(test)]
mod tests;
