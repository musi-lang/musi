//! Heap allocator for GC-managed objects.
//!
//! Objects are stored in a `Vec<Option<HeapObject>>`, where `None` represents
//! a freed (tombstone) slot. A free list tracks reusable indices.
//!
//! Mark-sweep GC: call `mark_reachable()` with root values, then `sweep()` to
//! reclaim unreachable objects.

use crate::error::VmError;
use crate::value::Value;

/// A heap-allocated object: a product, sum variant, array, or string.
pub struct HeapObject {
    /// Type pool id of this object's type (`type_id` in `.msbc`).
    pub type_id: u32,
    /// GC bookkeeping flags. Bit 0 = marked.
    pub gc_flags: u8,
    /// Product fields or variant payload fields.
    pub fields: Vec<Value>,
    /// Variant tag — `Some(t)` for sum variants, `None` for products.
    pub tag: Option<u32>,
    /// Array elements (non-empty only for array objects).
    pub elems: Vec<Value>,
    /// String data (non-`None` only for string objects).
    pub string: Option<Box<str>>,
}

/// The VM heap — a vector of objects indexed by their allocation address.
#[derive(Default)]
pub struct Heap {
    objects: Vec<Option<HeapObject>>,
    free_list: Vec<usize>,
}

impl Heap {
    /// Create an empty heap.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            objects: vec![],
            free_list: vec![],
        }
    }

    /// Number of live (non-freed) objects on the heap.
    #[must_use]
    pub fn live_count(&self) -> usize {
        self.objects.iter().filter(|o| o.is_some()).count()
    }

    /// Total allocated slots (including freed tombstones).
    #[must_use]
    pub const fn capacity(&self) -> usize {
        self.objects.len()
    }

    /// Allocate a product/variant object. Returns the heap index.
    ///
    /// # Panics
    ///
    /// Panics if the heap index exceeds `u64::MAX` (practically impossible).
    pub fn alloc(&mut self, type_id: u32, fields: Vec<Value>) -> u64 {
        let obj = HeapObject {
            type_id,
            gc_flags: 0,
            fields,
            tag: None,
            elems: vec![],
            string: None,
        };
        u64::try_from(self.insert(obj)).expect("heap index fits u64")
    }

    /// Allocate an array object.
    ///
    /// # Panics
    ///
    /// Panics if the heap index exceeds `u64::MAX` (practically impossible).
    pub fn alloc_array(&mut self, type_id: u32, elems: Vec<Value>) -> u64 {
        let obj = HeapObject {
            type_id,
            gc_flags: 0,
            fields: vec![],
            tag: None,
            elems,
            string: None,
        };
        u64::try_from(self.insert(obj)).expect("heap index fits u64")
    }

    /// Allocate a string object. Returns the heap index.
    ///
    /// # Panics
    ///
    /// Panics if the heap index exceeds `u64::MAX` (practically impossible).
    pub fn alloc_string(&mut self, type_id: u32, s: Box<str>) -> u64 {
        let obj = HeapObject {
            type_id,
            gc_flags: 0,
            fields: vec![],
            tag: None,
            elems: vec![],
            string: Some(s),
        };
        u64::try_from(self.insert(obj)).expect("heap index fits u64")
    }

    /// Look up an object by heap index.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` if `ptr` is not a valid heap index, or
    /// `FreedObject` if the slot has been swept.
    pub fn get(&self, ptr: usize) -> Result<&HeapObject, VmError> {
        let slot = self.objects.get(ptr).ok_or(VmError::OutOfBounds {
            index: ptr,
            len: self.objects.len(),
        })?;
        slot.as_ref().ok_or(VmError::FreedObject { index: ptr })
    }

    /// Look up an object mutably by heap index.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` if `ptr` is not a valid heap index, or
    /// `FreedObject` if the slot has been swept.
    pub fn get_mut(&mut self, ptr: usize) -> Result<&mut HeapObject, VmError> {
        let len = self.objects.len();
        let slot = self
            .objects
            .get_mut(ptr)
            .ok_or(VmError::OutOfBounds { index: ptr, len })?;
        slot.as_mut().ok_or(VmError::FreedObject { index: ptr })
    }

    // ── GC ──────────────────────────────────────────────────────────────────

    /// Mark all objects reachable from `roots`, clearing previous marks first.
    ///
    /// After calling this, call [`sweep`](Self::sweep) to free unreachable
    /// objects.
    pub fn mark_reachable(&mut self, roots: &[Value]) {
        // Clear all marks.
        for obj in self.objects.iter_mut().flatten() {
            obj.gc_flags &= !1;
        }

        // Iterative worklist traversal.
        let mut worklist: Vec<usize> = Vec::new();
        for root in roots {
            if let Some(idx) = root.try_as_ref() {
                worklist.push(idx);
            }
        }

        while let Some(idx) = worklist.pop() {
            let marked = self
                .objects
                .get(idx)
                .and_then(|slot| slot.as_ref())
                .is_some_and(|obj| obj.gc_flags & 1 != 0);

            if marked {
                continue;
            }

            if let Some(Some(obj)) = self.objects.get_mut(idx) {
                obj.gc_flags |= 1;
                for v in obj.fields.iter().chain(obj.elems.iter()) {
                    if let Some(child) = v.try_as_ref() {
                        worklist.push(child);
                    }
                }
            }
        }
    }

    /// Free all unmarked objects, adding their slots to the free list.
    ///
    /// Returns the number of objects freed.
    pub fn sweep(&mut self) -> usize {
        let mut freed = 0usize;
        for i in 0..self.objects.len() {
            let is_unmarked = self.objects[i]
                .as_ref()
                .is_some_and(|obj| obj.gc_flags & 1 == 0);
            if is_unmarked {
                self.objects[i] = None;
                self.free_list.push(i);
                freed += 1;
            }
        }
        freed
    }

    // ── Internal ────────────────────────────────────────────────────────────

    /// Insert an object, reusing a free slot if available.
    fn insert(&mut self, obj: HeapObject) -> usize {
        if let Some(idx) = self.free_list.pop() {
            self.objects[idx] = Some(obj);
            idx
        } else {
            let idx = self.objects.len();
            self.objects.push(Some(obj));
            idx
        }
    }
}
