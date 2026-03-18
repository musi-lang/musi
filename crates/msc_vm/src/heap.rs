//! Heap allocator for GC-managed objects.
//!
//! Objects are stored in a `Vec<Option<HeapObject>>`, where `None` represents
//! a freed (tombstone) slot. A free list tracks reusable indices.
//!
//! Mark-sweep GC: call `mark_reachable()` with root values, then `sweep()` to
//! reclaim unreachable objects.

use crate::error::VmError;
use crate::value::Value;

/// An upvalue cell: either open (pointing into a live frame's locals) or
/// closed (capturing the value after the originating frame exited).
pub enum UpvalueCell {
    /// The captured variable is still live in a frame.
    /// `frame_depth` is the index into `Vm::call_stack`; `slot` is the local index.
    Open { frame_depth: usize, slot: usize },
    /// The originating frame has exited; the value is stored here.
    Closed(Value),
}

/// Discriminated payload for a heap-allocated object.
///
/// Each variant only carries the fields it needs, eliminating the wasted
/// memory of the old flat `HeapObject` struct.
pub enum HeapPayload {
    /// Product type (record) or sum variant. `tag` is `None` for products,
    /// `Some(t)` for sum variants.
    Record {
        type_id: u32,
        tag: Option<u32>,
        fields: Vec<Value>,
    },
    /// Array of VM values.
    Array { type_id: u32, elems: Vec<Value> },
    /// Heap-allocated string.
    Str { type_id: u32, data: Box<str> },
    /// Wide signed integer that does not fit in the inline 48-bit payload.
    BoxedInt(i64),
    /// Wide unsigned integer that does not fit in the inline 48-bit payload.
    BoxedNat(i64),
    /// Closure: an `fn_id` plus a vector of captured upvalue cell refs (each is a
    /// `Value::from_ref` pointing to a `HeapPayload::Upvalue` cell).
    Closure { fn_id: u32, upvalues: Vec<Value> },
    /// Open/closed upvalue cell for the Lua-style open/closed upvalue model.
    Upvalue(UpvalueCell),
}

/// A heap-allocated object with GC bookkeeping.
pub struct HeapObject {
    /// GC bookkeeping flags. Bit 0 = marked.
    pub gc_flags: u8,
    pub payload: HeapPayload,
}

// Convenience accessors on HeapObject for the few callers that use get()/get_mut()
// and need to reach into the payload without going through the typed Heap methods.
impl HeapObject {
    /// Returns the `type_id` if this object is a Record or Array, else 0.
    #[must_use]
    pub fn type_id(&self) -> u32 {
        match &self.payload {
            HeapPayload::Record { type_id, .. } | HeapPayload::Array { type_id, .. } => *type_id,
            HeapPayload::Str { type_id, .. } => *type_id,
            _ => 0,
        }
    }
}

/// The VM heap - a vector of objects indexed by their allocation address.
#[derive(Default)]
pub struct Heap {
    objects: Vec<Option<HeapObject>>,
    free_list: Vec<usize>,
    allocs_since_gc: usize,
    gc_threshold: usize,
}

impl Heap {
    /// Create an empty heap.
    #[must_use]
    pub const fn new() -> Self {
        Self {
            objects: vec![],
            free_list: vec![],
            allocs_since_gc: 0,
            gc_threshold: 256,
        }
    }

    /// Returns `true` if the allocation count since the last GC has reached
    /// the threshold. The VM should call `collect_garbage` when this is true.
    #[must_use]
    pub const fn needs_gc(&self) -> bool {
        self.allocs_since_gc >= self.gc_threshold
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

    /// Allocate a record or variant object. Returns the heap index.
    ///
    /// # Panics
    ///
    /// Panics if the heap index exceeds `u64::MAX` (practically impossible).
    pub fn alloc_record(&mut self, type_id: u32, tag: Option<u32>, fields: Vec<Value>) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Record {
                type_id,
                tag,
                fields,
            },
        };
        u64::try_from(self.insert(obj)).expect("heap index fits u64")
    }

    /// Allocate a closure shell with an empty upvalues vec. Returns the heap index.
    ///
    /// # Panics
    ///
    /// Panics if the heap index exceeds `u64::MAX`.
    pub fn alloc_closure(&mut self, fn_id: u32) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Closure {
                fn_id,
                upvalues: vec![],
            },
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
            gc_flags: 0,
            payload: HeapPayload::Array { type_id, elems },
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
            gc_flags: 0,
            payload: HeapPayload::Str { type_id, data: s },
        };
        u64::try_from(self.insert(obj)).expect("heap index fits u64")
    }

    /// Allocate a heap-boxed wide signed integer. Returns the heap index.
    ///
    /// # Panics
    ///
    /// Panics if the heap index exceeds `u64::MAX`.
    pub fn alloc_wide_int(&mut self, n: i64) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::BoxedInt(n),
        };
        u64::try_from(self.insert(obj)).expect("heap index fits u64")
    }

    /// Allocate a heap-boxed wide unsigned integer. Returns the heap index.
    ///
    /// The u64 value is stored as i64 via bit reinterpretation.
    ///
    /// # Panics
    ///
    /// Panics if the heap index exceeds `u64::MAX`.
    pub fn alloc_wide_nat(&mut self, n: u64) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::BoxedNat(n.cast_signed()),
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

    // ------------------------------------------------------------------
    // Typed accessors
    // ------------------------------------------------------------------

    /// Return the string data stored at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` / `FreedObject` on bad pointer, or `TypeError` if
    /// the object is not a `HeapPayload::Str`.
    pub fn get_string(&self, ptr: usize) -> Result<&str, VmError> {
        match &self.get(ptr)?.payload {
            HeapPayload::Str { data, .. } => Ok(data.as_ref()),
            _ => Err(VmError::TypeError {
                expected: "string",
                found: "non-string ref",
            }),
        }
    }

    /// Return the element slice of an array at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if the object is not a `HeapPayload::Array`.
    pub fn get_array(&self, ptr: usize) -> Result<&[Value], VmError> {
        match &self.get(ptr)?.payload {
            HeapPayload::Array { elems, .. } => Ok(elems.as_slice()),
            _ => Err(VmError::TypeError {
                expected: "array",
                found: "non-array ref",
            }),
        }
    }

    /// Return the mutable element vec of an array at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if the object is not a `HeapPayload::Array`.
    pub fn get_array_mut(&mut self, ptr: usize) -> Result<&mut Vec<Value>, VmError> {
        match &mut self.get_mut(ptr)?.payload {
            HeapPayload::Array { elems, .. } => Ok(elems),
            _ => Err(VmError::TypeError {
                expected: "array",
                found: "non-array ref",
            }),
        }
    }

    /// Return the field slice of a record at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if the object is not a `HeapPayload::Record`.
    pub fn get_record_fields(&self, ptr: usize) -> Result<&[Value], VmError> {
        match &self.get(ptr)?.payload {
            HeapPayload::Record { fields, .. } => Ok(fields.as_slice()),
            _ => Err(VmError::TypeError {
                expected: "record",
                found: "non-record ref",
            }),
        }
    }

    /// Return the mutable field vec of a record at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if the object is not a `HeapPayload::Record`.
    pub fn get_record_fields_mut(&mut self, ptr: usize) -> Result<&mut Vec<Value>, VmError> {
        match &mut self.get_mut(ptr)?.payload {
            HeapPayload::Record { fields, .. } => Ok(fields),
            _ => Err(VmError::TypeError {
                expected: "record",
                found: "non-record ref",
            }),
        }
    }

    /// Return the variant tag of a record at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if the object is not a `HeapPayload::Record`.
    pub fn get_record_tag(&self, ptr: usize) -> Result<Option<u32>, VmError> {
        match &self.get(ptr)?.payload {
            HeapPayload::Record { tag, .. } => Ok(*tag),
            _ => Err(VmError::TypeError {
                expected: "record",
                found: "non-record ref",
            }),
        }
    }

    /// Set the variant tag of a record at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if the object is not a `HeapPayload::Record`.
    pub fn set_record_tag(&mut self, ptr: usize, new_tag: Option<u32>) -> Result<(), VmError> {
        match &mut self.get_mut(ptr)?.payload {
            HeapPayload::Record { tag, .. } => {
                *tag = new_tag;
                Ok(())
            }
            _ => Err(VmError::TypeError {
                expected: "record",
                found: "non-record ref",
            }),
        }
    }

    /// Return `(fn_id, upvalues)` for the closure at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if the object is not a `HeapPayload::Closure`.
    pub fn get_closure(&self, ptr: usize) -> Result<(u32, &[Value]), VmError> {
        match &self.get(ptr)?.payload {
            HeapPayload::Closure { fn_id, upvalues } => Ok((*fn_id, upvalues.as_slice())),
            _ => Err(VmError::TypeError {
                expected: "closure",
                found: "non-closure ref",
            }),
        }
    }

    /// Return the mutable upvalues vec for the closure at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `TypeError` if the object is not a `HeapPayload::Closure`.
    pub fn get_closure_upvalues_mut(&mut self, ptr: usize) -> Result<&mut Vec<Value>, VmError> {
        match &mut self.get_mut(ptr)?.payload {
            HeapPayload::Closure { upvalues, .. } => Ok(upvalues),
            _ => Err(VmError::TypeError {
                expected: "closure",
                found: "non-closure ref",
            }),
        }
    }

    /// Allocate an upvalue cell. Returns the heap index.
    ///
    /// # Panics
    ///
    /// Panics if the heap index exceeds `u64::MAX`.
    pub fn alloc_upvalue(&mut self, cell: UpvalueCell) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Upvalue(cell),
        };
        u64::try_from(self.insert(obj)).expect("heap index fits u64")
    }

    /// Return a shared reference to the upvalue cell at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` / `FreedObject` on bad pointer, or `TypeError` if
    /// the object is not a `HeapPayload::Upvalue`.
    pub fn get_upvalue(&self, ptr: usize) -> Result<&UpvalueCell, VmError> {
        match &self.get(ptr)?.payload {
            HeapPayload::Upvalue(cell) => Ok(cell),
            _ => Err(VmError::TypeError {
                expected: "upvalue",
                found: "non-upvalue ref",
            }),
        }
    }

    /// Return a mutable reference to the upvalue cell at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` / `FreedObject` on bad pointer, or `TypeError` if
    /// the object is not a `HeapPayload::Upvalue`.
    pub fn get_upvalue_mut(&mut self, ptr: usize) -> Result<&mut UpvalueCell, VmError> {
        match &mut self.get_mut(ptr)?.payload {
            HeapPayload::Upvalue(cell) => Ok(cell),
            _ => Err(VmError::TypeError {
                expected: "upvalue",
                found: "non-upvalue ref",
            }),
        }
    }

    /// Manually free a heap object by its index.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` if `ptr` is not a valid heap index, or
    /// `FreedObject` if the slot has already been freed.
    pub fn free(&mut self, ptr: usize) -> Result<(), VmError> {
        let len = self.objects.len();
        let slot = self
            .objects
            .get_mut(ptr)
            .ok_or(VmError::OutOfBounds { index: ptr, len })?;
        if slot.is_none() {
            return Err(VmError::FreedObject { index: ptr });
        }
        *slot = None;
        self.free_list.push(ptr);
        Ok(())
    }

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
        let mut worklist: Vec<usize> = vec![];
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
                let children: Vec<usize> = match &obj.payload {
                    HeapPayload::Record { fields, .. } => {
                        fields.iter().filter_map(|v| v.try_as_ref()).collect()
                    }
                    HeapPayload::Array { elems, .. } => {
                        elems.iter().filter_map(|v| v.try_as_ref()).collect()
                    }
                    HeapPayload::Closure { upvalues, .. } => {
                        upvalues.iter().filter_map(|v| v.try_as_ref()).collect()
                    }
                    HeapPayload::Upvalue(UpvalueCell::Closed(v)) => {
                        v.try_as_ref().into_iter().collect()
                    }
                    HeapPayload::Upvalue(UpvalueCell::Open { .. }) => vec![],
                    HeapPayload::Str { .. }
                    | HeapPayload::BoxedInt(_)
                    | HeapPayload::BoxedNat(_) => vec![],
                };
                worklist.extend(children);
            }
        }
    }

    /// Free all unmarked objects, adding their slots to the free list.
    ///
    /// Updates the GC threshold adaptively based on the surviving live count.
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
        let live = self.live_count();
        self.gc_threshold = 256usize.max(2 * live);
        self.allocs_since_gc = 0;
        freed
    }

    /// Insert an object, reusing a free slot if available.
    fn insert(&mut self, obj: HeapObject) -> usize {
        self.allocs_since_gc += 1;
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
