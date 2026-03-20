//! Generational heap allocator for GC-managed objects.
//!
//! ## Regions
//!
//! **Nursery** (`NurseryHeap`): bump-pointer allocator over a pre-allocated
//! `Vec<HeapObject>`. All fresh allocations land here. When the nursery is
//! full a minor GC promotes survivors to the mature region.
//!
//! **Mature** (`MatureHeap`): mark-sweep allocator using a
//! `Vec<Option<HeapObject>>` plus a free-list - the original design.
//!
//! ## Pointer representation
//!
//! Raw heap indices stored in `Value::from_ref` map to combined "flat"
//! pointer space:
//!
//! ```text
//! flat_ptr < nursery_capacity   →  nursery object at index flat_ptr
//! flat_ptr >= nursery_capacity  →  mature object at index (flat_ptr - nursery_capacity)
//! ```
//!
//! This means `Value::from_ref` / `as_ref` / `try_as_ref` are unchanged.
//! The `Heap` facade translates flat pointers to region-local indices.
//!
//! ## GC flags
//!
//! `HeapObject::gc_flags` bit layout:
//!
//! ```text
//! bit 0 - mark (used by mark-sweep on mature space)
//! bit 1 - generation: 0 = nursery, 1 = mature
//! bits 2-3 - age counter (0-3; objects with age >= 2 promote on next minor)
//! ```

use std::collections::HashSet;

use crate::VmResult;
use crate::error::VmError;
use crate::value::Value;

const GC_MARK_BIT: u8 = 0b0000_0001;
const GC_GEN_BIT: u8 = 0b0000_0010;
const GC_AGE_SHIFT: u32 = 2;
const GC_AGE_MASK: u8 = 0b0000_1100;

/// Default nursery: 4 MB expressed as object slots.
///
/// `HeapObject` is roughly 64-80 bytes on a 64-bit target; 4 MB / 72 ≈ 55 000.
pub const DEFAULT_NURSERY_CAPACITY: usize = 55_000;

/// Major-GC interval: run a major collection every this many minor GCs.
pub const DEFAULT_MAJOR_GC_INTERVAL: usize = 16;

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
    /// Arbitrary-precision integer stored as two's complement big-endian bytes.
    BigInt(Box<[u8]>),
    /// Arena region: a block of values that can be bulk-freed.
    Arena { slots: Vec<Value> },
    /// Closure: an `fn_id` plus a vector of captured upvalue cell refs (each is a
    /// `Value::from_ref` pointing to a `HeapPayload::Upvalue` cell).
    Closure { fn_id: u32, upvalues: Vec<Value> },
    /// Open/closed upvalue cell for the Lua-style open/closed upvalue model.
    Upvalue(UpvalueCell),
}

/// A heap-allocated object with GC bookkeeping.
///
/// `gc_flags` bit layout - see module-level docs.
pub struct HeapObject {
    /// GC bookkeeping flags. bit 0 = mark, bit 1 = generation, bits 2-3 = age.
    pub gc_flags: u8,
    pub payload: HeapPayload,
}

impl HeapObject {
    /// Returns the `type_id` if this object is a Record or Array, else 0.
    #[must_use]
    pub const fn type_id(&self) -> u32 {
        match &self.payload {
            HeapPayload::Record { type_id, .. }
            | HeapPayload::Array { type_id, .. }
            | HeapPayload::Str { type_id, .. } => *type_id,
            _ => 0,
        }
    }

    const fn age(&self) -> u8 {
        (self.gc_flags & GC_AGE_MASK) >> GC_AGE_SHIFT
    }

    const fn increment_age(&mut self) {
        let age = self.age();
        if age < 3 {
            self.gc_flags = (self.gc_flags & !GC_AGE_MASK) | ((age + 1) << GC_AGE_SHIFT);
        }
    }
}

/// Bump-pointer nursery: O(1) allocation, reset on minor GC.
struct NurseryHeap {
    /// Pre-allocated object storage; slots 0..cursor are live.
    slots: Vec<Option<HeapObject>>,
    cursor: usize,
    capacity: usize,
}

impl NurseryHeap {
    fn new(capacity: usize) -> Self {
        let mut slots = Vec::with_capacity(capacity);
        for _ in 0..capacity {
            slots.push(None);
        }
        Self {
            slots,
            cursor: 0,
            capacity,
        }
    }

    const fn is_full(&self) -> bool {
        self.cursor >= self.capacity
    }

    /// # Panics
    ///
    /// Panics if the nursery is full - callers must check `is_full()` first.
    fn bump_alloc(&mut self, obj: HeapObject) -> usize {
        debug_assert!(!self.is_full(), "nursery full before GC");
        let idx = self.cursor;
        self.slots[idx] = Some(obj);
        self.cursor += 1;
        idx
    }

    fn take(&mut self, local_idx: usize) -> Option<HeapObject> {
        self.slots.get_mut(local_idx)?.take()
    }

    fn reset(&mut self) {
        for slot in &mut self.slots[..self.cursor] {
            *slot = None;
        }
        self.cursor = 0;
    }

    fn get(&self, local_idx: usize) -> Option<&HeapObject> {
        self.slots.get(local_idx)?.as_ref()
    }

    fn get_mut(&mut self, local_idx: usize) -> Option<&mut HeapObject> {
        self.slots.get_mut(local_idx)?.as_mut()
    }
}

/// Mark-sweep mature heap: `Vec<Option<HeapObject>>` + free-list.
struct MatureHeap {
    objects: Vec<Option<HeapObject>>,
    free_list: Vec<usize>,
}

impl MatureHeap {
    const fn new() -> Self {
        Self {
            objects: vec![],
            free_list: vec![],
        }
    }

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

    fn live_count(&self) -> usize {
        self.objects.iter().filter(|o| o.is_some()).count()
    }

    const fn capacity(&self) -> usize {
        self.objects.len()
    }
}

/// The VM heap - generational (nursery + mature) with write barriers.
///
/// All raw heap indices in `Value::from_ref` are "flat pointers":
/// - `flat < nursery_capacity` → nursery local index `flat`
/// - `flat >= nursery_capacity` → mature local index `flat - nursery_capacity`
pub struct Heap {
    nursery: NurseryHeap,
    mature: MatureHeap,
    remembered_set: HashSet<usize>,
    minor_gcs_since_major: usize,
    major_gc_interval: usize,
    heap_max: Option<usize>,
    pub gc_mode: GcMode,
}

impl Default for Heap {
    fn default() -> Self {
        Self::new()
    }
}

/// GC operating mode selector (spec §8f).
#[derive(Clone, Copy, Debug, PartialEq, Eq)]
pub enum GcMode {
    Serial,
    Compact,
    Concurrent,
}

impl Heap {
    /// Create a heap with default nursery capacity.
    #[must_use]
    pub fn new() -> Self {
        Self::with_nursery_capacity(DEFAULT_NURSERY_CAPACITY)
    }

    /// Create a heap with an explicit nursery slot capacity.
    #[must_use]
    pub fn with_nursery_capacity(nursery_capacity: usize) -> Self {
        Self {
            nursery: NurseryHeap::new(nursery_capacity),
            mature: MatureHeap::new(),
            remembered_set: HashSet::new(),
            minor_gcs_since_major: 0,
            major_gc_interval: DEFAULT_MAJOR_GC_INTERVAL,
            heap_max: None,
            gc_mode: GcMode::Serial,
        }
    }

    /// Set the maximum number of total flat pointer slots.
    pub const fn set_heap_max(&mut self, max: usize) {
        self.heap_max = Some(max);
    }

    /// Returns `true` when the nursery has no room for another object.
    #[must_use]
    pub const fn nursery_is_full(&self) -> bool {
        self.nursery.is_full()
    }

    /// Returns the nursery capacity (objects, not bytes).
    #[must_use]
    pub const fn nursery_capacity(&self) -> usize {
        self.nursery.capacity
    }

    /// Returns `true` if `flat_ptr` addresses a nursery object.
    #[must_use]
    pub const fn is_nursery(&self, flat_ptr: usize) -> bool {
        flat_ptr < self.nursery.capacity
    }

    /// Returns `true` if `flat_ptr` addresses a mature object.
    #[must_use]
    pub const fn is_mature(&self, flat_ptr: usize) -> bool {
        flat_ptr >= self.nursery.capacity
    }

    /// Look up an object by flat heap pointer.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` if `ptr` is not a valid flat pointer, or
    /// `FreedObject` if the slot has been swept / is a forwarding tombstone.
    pub fn get(&self, ptr: usize) -> VmResult<&HeapObject> {
        if self.is_nursery(ptr) {
            let local = ptr;
            self.nursery.get(local).ok_or(VmError::OutOfBounds {
                index: ptr,
                len: self.nursery.capacity,
            })
        } else {
            let local = ptr - self.nursery.capacity;
            let slot = self
                .mature
                .objects
                .get(local)
                .ok_or_else(|| VmError::OutOfBounds {
                    index: ptr,
                    len: self.nursery.capacity + self.mature.capacity(),
                })?;
            slot.as_ref().ok_or(VmError::FreedObject { index: ptr })
        }
    }

    /// Look up an object mutably by flat heap pointer.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` or `FreedObject` on invalid pointer.
    pub fn get_mut(&mut self, ptr: usize) -> VmResult<&mut HeapObject> {
        if self.is_nursery(ptr) {
            let local = ptr;
            let nursery_cap = self.nursery.capacity;
            self.nursery.get_mut(local).ok_or(VmError::OutOfBounds {
                index: ptr,
                len: nursery_cap,
            })
        } else {
            let local = ptr - self.nursery.capacity;
            let len = self.nursery.capacity + self.mature.capacity();
            let slot = self
                .mature
                .objects
                .get_mut(local)
                .ok_or(VmError::OutOfBounds { index: ptr, len })?;
            slot.as_mut().ok_or(VmError::FreedObject { index: ptr })
        }
    }

    fn alloc_flat(&mut self, mut obj: HeapObject) -> usize {
        obj.gc_flags &= !GC_GEN_BIT;
        self.nursery.bump_alloc(obj)
    }

    fn mature_insert(&mut self, obj: HeapObject) -> usize {
        self.nursery.capacity + self.mature.insert(obj)
    }

    /// Allocate a record or variant object. Returns the flat heap pointer.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_record(&mut self, type_id: u32, tag: Option<u32>, fields: Vec<Value>) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Record {
                type_id,
                tag,
                fields,
            },
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Allocate a closure shell with an empty upvalues vec. Returns the flat pointer.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_closure(&mut self, fn_id: u32) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Closure {
                fn_id,
                upvalues: vec![],
            },
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Allocate an array object.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_array(&mut self, type_id: u32, elems: Vec<Value>) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Array { type_id, elems },
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Allocate a string object. Returns the flat pointer.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_string(&mut self, type_id: u32, s: Box<str>) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Str { type_id, data: s },
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Allocate a heap-boxed wide signed integer. Returns the flat pointer.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_wide_int(&mut self, n: i64) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::BoxedInt(n),
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Allocate a heap-boxed wide unsigned integer. Returns the flat pointer.
    ///
    /// The u64 value is stored as i64 via bit reinterpretation.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_wide_nat(&mut self, n: u64) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::BoxedNat(n.cast_signed()),
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Allocate an arena with the given initial capacity.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_arena(&mut self, capacity: usize) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Arena {
                slots: Vec::with_capacity(capacity),
            },
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Push a value into an arena, returning its index within the arena.
    ///
    /// # Errors
    ///
    /// Returns `VmError::Halted` if the arena pointer is out of bounds or if the payload is not an arena.
    pub fn arena_alloc(&mut self, arena_ptr: usize, val: Value) -> VmResult<u32> {
        let obj = self.get_mut(arena_ptr)?;
        match &mut obj.payload {
            HeapPayload::Arena { slots } => {
                let idx = u32::try_from(slots.len()).map_err(|_| VmError::Halted)?;
                slots.push(val);
                Ok(idx)
            }
            _ => Err(VmError::Halted),
        }
    }

    /// Free an arena, dropping all its contents.
    ///
    /// # Errors
    ///
    /// Returns `VmError::Halted` if the arena pointer is out of bounds or if the payload is not an arena.
    ///
    /// # Panics
    ///
    /// Panics if the arena pointer is out of bounds.
    pub fn arena_free(&mut self, arena_ptr: usize) -> VmResult {
        let obj = self.get_mut(arena_ptr)?;
        match &mut obj.payload {
            HeapPayload::Arena { slots } => {
                slots.clear();
                Ok(())
            }
            _ => Err(VmError::Halted),
        }
    }

    /// Allocate a heap-boxed arbitrary-precision integer from two's complement bytes.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_bigint(&mut self, bytes: Box<[u8]>) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::BigInt(bytes),
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Allocate an upvalue cell. Returns the flat pointer.
    ///
    /// # Panics
    ///
    /// Panics if the heap pointer exceeds `u64::MAX`.
    pub fn alloc_upvalue(&mut self, cell: UpvalueCell) -> u64 {
        let obj = HeapObject {
            gc_flags: 0,
            payload: HeapPayload::Upvalue(cell),
        };
        u64::try_from(self.alloc_flat(obj)).expect("heap index fits u64")
    }

    /// Manually free a heap object by its flat pointer.
    ///
    /// Only valid for mature objects. Nursery objects are collected en-masse.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` if `ptr` is not a valid flat pointer, or
    /// `FreedObject` if the slot has already been freed.
    pub fn free(&mut self, ptr: usize) -> VmResult {
        if self.is_nursery(ptr) {
            return Ok(());
        }
        let local = ptr - self.nursery.capacity;
        let len = self.nursery.capacity + self.mature.capacity();
        let slot = self
            .mature
            .objects
            .get_mut(local)
            .ok_or(VmError::OutOfBounds { index: ptr, len })?;
        if slot.is_none() {
            return Err(VmError::FreedObject { index: ptr });
        }
        *slot = None;
        self.mature.free_list.push(local);
        Ok(())
    }

    /// Return the string data stored at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` / `FreedObject` on bad pointer, or `TypeError` if
    /// the object is not a `HeapPayload::Str`.
    pub fn get_string(&self, ptr: usize) -> VmResult<&str> {
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
    pub fn get_array(&self, ptr: usize) -> VmResult<&[Value]> {
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
    pub fn get_array_mut(&mut self, ptr: usize) -> VmResult<&mut Vec<Value>> {
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
    pub fn get_record_fields(&self, ptr: usize) -> VmResult<&[Value]> {
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
    pub fn get_record_fields_mut(&mut self, ptr: usize) -> VmResult<&mut Vec<Value>> {
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
    pub fn get_record_tag(&self, ptr: usize) -> VmResult<Option<u32>> {
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
    pub fn set_record_tag(&mut self, ptr: usize, new_tag: Option<u32>) -> VmResult<()> {
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
    pub fn get_closure(&self, ptr: usize) -> VmResult<(u32, &[Value])> {
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
    pub fn get_closure_upvalues_mut(&mut self, ptr: usize) -> VmResult<&mut Vec<Value>> {
        match &mut self.get_mut(ptr)?.payload {
            HeapPayload::Closure { upvalues, .. } => Ok(upvalues),
            _ => Err(VmError::TypeError {
                expected: "closure",
                found: "non-closure ref",
            }),
        }
    }

    /// Return a shared reference to the upvalue cell at `ptr`.
    ///
    /// # Errors
    ///
    /// Returns `OutOfBounds` / `FreedObject` on bad pointer, or `TypeError` if
    /// the object is not a `HeapPayload::Upvalue`.
    pub fn get_upvalue(&self, ptr: usize) -> VmResult<&UpvalueCell> {
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
    pub fn get_upvalue_mut(&mut self, ptr: usize) -> VmResult<&mut UpvalueCell> {
        match &mut self.get_mut(ptr)?.payload {
            HeapPayload::Upvalue(cell) => Ok(cell),
            _ => Err(VmError::TypeError {
                expected: "upvalue",
                found: "non-upvalue ref",
            }),
        }
    }

    /// Write barrier: if `target_ptr` is in mature space and `stored_val`
    /// is a nursery reference, add `target_ptr` to the remembered set.
    ///
    /// Call this after every store of a `Value` into a heap object field.
    pub fn write_barrier(&mut self, target_ptr: usize, stored_val: Value) {
        if self.is_mature(target_ptr) {
            if let Some(stored_ptr) = stored_val.try_as_ref() {
                if self.is_nursery(stored_ptr) {
                    let _ = self.remembered_set.insert(target_ptr);
                }
            }
        }
    }

    /// Run a stop-the-world minor GC.
    ///
    /// 1. Collect root flat pointers from `roots` + remembered set.
    /// 2. Copy surviving nursery objects into mature space.
    /// 3. Build a forwarding table (old flat ptr → new flat ptr).
    /// 4. Patch all references in roots and mature space.
    /// 5. Reset the nursery bump pointer.
    ///
    /// Returns the number of nursery objects that were promoted.
    pub fn minor_gc(&mut self, roots: &mut [Value]) -> usize {
        let mut nursery_live: Vec<usize> = vec![];

        for root in roots.iter() {
            if let Some(ptr) = root.try_as_ref() {
                if self.is_nursery(ptr) {
                    nursery_live.push(ptr);
                }
            }
        }

        let rem: Vec<usize> = self.remembered_set.iter().copied().collect();
        for mature_flat in rem {
            let local = mature_flat - self.nursery.capacity;
            if let Some(Some(obj)) = self.mature.objects.get(local) {
                for child in object_refs(&obj.payload) {
                    if self.is_nursery(child) {
                        nursery_live.push(child);
                    }
                }
            }
        }

        let mut visited: Vec<bool> = vec![false; self.nursery.capacity];
        let mut worklist = nursery_live.clone();
        while let Some(ptr) = worklist.pop() {
            if visited[ptr] {
                continue;
            }
            visited[ptr] = true;
            if let Some(obj) = self.nursery.get(ptr) {
                for child in object_refs(&obj.payload) {
                    if self.is_nursery(child) && !visited[child] {
                        worklist.push(child);
                    }
                }
            }
        }

        // forwarding[nursery_local] = new flat pointer after promotion.
        let mut forwarding: Vec<Option<usize>> = vec![None; self.nursery.cursor];
        let mut promoted = 0usize;

        for local in 0..self.nursery.cursor {
            if !visited[local] {
                continue;
            }
            if let Some(mut obj) = self.nursery.take(local) {
                obj.increment_age();
                obj.gc_flags |= GC_GEN_BIT;
                let new_flat = self.mature_insert(obj);
                forwarding[local] = Some(new_flat);
                promoted += 1;
            }
        }

        for root in roots.iter_mut() {
            patch_value(root, &forwarding, self.nursery.capacity);
        }

        for slot in self.mature.objects.iter_mut().flatten() {
            patch_payload(&mut slot.payload, &forwarding, self.nursery.capacity);
        }

        self.remembered_set.clear();
        self.nursery.reset();
        self.minor_gcs_since_major += 1;

        promoted
    }

    /// Returns `true` if a major GC should run (interval threshold reached
    /// or mature space exceeds a heuristic).
    #[must_use]
    pub const fn needs_major_gc(&self) -> bool {
        self.minor_gcs_since_major >= self.major_gc_interval
    }

    /// Mark-sweep over mature space. Roots include `roots` plus all live nursery objects.
    ///
    /// Returns the number of mature objects freed.
    pub fn major_gc(&mut self, roots: &[Value]) -> usize {
        for slot in self.mature.objects.iter_mut().flatten() {
            slot.gc_flags &= !GC_MARK_BIT;
        }

        let mut worklist: Vec<usize> = vec![];

        for root in roots {
            if let Some(ptr) = root.try_as_ref() {
                if self.is_mature(ptr) {
                    worklist.push(ptr - self.nursery.capacity);
                }
            }
        }

        for local in 0..self.nursery.cursor {
            if let Some(obj) = self.nursery.get(local) {
                for child in object_refs(&obj.payload) {
                    if self.is_mature(child) {
                        worklist.push(child - self.nursery.capacity);
                    }
                }
            }
        }

        while let Some(local) = worklist.pop() {
            let already_marked = self
                .mature
                .objects
                .get(local)
                .and_then(|s| s.as_ref())
                .is_some_and(|o| o.gc_flags & GC_MARK_BIT != 0);

            if already_marked {
                continue;
            }

            if let Some(Some(obj)) = self.mature.objects.get_mut(local) {
                obj.gc_flags |= GC_MARK_BIT;
                let children = object_refs(&obj.payload);
                for child in children {
                    if self.is_mature(child) {
                        worklist.push(child - self.nursery.capacity);
                    }
                }
            }
        }

        let mut freed = 0usize;
        for i in 0..self.mature.objects.len() {
            let is_unmarked = self
                .mature
                .objects
                .get(i)
                .and_then(|s| s.as_ref())
                .is_some_and(|o| o.gc_flags & GC_MARK_BIT == 0);
            if is_unmarked {
                self.mature.objects[i] = None;
                self.mature.free_list.push(i);
                freed += 1;
            }
        }

        self.minor_gcs_since_major = 0;
        freed
    }

    /// Number of live objects across both regions.
    #[must_use]
    pub fn live_count(&self) -> usize {
        self.nursery.cursor + self.mature.live_count()
    }

    /// Total allocated object slots across both regions.
    #[must_use]
    pub const fn capacity(&self) -> usize {
        self.nursery.capacity + self.mature.capacity()
    }

    /// Mark all mature objects reachable from `roots`, clearing previous marks first.
    ///
    /// Pair with [`sweep`](Self::sweep). Mature-only; nursery is managed by minor GC.
    pub fn mark_reachable(&mut self, roots: &[Value]) {
        for slot in self.mature.objects.iter_mut().flatten() {
            slot.gc_flags &= !GC_MARK_BIT;
        }

        let mut worklist: Vec<usize> = vec![];

        for root in roots {
            if let Some(ptr) = root.try_as_ref() {
                if self.is_mature(ptr) {
                    worklist.push(ptr - self.nursery.capacity);
                }
            }
        }

        for local in 0..self.nursery.cursor {
            if let Some(obj) = self.nursery.get(local) {
                for child in object_refs(&obj.payload) {
                    if self.is_mature(child) {
                        worklist.push(child - self.nursery.capacity);
                    }
                }
            }
        }

        while let Some(local) = worklist.pop() {
            let already_marked = self
                .mature
                .objects
                .get(local)
                .and_then(|s| s.as_ref())
                .is_some_and(|o| o.gc_flags & GC_MARK_BIT != 0);

            if already_marked {
                continue;
            }

            if let Some(Some(obj)) = self.mature.objects.get_mut(local) {
                obj.gc_flags |= GC_MARK_BIT;
                let children = object_refs(&obj.payload);
                for child in children {
                    if self.is_mature(child) {
                        worklist.push(child - self.nursery.capacity);
                    }
                }
            }
        }
    }

    /// Free all unmarked mature objects, adding their slots to the free list.
    ///
    /// Returns the number of objects freed.
    pub fn sweep(&mut self) -> usize {
        let mut freed = 0usize;
        for i in 0..self.mature.objects.len() {
            let is_unmarked = self
                .mature
                .objects
                .get(i)
                .and_then(|s| s.as_ref())
                .is_some_and(|o| o.gc_flags & GC_MARK_BIT == 0);
            if is_unmarked {
                self.mature.objects[i] = None;
                self.mature.free_list.push(i);
                freed += 1;
            }
        }
        freed
    }
}

/// Collect all ref-typed child values of a payload as flat heap pointers.
fn object_refs(payload: &HeapPayload) -> Vec<usize> {
    match payload {
        HeapPayload::Record { fields, .. } => {
            fields.iter().filter_map(|v| v.try_as_ref()).collect()
        }
        HeapPayload::Array { elems, .. } => elems.iter().filter_map(|v| v.try_as_ref()).collect(),
        HeapPayload::Closure { upvalues, .. } => {
            upvalues.iter().filter_map(|v| v.try_as_ref()).collect()
        }
        HeapPayload::Upvalue(UpvalueCell::Closed(v)) => v.try_as_ref().into_iter().collect(),
        HeapPayload::Upvalue(UpvalueCell::Open { .. })
        | HeapPayload::Str { .. }
        | HeapPayload::BoxedInt(_)
        | HeapPayload::BoxedNat(_)
        | HeapPayload::BigInt(_) => vec![],
        HeapPayload::Arena { slots } => slots.iter().filter_map(|v| v.try_as_ref()).collect(),
    }
}

/// Rewrite a `Value` using the forwarding table when it is a nursery ref that
/// was promoted. `forwarding[nursery_local]` gives the new flat pointer.
fn patch_value(val: &mut Value, forwarding: &[Option<usize>], nursery_capacity: usize) {
    if let Some(ptr) = val.try_as_ref() {
        if ptr < nursery_capacity {
            if let Some(new_flat) = forwarding.get(ptr).copied().flatten() {
                *val = Value::from_ref(u64::try_from(new_flat).unwrap_or(u64::MAX));
            }
        }
    }
}

/// Recursively patch all `Value` references inside a payload.
fn patch_payload(payload: &mut HeapPayload, forwarding: &[Option<usize>], nursery_capacity: usize) {
    match payload {
        HeapPayload::Record { fields, .. } => {
            for v in fields.iter_mut() {
                patch_value(v, forwarding, nursery_capacity);
            }
        }
        HeapPayload::Array { elems, .. } => {
            for v in elems.iter_mut() {
                patch_value(v, forwarding, nursery_capacity);
            }
        }
        HeapPayload::Closure { upvalues, .. } => {
            for v in upvalues.iter_mut() {
                patch_value(v, forwarding, nursery_capacity);
            }
        }
        HeapPayload::Upvalue(UpvalueCell::Closed(v)) => {
            patch_value(v, forwarding, nursery_capacity);
        }
        HeapPayload::Arena { slots } => {
            for v in slots.iter_mut() {
                patch_value(v, forwarding, nursery_capacity);
            }
        }
        HeapPayload::Upvalue(UpvalueCell::Open { .. })
        | HeapPayload::Str { .. }
        | HeapPayload::BoxedInt(_)
        | HeapPayload::BoxedNat(_)
        | HeapPayload::BigInt(_) => {}
    }
}
