//! Garbage collection helpers.

use crate::value::Value;
use crate::vm::Vm;

impl Vm {
    /// Full GC cycle: minor evacuation then optional major sweep.
    ///
    /// Returns total objects freed (nursery dead + mature swept).
    pub fn collect_garbage(&mut self) -> usize {
        let mut roots = self.collect_roots();
        let nursery_before = self.heap.live_count();
        let promoted = self.heap.minor_gc(&mut roots);
        self.patch_roots_from_vec(&roots);
        let nursery_dead = nursery_before.saturating_sub(promoted);

        let mature_freed = if self.heap.needs_major_gc() {
            let roots = self.collect_roots();
            self.heap.major_gc(&roots)
        } else {
            0
        };

        nursery_dead + mature_freed
    }

    /// Safepoint GC check: evacuate the nursery when full, then run major GC
    /// if the minor-GC interval has been reached.
    ///
    /// Called only at safepoint opcodes (allocs, calls, effect operations).
    pub(super) fn safepoint_gc(&mut self) {
        if !self.heap.nursery_is_full() {
            return;
        }
        let mut roots = self.collect_roots();
        let _ = self.heap.minor_gc(&mut roots);
        self.patch_roots_from_vec(&roots);
        if self.heap.needs_major_gc() {
            let roots = self.collect_roots();
            let _ = self.heap.major_gc(&roots);
        }
    }

    pub(super) fn collect_roots(&self) -> Vec<Value> {
        let mut roots = vec![];
        for frame in &self.call_stack {
            roots.extend_from_slice(&frame.locals);
            roots.extend_from_slice(&frame.stack);
            if let Some(cr) = frame.closure_ref {
                roots.push(cr);
            }
        }
        for &upv_ptr in self.open_upvalue_map.values() {
            roots.push(Value::from_ref(u64::try_from(upv_ptr).unwrap_or(u64::MAX)));
        }
        for cont in &self.continuations {
            for frame in &cont.frames {
                roots.extend_from_slice(&frame.locals);
                roots.extend_from_slice(&frame.stack);
                if let Some(cr) = frame.closure_ref {
                    roots.push(cr);
                }
            }
        }
        roots.extend_from_slice(&self.globals);
        roots
    }

    /// Write a post-minor-GC root vector back into live VM state.
    ///
    /// `minor_gc` patches forwarding pointers in the slice in-place; this
    /// method mirrors the layout of `collect_roots` to push them back.
    fn patch_roots_from_vec(&mut self, roots: &[Value]) {
        let mut idx = 0usize;

        for frame in &mut self.call_stack {
            let llen = frame.locals.len();
            frame.locals.copy_from_slice(&roots[idx..idx + llen]);
            idx += llen;
            let slen = frame.stack.len();
            frame.stack.copy_from_slice(&roots[idx..idx + slen]);
            idx += slen;
            if frame.closure_ref.is_some() {
                frame.closure_ref = Some(roots[idx]);
                idx += 1;
            }
        }

        for upv_ptr in self.open_upvalue_map.values_mut() {
            if let Some(new_ptr) = roots[idx].try_as_ref() {
                *upv_ptr = new_ptr;
            }
            idx += 1;
        }

        for cont in &mut self.continuations {
            for frame in &mut cont.frames {
                let llen = frame.locals.len();
                frame.locals.copy_from_slice(&roots[idx..idx + llen]);
                idx += llen;
                let slen = frame.stack.len();
                frame.stack.copy_from_slice(&roots[idx..idx + slen]);
                idx += slen;
                if frame.closure_ref.is_some() {
                    frame.closure_ref = Some(roots[idx]);
                    idx += 1;
                }
            }
        }

        let glen = self.globals.len();
        self.globals.copy_from_slice(&roots[idx..idx + glen]);
    }
}
