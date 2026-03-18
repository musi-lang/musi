//! Garbage collection helpers.

use crate::value::Value;
use crate::vm::Vm;

impl Vm {
    /// Run a mark-sweep garbage collection cycle.
    pub fn collect_garbage(&mut self) -> usize {
        let roots = self.collect_roots();
        self.heap.mark_reachable(&roots);
        self.heap.sweep()
    }

    pub(super) fn maybe_gc(&mut self) {
        if self.heap.needs_gc() {
            let _ = self.collect_garbage();
        }
    }

    fn collect_roots(&self) -> Vec<Value> {
        let mut roots = vec![];
        for frame in &self.call_stack {
            roots.extend_from_slice(&frame.locals);
            roots.extend_from_slice(&frame.stack);
            if let Some(cr) = frame.closure_ref {
                roots.push(cr);
            }
        }
        for &upv_ptr in self.open_upvalue_map.values() {
            roots.push(Value::from_ref(upv_ptr as u64));
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
}
