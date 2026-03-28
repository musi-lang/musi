use super::Vm;
use crate::value::Value;

impl Vm {
    pub(super) fn maybe_collect(&mut self) {
        if self.heap.should_collect() {
            if self.heap.should_major() {
                self.collect_major();
            } else {
                self.collect_minor();
            }
        }
    }

    pub(super) fn collect_minor(&mut self) {
        let roots = self.gather_roots();
        self.heap.collect_minor(&roots);
        self.heap.reset_threshold();
    }

    pub(super) fn collect_major(&mut self) {
        let roots = self.gather_roots();
        self.heap.collect_major(&roots);
        self.heap.reset_threshold();
    }

    pub(super) fn gather_roots(&self) -> Vec<Value> {
        let mut roots = Vec::new();
        roots.extend_from_slice(&self.globals);
        roots.extend_from_slice(&self.resolved_constants);
        for frame in &self.frames {
            roots.extend(frame.locals_iter());
            roots.extend(frame.stack_iter());
            if let Some(idx) = frame.closure {
                roots.push(Value::from_ptr(idx));
            }
        }
        roots
    }

    pub(super) fn write_barrier(&mut self, target_idx: usize, stored_value: Value) {
        if stored_value.is_ptr()
            && self.heap.is_marked(target_idx)
            && !self.heap.is_marked(stored_value.as_ptr_idx())
        {
            self.heap.remember(target_idx);
        }
    }
}
