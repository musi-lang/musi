use super::state::{CallFrameList, EffectHandlerList, LoadedModuleList, ResumeList};
use super::{GcRef, HeapCollectionStats, HeapOptions, Value, Vm, VmError, VmErrorKind, VmResult};

impl Vm {
    pub(crate) fn observe_heap_value(&self, value: &Value) -> VmResult {
        if let Some(reference) = value.gc_ref() {
            self.heap.validate_ref(reference)?;
        }
        Ok(())
    }

    pub(super) fn retain_external_value(&mut self, value: &Value) -> VmResult {
        if !is_heap_value(value) {
            return Ok(());
        }
        self.observe_heap_value(value)?;
        self.external_roots.push(value.clone());
        self.enforce_heap_limit_with_extra(Some(value))
    }

    pub(crate) fn after_heap_allocation(&mut self, allocated: &Value) -> VmResult {
        self.heap_dirty = true;
        self.enforce_heap_limit_with_extra(Some(allocated))
    }

    pub(crate) fn after_host_call_result(&mut self, result: &Value) -> VmResult {
        self.observe_heap_value(result)?;
        if is_heap_value(result) {
            self.heap_dirty = true;
        }
        self.enforce_heap_limit_with_extra(Some(result))
    }

    pub(super) fn enforce_heap_limit_with_extra(&mut self, extra_root: Option<&Value>) -> VmResult {
        if self.options.gc_stress {
            let _ = self.collect_garbage_with_extra(extra_root);
        }
        let Some(limit) = self.options.heap_limit_bytes else {
            return Ok(());
        };
        if self.heap.allocated_bytes() <= limit {
            return Ok(());
        }
        let _ = self.collect_garbage_with_extra(extra_root);
        let allocated = self.heap.allocated_bytes();
        if allocated <= limit {
            Ok(())
        } else {
            Err(VmError::new(VmErrorKind::HeapLimitExceeded {
                allocated,
                limit,
            }))
        }
    }

    pub(super) const fn heap_options(&self) -> HeapOptions {
        HeapOptions {
            max_object_bytes: self.options.max_object_bytes,
        }
    }

    pub(super) fn collect_garbage_with_extra(
        &mut self,
        extra_root: Option<&Value>,
    ) -> HeapCollectionStats {
        let loaded_modules = &self.loaded_modules;
        let frames = &self.frames;
        let handlers = &self.handlers;
        let active_resumes = &self.active_resumes;
        let external_roots = &self.external_roots;
        let stats = self.heap.collect_from_refs(Self::heap_root_refs(
            loaded_modules,
            frames,
            handlers,
            active_resumes,
            external_roots,
            extra_root,
        ));
        self.heap_dirty = false;
        stats
    }

    fn heap_root_refs<'a>(
        loaded_modules: &'a LoadedModuleList,
        frames: &'a CallFrameList,
        handlers: &'a EffectHandlerList,
        active_resumes: &'a ResumeList,
        external_roots: &'a [Value],
        extra_root: Option<&'a Value>,
    ) -> impl Iterator<Item = GcRef> + 'a {
        loaded_modules
            .iter()
            .flat_map(|module| module.globals.iter())
            .chain(
                frames
                    .iter()
                    .flat_map(|frame| frame.locals.iter().chain(frame.stack.iter())),
            )
            .filter_map(Value::gc_ref)
            .chain(
                handlers
                    .iter()
                    .filter_map(|handler| handler.handler.gc_ref()),
            )
            .chain(active_resumes.iter().copied())
            .chain(external_roots.iter().filter_map(Value::gc_ref))
            .chain(extra_root.into_iter().filter_map(Value::gc_ref))
    }
}

const fn is_heap_value(value: &Value) -> bool {
    matches!(
        value,
        Value::String(_)
            | Value::Syntax(_)
            | Value::Seq(_)
            | Value::Data(_)
            | Value::Closure(_)
            | Value::Continuation(_)
            | Value::Module(_)
    )
}
